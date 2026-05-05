"""TTY frontend.

Spawns a compiler subprocess and a VM subprocess (defaulting to the
stage-1 stubs), then runs a prompt_toolkit-driven loop. The frontend is
deliberately thin: all evaluation logic lives in ``ReplSession`` and all
rendering in ``velox_repl.render``.
"""

from __future__ import annotations

import argparse
import os
import shlex
import sys
from pathlib import Path
from typing import List, Optional

from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory, InMemoryHistory

from velox_repl.compiler import CompilerClient
from velox_repl.core import (
    EvalCompileError,
    EvalDesync,
    EvalIncomplete,
    EvalRunError,
    EvalSuccess,
    ReplSession,
)
from velox_repl.render import render_for
from velox_repl.vm import VmClient


PRIMARY_PROMPT = {"scheme": "velox> ", "python": "pyvelox> "}
CONT_PROMPT = {"scheme": "...    ", "python": "...      "}


def main(argv: Optional[List[str]] = None) -> int:
    args = _parse_args(argv)

    compiler_cmd = _resolve_subprocess(args.compiler, "stub_compiler", args.language)
    vm_cmd = _resolve_subprocess(args.vm, "stub_vm", args.language)

    history = _resolve_history(args.history_file)

    compiler = CompilerClient(compiler_cmd, name="compiler")
    vm = VmClient(vm_cmd, name="vm")

    # Track whether the application emitted IO_OUT during the current turn
    # and whether the last byte was a newline. We use this to decide whether
    # to insert a separator before printing the RESULT or the next prompt.
    io_state = {"saw_output": False, "ended_with_newline": True}

    def io_sink(data: bytes) -> None:
        # Application output during a turn. Print straight to stdout; the
        # prompt is not on screen while we're inside ReplSession.evaluate.
        if not data:
            return
        text = data.decode("utf-8", errors="replace")
        sys.stdout.write(text)
        sys.stdout.flush()
        io_state["saw_output"] = True
        io_state["ended_with_newline"] = text.endswith("\n")

    repl = ReplSession(compiler, vm, io_sink=io_sink)
    try:
        repl.start()
    except FileNotFoundError as e:
        print(f"velox-repl: failed to spawn subprocess: {e}", file=sys.stderr)
        return 1
    except OSError as e:
        print(f"velox-repl: spawn error: {e}", file=sys.stderr)
        return 1

    print(f"VeloxVM REPL (language: {args.language})")
    if sys.stdin.isatty():
        print("Type :help for commands, :quit or Ctrl-D to exit.")

    primary = PRIMARY_PROMPT.get(args.language, "velox> ")
    cont = CONT_PROMPT.get(args.language, "...    ")

    read_line = _make_line_reader(history, primary)

    buffer = ""
    prompt = primary
    exit_code = 0

    try:
        while True:
            try:
                line = read_line(prompt)
            except KeyboardInterrupt:
                # Ctrl-C: clear the in-progress buffer
                buffer = ""
                prompt = primary
                continue
            except EOFError:
                # Ctrl-D
                break

            if not buffer and line.strip().startswith(":"):
                if not _handle_meta(repl, line.strip(), args):
                    break
                continue

            buffer += line + "\n"

            io_state["saw_output"] = False
            io_state["ended_with_newline"] = True

            outcome = repl.evaluate(buffer)

            if isinstance(outcome, EvalIncomplete):
                prompt = cont
                continue

            buffer = ""
            prompt = primary

            # If the form printed output but didn't end with a newline,
            # add one so the next prompt or REPL message starts cleanly.
            if io_state["saw_output"] and not io_state["ended_with_newline"]:
                sys.stdout.write("\n")
                sys.stdout.flush()

            if isinstance(outcome, EvalSuccess):
                _print_success(outcome, args.language)
            elif isinstance(outcome, EvalCompileError):
                print(f"; compile error: {outcome.message}")
            elif isinstance(outcome, EvalRunError):
                print(f"; runtime error [{outcome.error_type}]: {outcome.message}")
            elif isinstance(outcome, EvalDesync):
                print(f"; session desynced: {outcome.reason}")
                print("; run :reset to recover (or :quit to exit)")
    finally:
        repl.stop()

    return exit_code


def _make_line_reader(history, primary):
    """Return a callable ``read_line(prompt) -> str`` appropriate for the
    current stdin. Uses prompt_toolkit when stdin is a TTY (rich editing,
    persistent history) and a stdlib readline-equivalent otherwise (so
    ``velox-repl < script.scm`` and ``echo ... | velox-repl`` work)."""
    if sys.stdin.isatty():
        session: PromptSession = PromptSession(history=history)
        return lambda prompt: session.prompt(prompt)

    def read(prompt: str) -> str:
        # Echo the prompt so transcripts are readable; keep silent if the
        # user pipes us into something that already records it.
        if sys.stderr.isatty():
            sys.stderr.write(prompt)
            sys.stderr.flush()
        line = sys.stdin.readline()
        if line == "":
            raise EOFError
        return line.rstrip("\n")

    return read


def _print_success(outcome: EvalSuccess, language: str) -> None:
    if outcome.kind == "define":
        name = outcome.name or "<anonymous>"
        print(f"; defined {name}")
        return
    if outcome.kind == "stmt":
        # Statements yield no value (define-syntax, etc.)
        return
    # For expression forms, suppress unspecified results -- e.g. (display ...)
    # or (set! ...) leave thread->result as VM_TYPE_NONE, and Scheme REPLs
    # conventionally print nothing in that case. The application's IO_OUT
    # already surfaced anything it wanted to show.
    if outcome.obj_encoding[:1] == b"\x01":  # TAG_NONE
        return
    rendered = render_for(language, outcome.obj_encoding)
    print(rendered)


def _handle_meta(repl: ReplSession, command: str, args: argparse.Namespace) -> bool:
    """Returns False if the REPL should exit."""
    parts = command.split(maxsplit=1)
    head = parts[0]
    if head in (":quit", ":q", ":exit"):
        return False
    if head == ":help":
        print(":help              show this help")
        print(":quit | :q | :exit exit the REPL")
        print(":reset             reset compiler and VM state")
        print(":threads           show running thread count")
        print(":sync              show sync invariants (debug)")
        return True
    if head == ":reset":
        try:
            repl.reset()
            print("; session reset")
        except RuntimeError as e:
            print(f"; reset failed: {e}")
        return True
    if head == ":threads":
        print(f"; threads running: {repl.threads_running}")
        return True
    if head == ":sync":
        print(f"; in_sync={repl.in_sync} last_acked_start_id={repl.last_acked_start_id}")
        return True
    print(f"; unknown command: {head} (try :help)")
    return True


def _resolve_subprocess(spec: Optional[str], stub_module: str, language: str) -> List[str]:
    """Translate the --compiler / --vm CLI argument into argv."""
    if spec is None or spec == "stub":
        # Run the stub via the *current* Python interpreter so it picks
        # up the same env (uv-cached venv or the project venv).
        return [sys.executable, "-m", f"velox_repl.stubs.{stub_module}"]
    return shlex.split(spec)


def _resolve_history(history_file: Optional[str]):
    if history_file == "":
        return InMemoryHistory()
    path = history_file or os.path.expanduser("~/.veloxvm_history")
    try:
        Path(path).touch(exist_ok=True)
        return FileHistory(path)
    except OSError:
        return InMemoryHistory()


def _parse_args(argv: Optional[List[str]]) -> argparse.Namespace:
    p = argparse.ArgumentParser(prog="velox-repl", description=__doc__)
    p.add_argument(
        "--language", "-l",
        choices=["scheme", "python"],
        default="scheme",
        help="surface language (default: scheme)",
    )
    p.add_argument(
        "--compiler",
        help='compiler command; "stub" (default) uses the in-tree stub',
    )
    p.add_argument(
        "--vm",
        help='VM command; "stub" (default) uses the in-tree stub',
    )
    p.add_argument(
        "--history-file",
        help='history file path; empty string disables on-disk history',
    )
    return p.parse_args(argv)


if __name__ == "__main__":
    sys.exit(main())
