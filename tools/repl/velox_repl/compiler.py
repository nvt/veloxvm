"""Compiler subprocess client.

The compiler is always co-located with the driver. We spawn it as a
long-running subprocess and exchange one s-expression per request on
stdin/stdout. Both Racket and PyVelox compilers expose the same protocol
shape:

  request                                response
  -------                                --------
  (compile-form "<source>")              (ok "<delta-b64>" <entry-id> <kind>)
                                       | (incomplete)
                                       | (error "<msg>" (<line> <col>))
  (reset)                                (ok)
  (shutdown)                             ; no response, process exits

``<kind>`` is a symbol: ``expr`` (auto-printable expression),
``stmt`` (statement / definition that yielded no value), or
``define`` (top-level binding; we report the bound name to the user).

The delta payload is base64 because s-expressions don't have a great
binary literal in our minimal reader. Unwraps to the raw delta bytes
that the VM's append-loader expects.
"""

from __future__ import annotations

import base64
import io
import subprocess
from dataclasses import dataclass
from typing import List, Optional, Sequence

from velox_repl.protocol import (
    SExp,
    Symbol,
    SExpError,
    read_sexp_line,
    write_sexp_line,
)


class CompilerError(RuntimeError):
    """Compiler failed in a way that does not invalidate session state."""


class CompilerCrash(RuntimeError):
    """Compiler process died or produced unparseable output. The driver
    should consider its sync invariants broken until reset."""


@dataclass
class CompiledForm:
    delta: bytes              # raw bytes, ready to ship as APPLY payload
    entry_expr_id: int        # which expression to RUN
    kind: str                 # "expr" | "stmt" | "define"
    name: Optional[str] = None  # for "define", the bound symbol name


class IncompleteForm:
    """Sentinel: the source isn't a complete top-level form yet. The
    driver should accumulate more user input and retry."""

    __slots__ = ()


INCOMPLETE = IncompleteForm()


@dataclass
class SourceLocation:
    line: int
    col: int


class CompilerClient:
    def __init__(self, argv: Sequence[str], *, name: str = "compiler"):
        self.argv = list(argv)
        self.name = name
        self._proc: Optional[subprocess.Popen[str]] = None

    def start(self) -> None:
        if self._proc is not None:
            return
        self._proc = subprocess.Popen(
            self.argv,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=None,  # let stderr flow through to user TTY for diagnostics
            text=True,
            bufsize=1,
        )

    def stop(self) -> None:
        if self._proc is None:
            return
        try:
            if self._proc.stdin and not self._proc.stdin.closed:
                try:
                    write_sexp_line(self._proc.stdin, [Symbol("shutdown")])
                except (BrokenPipeError, ValueError):
                    pass
                try:
                    self._proc.stdin.close()
                except OSError:
                    pass
            try:
                self._proc.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                self._proc.terminate()
                try:
                    self._proc.wait(timeout=1.0)
                except subprocess.TimeoutExpired:
                    self._proc.kill()
        finally:
            self._proc = None

    def is_alive(self) -> bool:
        return self._proc is not None and self._proc.poll() is None

    def compile_form(self, source: str):
        return self._request([Symbol("compile-form"), source], parse_compile_response)

    def reset(self) -> None:
        result = self._request([Symbol("reset")], parse_reset_response)
        if result is not True:
            raise CompilerError(f"compiler reset returned unexpected: {result!r}")

    def _request(self, request: SExp, parser):
        if not self.is_alive():
            raise CompilerCrash(f"{self.name}: not running")
        assert self._proc is not None and self._proc.stdin and self._proc.stdout
        try:
            write_sexp_line(self._proc.stdin, request)
        except BrokenPipeError as e:
            raise CompilerCrash(f"{self.name}: broken stdin") from e
        try:
            response = read_sexp_line(self._proc.stdout)
        except SExpError as e:
            raise CompilerCrash(f"{self.name}: malformed response: {e}") from e
        if response is None:
            raise CompilerCrash(f"{self.name}: closed stdout unexpectedly")
        return parser(response)


def parse_compile_response(resp: SExp):
    if not isinstance(resp, list) or not resp:
        raise CompilerCrash(f"unexpected compile-form response: {resp!r}")
    head = resp[0]
    if not isinstance(head, Symbol):
        raise CompilerCrash(f"compile-form response has non-symbol head: {resp!r}")
    if head == "incomplete":
        return INCOMPLETE
    if head == "error":
        msg = resp[1] if len(resp) > 1 else ""
        loc = None
        if len(resp) > 2 and isinstance(resp[2], list) and len(resp[2]) >= 2:
            loc = SourceLocation(line=int(resp[2][0]), col=int(resp[2][1]))
        if not isinstance(msg, str):
            msg = str(msg)
        return CompilerError(_format_error(msg, loc))
    if head == "ok":
        # (ok "<delta-b64>" <entry-id> <kind> [<name>])
        if len(resp) < 4:
            raise CompilerCrash(f"compile-form ok missing fields: {resp!r}")
        delta_b64, entry, kind = resp[1], resp[2], resp[3]
        if not isinstance(delta_b64, str):
            raise CompilerCrash(f"compile-form ok: delta is not a string: {resp!r}")
        if not isinstance(entry, int):
            raise CompilerCrash(f"compile-form ok: entry is not an int: {resp!r}")
        if not isinstance(kind, Symbol):
            raise CompilerCrash(f"compile-form ok: kind is not a symbol: {resp!r}")
        try:
            delta = base64.b64decode(delta_b64, validate=True)
        except (ValueError, base64.binascii.Error) as e:  # type: ignore[attr-defined]
            raise CompilerCrash(f"compile-form ok: bad base64: {e}") from e
        name = None
        if len(resp) > 4:
            n = resp[4]
            if isinstance(n, (Symbol, str)):
                name = str(n)
        return CompiledForm(delta=delta, entry_expr_id=entry, kind=str(kind), name=name)
    raise CompilerCrash(f"unknown compile-form response head: {head!r}")


def parse_reset_response(resp: SExp) -> bool:
    if isinstance(resp, list) and resp and isinstance(resp[0], Symbol):
        return resp[0] == "ok"
    return False


def _format_error(msg: str, loc: Optional[SourceLocation]) -> str:
    if loc is None:
        return msg
    return f"{loc.line}:{loc.col}: {msg}"
