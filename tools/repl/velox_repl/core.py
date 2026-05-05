"""REPL session: orchestrates the compiler and the VM.

The driver-side responsibilities that don't belong in either peer live
here:

* compile-then-apply-then-run sequencing
* sync invariants between compiler and VM (out-of-sync after a crash)
* coordinated reset across both peers
* surfacing IO_OUT bytes from the VM to a caller-supplied sink

Outcomes are returned as discriminated dataclasses so the TTY frontend
(or any other frontend) can pattern-match on them and decide how to
render. This module deliberately doesn't print anything.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Optional, Union

from velox_repl.compiler import (
    INCOMPLETE,
    CompilerClient,
    CompilerCrash,
    CompilerError,
    CompiledForm,
)
from velox_repl.vm import (
    RunFailure,
    RunSuccess,
    VmClient,
    VmCrash,
    VmError,
)


@dataclass
class EvalIncomplete:
    """The source isn't a complete top-level form yet."""


@dataclass
class EvalSuccess:
    obj_encoding: bytes      # raw vm_obj_t encoding; renderer formats it
    kind: str                # "expr" | "stmt" | "define"
    name: Optional[str]      # for "define"
    threads_running: int = 0


@dataclass
class EvalCompileError:
    message: str


@dataclass
class EvalRunError:
    error_type: int
    message: str
    threads_running: int = 0


@dataclass
class EvalDesync:
    """The compiler or VM crashed/became unreachable. Driver should reset."""

    reason: str


EvalOutcome = Union[
    EvalIncomplete,
    EvalSuccess,
    EvalCompileError,
    EvalRunError,
    EvalDesync,
]


IoSink = Callable[[bytes], None]


class ReplSession:
    def __init__(
        self,
        compiler: CompilerClient,
        vm: VmClient,
        *,
        io_sink: Optional[IoSink] = None,
    ):
        self.compiler = compiler
        self.vm = vm
        self.io_sink = io_sink or (lambda _b: None)

        # sync invariants
        self.last_acked_start_id: int = 0
        self.in_sync: bool = True

        # transient per-turn state (None when no turn in flight)
        self.current_kind: Optional[str] = None
        self.threads_running: int = 0

    # ---- lifecycle ---------------------------------------------------

    def start(self) -> None:
        self.compiler.start()
        self.vm.start()

    def stop(self) -> None:
        # Best-effort shutdown of both peers.
        try:
            self.vm.stop()
        finally:
            self.compiler.stop()

    # ---- main entry --------------------------------------------------

    def evaluate(self, source: str) -> EvalOutcome:
        if not self.in_sync:
            return EvalDesync(reason="session out of sync; run :reset")

        # 1. Compile
        try:
            compiled = self.compiler.compile_form(source)
        except CompilerCrash as e:
            self.in_sync = False
            return EvalDesync(reason=str(e))
        if compiled is INCOMPLETE:
            return EvalIncomplete()
        if isinstance(compiled, CompilerError):
            # Recoverable: session state in both peers is unchanged because
            # a well-behaved compiler must not mutate its state on failure.
            return EvalCompileError(message=str(compiled))
        assert isinstance(compiled, CompiledForm)

        # 2. Apply delta to VM
        try:
            self.last_acked_start_id = self.vm.apply_delta(compiled.delta)
        except VmCrash as e:
            self.in_sync = False
            return EvalDesync(reason=str(e))
        except VmError as e:
            # APPLY-time errors are unusual (delta version mismatch, OOM in
            # the VM's table grow). Treat as desync to be safe.
            self.in_sync = False
            return EvalDesync(reason=f"VM rejected delta: {e}")

        # 3. Run entry expression
        self.current_kind = compiled.kind
        try:
            outcome = self.vm.run(
                compiled.entry_expr_id,
                on_io=self._on_io,
                on_status=self._on_status,
            )
        except VmCrash as e:
            self.in_sync = False
            return EvalDesync(reason=str(e))
        finally:
            self.current_kind = None

        if isinstance(outcome, RunSuccess):
            return EvalSuccess(
                obj_encoding=outcome.obj_encoding,
                kind=compiled.kind,
                name=compiled.name,
                threads_running=outcome.threads_running,
            )
        assert isinstance(outcome, RunFailure)
        return EvalRunError(
            error_type=outcome.error_type,
            message=outcome.message,
            threads_running=outcome.threads_running,
        )

    # ---- session-level operations ------------------------------------

    def reset(self) -> None:
        """Coordinated reset: compiler first, then VM. Either-or-both
        being already out of sync is fine -- this is how we recover."""
        compiler_err: Optional[Exception] = None
        vm_err: Optional[Exception] = None
        try:
            self.compiler.reset()
        except (CompilerCrash, CompilerError) as e:
            compiler_err = e
        try:
            self.vm.reset()
        except (VmCrash, VmError) as e:
            vm_err = e
        self.last_acked_start_id = 0
        self.threads_running = 0
        if compiler_err is None and vm_err is None:
            self.in_sync = True
            return
        # If either side failed, session remains out of sync. The driver
        # may decide to restart subprocesses entirely.
        self.in_sync = False
        msg = []
        if compiler_err is not None:
            msg.append(f"compiler: {compiler_err}")
        if vm_err is not None:
            msg.append(f"vm: {vm_err}")
        raise RuntimeError("; ".join(msg))

    # ---- callbacks ---------------------------------------------------

    def _on_io(self, data: bytes) -> None:
        self.io_sink(data)

    def _on_status(self, threads_running: int) -> None:
        self.threads_running = threads_running
