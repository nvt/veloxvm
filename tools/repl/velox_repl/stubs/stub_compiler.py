"""In-process stand-in for the Racket compiler service.

Used so the driver can be exercised end-to-end before the real Racket
``--repl-server`` mode exists. Speaks the real driver-compiler line
protocol (s-expressions over stdio), but the deltas it produces are in
``stub_delta`` format and only meaningful to ``stub_vm``.

Tries to actually evaluate a small subset of Scheme so the demo feels
alive: integer/real arithmetic with ``+ - * /``, comparisons, basic
``define``s of constants. Anything else is reported as the source
string echoed back. Macros, lambdas, control flow, etc. are out of
scope -- that's the real compiler's job.
"""

from __future__ import annotations

import base64
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

from velox_repl.protocol import (
    SExp,
    Symbol,
    SExpError,
    parse_sexp,
    read_sexp_line,
    write_sexp_line,
)
from velox_repl.render.decode import (
    encode_bool,
    encode_int,
    encode_list,
    encode_none,
    encode_real,
    encode_string,
    encode_symbol,
)
from velox_repl.stubs.stub_delta import StubDelta, encode as encode_delta


@dataclass
class StubSession:
    next_expr_id: int = 1   # 0 reserved as the no-op entry placeholder
    sent_strings: int = 0
    sent_symbols: int = 0
    sent_exprs: int = 0
    bindings: Dict[str, Any] = field(default_factory=dict)


def main() -> int:
    session = StubSession()
    while True:
        try:
            request = read_sexp_line(sys.stdin)
        except SExpError as e:
            _write_error(f"protocol error: {e}", line=0, col=0)
            continue
        if request is None:
            return 0
        try:
            handle_request(session, request)
        except _Shutdown:
            return 0


class _Shutdown(Exception):
    pass


def handle_request(session: StubSession, request: SExp) -> None:
    if not isinstance(request, list) or not request or not isinstance(request[0], Symbol):
        _write_error("malformed request", 0, 0)
        return
    head = str(request[0])
    if head == "shutdown":
        raise _Shutdown
    if head == "reset":
        session.next_expr_id = 1
        session.sent_strings = 0
        session.sent_symbols = 0
        session.sent_exprs = 0
        session.bindings.clear()
        write_sexp_line(sys.stdout, [Symbol("ok")])
        return
    if head == "compile-form":
        if len(request) < 2 or not isinstance(request[1], str):
            _write_error("compile-form expects one string argument", 0, 0)
            return
        compile_form(session, request[1])
        return
    _write_error(f"unknown request: {head}", 0, 0)


def compile_form(session: StubSession, source: str) -> None:
    if not _balanced(source):
        write_sexp_line(sys.stdout, [Symbol("incomplete")])
        return
    if not source.strip():
        write_sexp_line(sys.stdout, [Symbol("incomplete")])
        return
    try:
        ast = parse_sexp(source)
    except SExpError as e:
        _write_error(f"reader: {e}", 1, 1)
        return

    try:
        kind, name, value = _evaluate(session, ast)
        obj_encoding = _encode_value(value)
    except Exception as e:
        # Stub-eval errors are surfaced as compile errors so the driver
        # can keep going. (The real compiler will surface them as
        # runtime errors via the VM, which is more accurate.)
        _write_error(f"stub-eval: {type(e).__name__}: {e}", 1, 1)
        return
    entry_expr_id = session.next_expr_id
    session.next_expr_id += 1
    session.sent_exprs += 1

    delta = StubDelta(
        entry_expr_id=entry_expr_id,
        kind=kind,
        name=name or "",
        obj_encoding=obj_encoding,
    )
    delta_bytes = encode_delta(delta)
    delta_b64 = base64.b64encode(delta_bytes).decode("ascii")

    response: List[SExp] = [
        Symbol("ok"),
        delta_b64,
        entry_expr_id,
        Symbol(kind),
    ]
    if name:
        response.append(Symbol(name))
    write_sexp_line(sys.stdout, response)


# ---- tiny evaluator -------------------------------------------------


def _evaluate(session: StubSession, ast: SExp) -> tuple[str, Optional[str], Any]:
    """Returns (kind, name, value). value is a Python int/float/str/bool/list/None."""
    if isinstance(ast, list) and ast and isinstance(ast[0], Symbol):
        head = str(ast[0])
        if head == "define" and len(ast) >= 3 and isinstance(ast[1], Symbol):
            name = str(ast[1])
            value = _eval_expr(session, ast[2])
            session.bindings[name] = value
            return ("define", name, None)
        if head == "define-syntax":
            return ("stmt", None, None)
    return ("expr", None, _eval_expr(session, ast))


def _eval_expr(session: StubSession, node: SExp) -> Any:
    # Symbol must be checked before str (Symbol subclasses str).
    if isinstance(node, Symbol):
        s = str(node)
        if s == "#t":
            return True
        if s == "#f":
            return False
        if s in session.bindings:
            return session.bindings[s]
        return Symbol(s)  # surface symbol literal back
    if isinstance(node, bool):
        return node
    if isinstance(node, int):
        return node
    if isinstance(node, float):
        return node
    if isinstance(node, str):
        return node
    if isinstance(node, list):
        if not node:
            return []
        head = node[0]
        if isinstance(head, Symbol) and str(head) in _PRIMITIVES:
            args = [_eval_expr(session, a) for a in node[1:]]
            return _PRIMITIVES[str(head)](args)
        # Unknown form: render as a list value
        return [_eval_expr(session, item) for item in node]
    return None


def _add(args: List[Any]) -> Any:
    return sum(args)


def _sub(args: List[Any]) -> Any:
    if not args:
        return 0
    if len(args) == 1:
        return -args[0]
    out = args[0]
    for v in args[1:]:
        out -= v
    return out


def _mul(args: List[Any]) -> Any:
    out: Any = 1
    for v in args:
        out *= v
    return out


def _div(args: List[Any]) -> Any:
    if not args:
        return 1
    out = args[0]
    for v in args[1:]:
        out = out / v
    return out


def _list(args: List[Any]) -> Any:
    return args


_PRIMITIVES = {
    "+": _add,
    "-": _sub,
    "*": _mul,
    "/": _div,
    "list": _list,
}


# ---- value -> obj_encoding -----------------------------------------


def _encode_value(value: Any) -> bytes:
    if value is None:
        return encode_none()
    # Symbol must be checked before str (Symbol subclasses str).
    if isinstance(value, Symbol):
        return encode_symbol(str(value))
    if isinstance(value, bool):
        return encode_bool(value)
    if isinstance(value, int):
        return encode_int(value)
    if isinstance(value, float):
        return encode_real(value)
    if isinstance(value, str):
        return encode_string(value)
    if isinstance(value, list):
        return encode_list([_encode_value(item) for item in value])
    return encode_string(repr(value))


# ---- helpers --------------------------------------------------------


def _balanced(source: str) -> bool:
    depth = 0
    in_string = False
    escape = False
    for ch in source:
        if escape:
            escape = False
            continue
        if in_string:
            if ch == "\\":
                escape = True
            elif ch == '"':
                in_string = False
            continue
        if ch == '"':
            in_string = True
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth < 0:
                return True  # syntactically broken; let the parser fail
        elif ch == ";":
            # comment to end of line
            while ch and ch != "\n":
                ch = ""  # noqa
                break
    return depth == 0 and not in_string


def _write_error(msg: str, line: int, col: int) -> None:
    write_sexp_line(sys.stdout, [Symbol("error"), msg, [line, col]])


if __name__ == "__main__":
    sys.exit(main())
