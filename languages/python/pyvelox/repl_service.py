# Copyright (c) 2026, RISE Research Institutes of Sweden AB
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived
#    from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.

"""PyVelox compiler service for the VeloxVM REPL.

Long-running compiler that speaks the s-expression line protocol from
``doc/repl-design.md`` on stdin/stdout. Maintains a persistent
``Bytecode`` and ``PythonTranslator`` across forms so strings, symbols,
expressions, function/class definitions, captures, and macro-like
helpers stay visible from one REPL turn to the next. A failed compile
is rolled back so the session stays consistent with the VM's view.

Run as:  python3 -m pyvelox.repl_service
"""

from __future__ import annotations

import ast
import base64
import codeop
import struct
import sys
import traceback
from typing import Any, List, Optional, Tuple

from .bytecode import Bytecode
from .errors import PyveloxCompileError
from .translator import PythonTranslator


# ---------------------------------------------------------------------------
# Wire-format constants. Match include/vm-bytecode.h delta tags and the Racket
# emitter in languages/scheme-racket/repl-server.rkt.
# ---------------------------------------------------------------------------

DELTA_MAGIC = b"\x5e\xb6"
DELTA_VERSION = 1

TAG_STRINGS = 0x01
TAG_SYMBOLS = 0x02
TAG_EXPRS = 0x03
TAG_CAPTURES = 0x04
TAG_ENTRY_EXPR = 0x05
TAG_END = 0xFF


# ---------------------------------------------------------------------------
# Session
# ---------------------------------------------------------------------------


class _Session:
    """Persistent compiler state across REPL forms.

    The same ``PythonTranslator`` and ``Bytecode`` are reused for every
    ``compile-form`` request so module-scope definitions, helper-emit
    bookkeeping, and signature/default-arg tables carry over. Watermarks
    track how many items in each table have been delivered to the VM in
    earlier deltas; the next delta covers indices ``>= watermark``.
    """

    def __init__(self) -> None:
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)
        self.sent_strings = 0
        self.sent_symbols = 0
        self.sent_exprs = 0
        self.sent_captures = 0


# ---------------------------------------------------------------------------
# Snapshot / rollback
# ---------------------------------------------------------------------------


class _Snapshot:
    """Frozen copy of mutable session state, for rollback when a form
    fails to compile. Restoring keeps the session in lockstep with the
    VM (the VM never saw the failed form's strings/symbols/exprs)."""

    __slots__ = (
        "strings", "symbols", "string_map", "symbol_map",
        "expressions", "captures", "loop_counter", "var_counter",
        "preamble", "emitted_helpers",
        "renamed_vars", "default_nodes", "default_bytes",
        "vararg_funcs", "defined_classes",
        "scope_stack", "boxed_stack",
        "loop_depth", "exception_handler_vars", "enclosing_class_stack",
    )

    def __init__(self, session: _Session) -> None:
        bc = session.bc
        st = bc.symbol_table
        t = session.translator
        self.strings = list(st.strings)
        self.symbols = list(st.symbols)
        self.string_map = dict(st._string_map)
        self.symbol_map = dict(st._symbol_map)
        self.expressions = list(bc.expressions)
        self.captures = {k: list(v) for k, v in bc.captures.items()}
        self.loop_counter = bc.loop_counter
        self.var_counter = bc.var_counter
        self.preamble = bytearray(t._preamble)
        self.emitted_helpers = set(t._emitted_helpers)
        self.renamed_vars = dict(t.renamed_vars)
        self.default_nodes = {k: list(v) for k, v in t._default_nodes.items()}
        self.default_bytes = {k: list(v) for k, v in t._default_bytes.items()}
        self.vararg_funcs = dict(t._vararg_funcs)
        self.defined_classes = set(t._defined_classes)
        self.scope_stack = [set(s) for s in t.scope_stack]
        self.boxed_stack = [set(s) for s in t.boxed_stack]
        self.loop_depth = t._loop_depth
        self.exception_handler_vars = list(t._exception_handler_vars)
        self.enclosing_class_stack = list(t._enclosing_class_stack)

    def restore_to(self, session: _Session) -> None:
        bc = session.bc
        st = bc.symbol_table
        t = session.translator
        st.strings[:] = self.strings
        st.symbols[:] = self.symbols
        st._string_map.clear()
        st._string_map.update(self.string_map)
        st._symbol_map.clear()
        st._symbol_map.update(self.symbol_map)
        bc.expressions[:] = self.expressions
        bc.captures.clear()
        bc.captures.update(self.captures)
        bc.loop_counter = self.loop_counter
        bc.var_counter = self.var_counter
        t._preamble = bytearray(self.preamble)
        t._emitted_helpers = set(self.emitted_helpers)
        t.renamed_vars = dict(self.renamed_vars)
        t._default_nodes = {k: list(v) for k, v in self.default_nodes.items()}
        t._default_bytes = {k: list(v) for k, v in self.default_bytes.items()}
        t._vararg_funcs = dict(self.vararg_funcs)
        t._defined_classes = set(self.defined_classes)
        t.scope_stack = [set(s) for s in self.scope_stack]
        t.boxed_stack = [set(s) for s in self.boxed_stack]
        t._loop_depth = self.loop_depth
        t._exception_handler_vars = list(self.exception_handler_vars)
        t._enclosing_class_stack = list(self.enclosing_class_stack)


# ---------------------------------------------------------------------------
# Delta wire-format emission
# ---------------------------------------------------------------------------


def _build_delta(session: _Session, entry_expr_id: int) -> bytes:
    """Encode everything new in this session since the last successful
    APPLY into a single delta blob, terminated with TAG_ENTRY_EXPR + END."""
    bc = session.bc
    st = bc.symbol_table
    out = bytearray()
    out += DELTA_MAGIC
    out.append(DELTA_VERSION)

    new_strings = st.strings[session.sent_strings:]
    if new_strings:
        _write_section(out, TAG_STRINGS,
                       _encode_string_section(session.sent_strings,
                                              new_strings))

    new_symbols = st.symbols[session.sent_symbols:]
    if new_symbols:
        _write_section(out, TAG_SYMBOLS,
                       _encode_string_section(session.sent_symbols,
                                              new_symbols))

    new_exprs = bc.expressions[session.sent_exprs:]
    if new_exprs:
        _write_section(out, TAG_EXPRS,
                       _encode_expr_section(session.sent_exprs, new_exprs))

    # bc.captures is a dict keyed by expr_id; insertion order is preserved
    # in Python 3.7+, so slicing items() at the watermark gives entries
    # added since the last APPLY in registration order.
    new_captures = list(bc.captures.items())[session.sent_captures:]
    if new_captures:
        _write_section(out, TAG_CAPTURES,
                       _encode_captures_section(new_captures))

    _write_section(out, TAG_ENTRY_EXPR, struct.pack(">H", entry_expr_id))

    out.append(TAG_END)
    out += struct.pack(">H", 0)
    return bytes(out)


def _write_section(buf: bytearray, tag: int, payload: bytes) -> None:
    buf.append(tag)
    buf += struct.pack(">H", len(payload))
    buf += payload


def _encode_string_section(start_id: int, items: List[str]) -> bytes:
    out = bytearray()
    out += struct.pack(">HH", start_id, len(items))
    for s in items:
        b = s.encode("utf-8")
        out += struct.pack(">H", len(b))
        out += b
    return bytes(out)


def _encode_expr_section(start_id: int, items: List[bytes]) -> bytes:
    out = bytearray()
    out += struct.pack(">HH", start_id, len(items))
    for e in items:
        out += struct.pack(">H", len(e))
        out += bytes(e)
    return bytes(out)


def _encode_captures_section(entries: List[Tuple[int, List[int]]]) -> bytes:
    out = bytearray()
    out += struct.pack(">H", len(entries))
    for expr_id, sym_ids in entries:
        entry_len = 2 + 2 * len(sym_ids)
        out += struct.pack(">H", entry_len)
        out += struct.pack(">H", expr_id)
        for sid in sym_ids:
            out += struct.pack(">H", sid)
    return bytes(out)


# ---------------------------------------------------------------------------
# AST helpers
# ---------------------------------------------------------------------------


def _is_incomplete_source(source: str) -> Optional[bool]:
    """Distinguish "needs more input" from "actual syntax error".

    Builds on ``codeop.compile_command`` (CPython's standard
    incomplete-detector) but also keeps indented blocks open until the
    user terminates them with a blank line. Without this, codeop calls
    ``def f():\\n    return 1\\n`` complete the moment the body parses
    and the next typed line is rejected as "unexpected indent" instead
    of being treated as another body statement. CPython's interactive
    REPL has the same blank-line convention.

    Returns True when more input is needed, False when the source is
    ready to compile, and re-raises ``SyntaxError`` to surface a real
    syntax problem the caller can format with location info.
    """
    try:
        code = codeop.compile_command(source, "<repl>", "exec")
    except SyntaxError:
        # codeop already decided this isn't a continuation candidate
        # (e.g. unmatched bracket on the same line). Re-raise so the
        # caller can package it as an `(error ...)` response.
        raise
    if code is None:
        return True
    # When the last non-empty line is indented, the user is mid-block;
    # honor codeop's verdict only once they've hit Enter on a blank
    # line (signalled by a trailing "\\n\\n").
    last_nonempty = ""
    for line in reversed(source.splitlines()):
        if line.strip():
            last_nonempty = line
            break
    if last_nonempty[:1] in (" ", "\t") and not source.endswith("\n\n"):
        return True
    return False


def _detect_kind(module: ast.Module) -> Tuple[str, Optional[str]]:
    """Classify a top-level form into (kind, name).

    ``expr``    -- yields a value the REPL should auto-print.
    ``define``  -- binds a name; ``name`` reports the bound symbol.
    ``stmt``    -- statement that yields no value; nothing to print.

    Multi-statement input is reported as ``stmt`` (no single binding to
    name), matching how the Scheme side wraps multiple forms in a
    ``begin``.
    """
    body = module.body
    if not body or len(body) > 1:
        return "stmt", None
    stmt = body[0]
    if isinstance(stmt, ast.Expr):
        return "expr", None
    if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
        return "define", stmt.name
    if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 \
            and isinstance(stmt.targets[0], ast.Name):
        return "define", stmt.targets[0].id
    if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name) \
            and stmt.value is not None:
        return "define", stmt.target.id
    return "stmt", None


def _syntax_error_loc(exc: SyntaxError) -> Tuple[int, int]:
    line = exc.lineno or 0
    # Python's offset is 1-based; report 0 when missing rather than
    # subtracting 1 and hitting -1 for `offset is None`.
    col = exc.offset or 0
    return line, col


# ---------------------------------------------------------------------------
# S-expression I/O
# ---------------------------------------------------------------------------


class _Sym:
    """Marker for a bare s-expr symbol (no quotes) when emitting."""
    __slots__ = ("name",)

    def __init__(self, name: str) -> None:
        self.name = name


def _emit_sexp(value: Any) -> str:
    if isinstance(value, _Sym):
        return value.name
    if isinstance(value, bool):
        # Must precede the int branch -- bool is a subclass of int.
        return "#t" if value else "#f"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, str):
        return _emit_string(value)
    if isinstance(value, (list, tuple)):
        return "(" + " ".join(_emit_sexp(v) for v in value) + ")"
    raise TypeError(f"cannot emit s-expr: {type(value).__name__}")


def _emit_string(s: str) -> str:
    out = ['"']
    for ch in s:
        if ch == "\\":
            out.append("\\\\")
        elif ch == '"':
            out.append('\\"')
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\r":
            out.append("\\r")
        elif ch == "\t":
            out.append("\\t")
        elif ord(ch) < 0x20 or ord(ch) == 0x7F:
            out.append(f"\\x{ord(ch):02x};")
        else:
            out.append(ch)
    out.append('"')
    return "".join(out)


def _write_response(value: Any) -> None:
    sys.stdout.write(_emit_sexp(value) + "\n")
    sys.stdout.flush()


# ---------------------------------------------------------------------------
# Request handlers
# ---------------------------------------------------------------------------


def _do_compile_form(session: _Session, source: str) -> None:
    # Incomplete-vs-syntax-error gate first. codeop's heuristic is the
    # same one CPython's interactive shell uses, so multi-line input
    # behaves the way users expect.
    try:
        incomplete = _is_incomplete_source(source)
    except SyntaxError as exc:
        line, col = _syntax_error_loc(exc)
        msg = exc.msg or "syntax error"
        _write_response([_Sym("error"), msg, [line, col]])
        return
    if incomplete:
        _write_response([_Sym("incomplete")])
        return

    try:
        module = ast.parse(source)
    except SyntaxError as exc:
        line, col = _syntax_error_loc(exc)
        msg = exc.msg or "syntax error"
        _write_response([_Sym("error"), msg, [line, col]])
        return

    if not module.body:
        # Whitespace-only / comment-only input. Treat as incomplete so
        # the driver waits for more rather than emitting an empty entry.
        _write_response([_Sym("incomplete")])
        return

    snap = _Snapshot(session)
    try:
        # Each form starts with an empty preamble; helpers already emitted
        # in earlier forms are guarded by _emitted_helpers.
        session.translator._preamble = bytearray()
        session.translator._source_lines = source.splitlines()

        form_bytes = session.translator.translate_module(module)
        entry_id = session.bc.add_expression(form_bytes)
    except PyveloxCompileError as exc:
        snap.restore_to(session)
        line = exc.lineno or 0
        # Convert pyvelox's 0-based AST col_offset to the 1-based column
        # the protocol uses (matches Racket's srcloc-column).
        col = exc.col_offset + 1 if exc.col_offset is not None else 0
        _write_response([_Sym("error"), exc.raw_message, [line, col]])
        return
    except SyntaxError as exc:
        snap.restore_to(session)
        line, col = _syntax_error_loc(exc)
        _write_response([_Sym("error"), exc.msg or "syntax error",
                         [line, col]])
        return
    except Exception as exc:  # noqa: BLE001 -- compiler bug, not user error
        snap.restore_to(session)
        # Best effort: include the traceback summary so a compiler bug
        # doesn't silently stall the session.
        tb = traceback.format_exception_only(type(exc), exc)
        msg = "internal compiler error: " + "".join(tb).strip()
        _write_response([_Sym("error"), msg, [0, 0]])
        return

    kind, name = _detect_kind(module)
    delta = _build_delta(session, entry_id)
    delta_b64 = base64.b64encode(delta).decode("ascii")

    response = [_Sym("ok"), delta_b64, entry_id, _Sym(kind)]
    if name is not None:
        response.append(name)

    session.sent_strings = len(session.bc.symbol_table.strings)
    session.sent_symbols = len(session.bc.symbol_table.symbols)
    session.sent_exprs = len(session.bc.expressions)
    session.sent_captures = len(session.bc.captures)

    _write_response(response)


def _do_reset(session: _Session) -> None:
    session.bc = Bytecode()
    session.translator = PythonTranslator(session.bc)
    session.sent_strings = 0
    session.sent_symbols = 0
    session.sent_exprs = 0
    session.sent_captures = 0
    _write_response([_Sym("ok")])


# ---------------------------------------------------------------------------
# Request reader
# ---------------------------------------------------------------------------


def _read_request() -> Optional[List[Any]]:
    """Read one s-expression from stdin and return it as a Python list,
    or None on EOF. Reuses the driver's parser so the two sides agree on
    syntax (escapes, whitespace, etc.)."""
    # The driver and service share the same protocol shape. Importing
    # the driver's parser would create a cross-package dependency, so we
    # vendor a minimal reader here that handles ASCII s-expressions,
    # quoted strings (with the same escapes the driver writes), and
    # nested lists. That's all the protocol uses.
    line_parts: List[str] = []
    depth = 0
    in_string = False
    escape = False
    while True:
        ch = sys.stdin.read(1)
        if ch == "":
            if not line_parts:
                return None
            break
        line_parts.append(ch)
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
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                # End of a top-level list. Consume any trailing newline.
                # Don't read more after; let the next request start a fresh
                # line.
                break
        elif ch == "\n" and depth == 0 and not in_string:
            # Blank line / stray whitespace at top level; reset.
            line_parts = []
            continue

    text = "".join(line_parts).strip()
    if not text:
        return None
    return _parse_sexp(text)


def _parse_sexp(text: str) -> Any:
    parser = _Parser(text)
    value = parser.read()
    parser.skip_ws()
    if not parser.eof():
        raise ValueError("trailing input after request")
    return value


class _Parser:
    def __init__(self, text: str) -> None:
        self.text = text
        self.pos = 0

    def eof(self) -> bool:
        return self.pos >= len(self.text)

    def peek(self) -> str:
        return self.text[self.pos] if not self.eof() else ""

    def advance(self) -> str:
        ch = self.text[self.pos]
        self.pos += 1
        return ch

    def skip_ws(self) -> None:
        while not self.eof() and self.text[self.pos] in " \t\r\n":
            self.pos += 1

    def read(self) -> Any:
        self.skip_ws()
        if self.eof():
            raise ValueError("unexpected end of input")
        ch = self.peek()
        if ch == "(":
            return self._read_list()
        if ch == '"':
            return self._read_string()
        if ch == "#":
            return self._read_hash()
        return self._read_atom()

    def _read_list(self) -> List[Any]:
        assert self.advance() == "("
        items: List[Any] = []
        while True:
            self.skip_ws()
            if self.eof():
                raise ValueError("unterminated list")
            if self.peek() == ")":
                self.advance()
                return items
            items.append(self.read())

    def _read_string(self) -> str:
        assert self.advance() == '"'
        out: List[str] = []
        while True:
            if self.eof():
                raise ValueError("unterminated string")
            ch = self.advance()
            if ch == '"':
                return "".join(out)
            if ch == "\\":
                if self.eof():
                    raise ValueError("trailing backslash")
                esc = self.advance()
                if esc == "n":
                    out.append("\n")
                elif esc == "r":
                    out.append("\r")
                elif esc == "t":
                    out.append("\t")
                elif esc == "\\":
                    out.append("\\")
                elif esc == '"':
                    out.append('"')
                elif esc == "x":
                    hex_chars: List[str] = []
                    while not self.eof() and self.peek() != ";":
                        hex_chars.append(self.advance())
                    if self.eof():
                        raise ValueError("unterminated \\x escape")
                    self.advance()  # consume ';'
                    out.append(chr(int("".join(hex_chars), 16)))
                else:
                    raise ValueError(f"unknown escape \\{esc}")
            else:
                out.append(ch)

    def _read_hash(self) -> Any:
        assert self.advance() == "#"
        if self.eof():
            raise ValueError("trailing '#'")
        ch = self.advance()
        if ch == "t":
            return True
        if ch == "f":
            return False
        raise ValueError(f"unsupported hash literal: #{ch}")

    def _read_atom(self) -> Any:
        start = self.pos
        while not self.eof() and self.text[self.pos] not in " \t\r\n()":
            self.pos += 1
        token = self.text[start:self.pos]
        # Try integer first.
        try:
            return int(token)
        except ValueError:
            return _Sym(token)


# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------


def main() -> int:
    session = _Session()
    while True:
        try:
            request = _read_request()
        except ValueError as exc:
            _write_response([_Sym("error"), f"malformed request: {exc}",
                             [0, 0]])
            continue
        if request is None:
            return 0
        if not isinstance(request, list) or not request \
                or not isinstance(request[0], _Sym):
            _write_response([_Sym("error"), "malformed request", [0, 0]])
            continue
        head = request[0].name
        if head == "shutdown":
            return 0
        if head == "reset":
            _do_reset(session)
            continue
        if head == "compile-form":
            if len(request) != 2 or not isinstance(request[1], str):
                _write_response([_Sym("error"),
                                 "compile-form expects one string argument",
                                 [0, 0]])
                continue
            _do_compile_form(session, request[1])
            continue
        _write_response([_Sym("error"), f"unknown request: {head}",
                         [0, 0]])


if __name__ == "__main__":
    sys.exit(main())
