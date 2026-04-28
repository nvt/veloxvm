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

"""
Python AST to VeloxVM Bytecode Translator

This module translates Python AST nodes to VeloxVM bytecode using
the encoding functions. It implements a visitor pattern to handle
different Python constructs.
"""

import ast
import contextlib
import functools
import sys
from typing import Callable, Dict, List, Optional, Set
from ..bytecode import Bytecode
from ..encoder import (
    encode_integer, encode_boolean, encode_string, encode_symbol,
    encode_form_ref, encode_form_lambda,
    create_inline_call, create_bind_form
)
from ..errors import PyveloxCompileError
from ..primitives import get_primitive_id
from .builtins import _BuiltinHandlers
from .methods import _MethodHandlers


def _located_dispatch(method):
    """Decorate a translator method that takes an AST node so that any
    exception raised below it gets re-raised as a PyveloxCompileError
    with the node's source location attached. Inner frames see the
    most-specific node; outer frames leave an already-located error
    alone, so the deepest in-flight node wins."""
    @functools.wraps(method)
    def wrapper(self, node, *args, **kwargs):
        try:
            return method(self, node, *args, **kwargs)
        except PyveloxCompileError:
            raise
        except Exception as exc:
            raise PyveloxCompileError(
                str(exc),
                lineno=getattr(node, 'lineno', None),
                col_offset=getattr(node, 'col_offset', None),
                source_lines=self._source_lines,
            ) from exc
    return wrapper




class PythonTranslator(_BuiltinHandlers, _MethodHandlers):
    """Translate Python AST nodes to VeloxVM bytecode.

    The class itself owns dispatch (`translate_stmt`, `translate_expr`),
    scope and closure analysis (`_analyze_body`, `collect_assigned_vars`),
    expression-table helpers (`_hoist`, `_evaluate_once`,
    `_emit_name_load`, `_make_box_init_bytes`), the basic operators
    and control-flow handlers, and the function/lambda/call
    machinery. Per-built-in (`print`, `range`, `int`, ...) and
    per-receiver method (`.append`, `.keys`, `.upper`, ...) handlers
    live in the `_BuiltinHandlers` and `_MethodHandlers` mixins.
    """

    # Sentinel symbols raised by `break`/`continue` and caught by the
    # surrounding loop. They are module-level (not per-loop) because
    # each loop installs its own break/continue guard, so the nearest
    # enclosing one always catches first. User except handlers
    # re-raise these so loop control isn't accidentally swallowed by
    # `except:` inside a loop body.
    BREAK_SENTINEL = '__pyvelox_break__'
    CONTINUE_SENTINEL = '__pyvelox_continue__'

    def __init__(self, bc: Bytecode, source_lines: Optional[List[str]] = None):
        self.bc = bc
        self.scope_stack: List[Set[str]] = [set()]  # Stack of scopes, outermost first
        # Per-scope set of boxed names. Parallel to scope_stack; entry i lists
        # the names in scope_stack[i] that live in heap-allocated boxes
        # (because they are both captured by an inner function AND mutated).
        self.boxed_stack: List[Set[str]] = [set()]
        self.renamed_vars: Dict[str, str] = {}  # Map original names to safe names
        # Used by PyveloxCompileError to render source-line snippets.
        self._source_lines: Optional[List[str]] = source_lines
        # Depth of the currently-being-translated loop nest. `break` and
        # `continue` are only legal when this is positive.
        self._loop_depth: int = 0
        # Module-level bytecode emitted before any user statement —
        # used to inject helpers (e.g. `_pyvelox_range`) on demand. The
        # `_emitted_helpers` set guards against emitting the same helper
        # twice when the construct appears more than once in a program.
        self._preamble: bytearray = bytearray()
        self._emitted_helpers: Set[str] = set()
        # Defaults table populated by a pre-pass over the AST. Maps a
        # function's safe name to a list whose i-th entry is either
        # None (the i-th positional argument is required) or the AST
        # node for its literal default value. translate_call pads
        # missing trailing arguments at the call site by encoding the
        # node lazily — see `_get_function_defaults`. Storing AST
        # nodes instead of pre-encoded bytes keeps the pre-pass from
        # writing into bc's string table; that's the main pass's
        # responsibility.
        self._default_nodes: Dict[str, List[Optional[ast.expr]]] = {}
        # Caches the encoded form of each default the first time a
        # call site references it; keeps subsequent call sites from
        # re-emitting the same constant.
        self._default_bytes: Dict[str, List[Optional[bytes]]] = {}

    def is_vm_primitive(self, name: str) -> bool:
        """Check if a name conflicts with a VM primitive."""
        return get_primitive_id(name) is not None

    def get_safe_name(self, name: str) -> str:
        """
        Get a safe variable name, renaming if it conflicts with a VM primitive.

        If the name conflicts with a VM primitive, it will be renamed with the
        prefix 'py_' and a warning will be emitted.

        Args:
            name: Original Python variable name

        Returns:
            Safe name to use in bytecode (either original or renamed)
        """
        # Check if already renamed
        if name in self.renamed_vars:
            return self.renamed_vars[name]

        # Check if it conflicts with a VM primitive
        if self.is_vm_primitive(name):
            safe_name = f'py_{name}'
            self.renamed_vars[name] = safe_name
            print(f"Warning: Variable '{name}' conflicts with VM primitive, renamed to '{safe_name}'",
                  file=sys.stderr)
            return safe_name

        return name

    def encodes_as_single_token(self, expr: ast.expr) -> bool:
        """True if `expr` lowers to a single bytecode token (an atom
        or a lambda form-ref) rather than an inline call.

        Such expressions can be embedded directly as arguments to
        other inline forms; everything else has to be hoisted into
        the expression table and referenced by form-ref to avoid
        nested inline forms (which the byte loader can't parse).

        Currently the qualifying shapes are `Constant` (atom),
        `Name` (atom: symbol), and `Lambda` (lambda form-ref).
        """
        return isinstance(expr, (ast.Constant, ast.Name, ast.Lambda))

    def translate_expr_with_ref(self, expr: ast.expr) -> bytes:
        """
        Translate an expression and store it separately if it's complex.

        Returns either:
        - Inlined bytecode for simple expressions
        - Form reference for complex expressions
        """
        if self.encodes_as_single_token(expr):
            # Single-token expression — safe to embed inline.
            return self.translate_expr(expr)
        else:
            # Complex expression - store separately and return form ref
            expr_bytes = self.translate_expr(expr)
            expr_id = self.bc.add_expression(expr_bytes)
            return encode_form_ref(expr_id)

    def translate_module(self, module: ast.Module) -> bytes:
        """
        Translate a Python module to bytecode.

        This follows the CL-style compilation:
        1. Pre-allocate expression 0
        2. Compile each top-level statement
        3. Concatenate all bytes into expression 0 (DO NOT wrap in begin)

        Args:
            module: Python AST module

        Returns:
            Compiled bytecode (to be stored in expression 0)
        """
        # Pre-pass: validate every function/lambda signature and
        # record positional defaults. Doing it before any per-statement
        # translation means call sites in earlier code can resolve
        # defaults of functions defined later, matching Python's late
        # binding for direct calls.
        self._collect_function_defaults(module)

        statements = bytearray()

        for stmt in module.body:
            bytecode = self.translate_stmt(stmt)
            statements.extend(bytecode)

        # Helpers (filled in by translate_*) must run before user code,
        # since user code may call them. They're emitted at translation
        # time and concatenated here as the program prologue.
        return bytes(self._preamble) + bytes(statements)

    def _collect_function_defaults(self, node: ast.AST) -> None:
        """Walk `node` recursively, validating every FunctionDef and
        Lambda signature and recording literal positional defaults.
        Errors are re-raised as PyveloxCompileError with the offending
        function's source location."""
        if isinstance(node, (ast.FunctionDef, ast.Lambda,
                             ast.AsyncFunctionDef)):
            try:
                self._validate_function_signature(
                    node, isinstance(node, ast.Lambda))
            except NotImplementedError as exc:
                raise PyveloxCompileError(
                    str(exc),
                    lineno=getattr(node, 'lineno', None),
                    col_offset=getattr(node, 'col_offset', None),
                    source_lines=self._source_lines,
                ) from exc

        # Recurse. ast.iter_child_nodes covers function body, lambda
        # body, expression children — everything we need.
        for child in ast.iter_child_nodes(node):
            self._collect_function_defaults(child)

    def _validate_function_signature(self, node, is_lambda: bool) -> None:
        """Refuse vararg/kwarg/kw-only/positional-only forms; for
        FunctionDefs also encode and record any literal defaults.
        Raises NotImplementedError on anything we don't handle — the
        caller wraps it with a source location."""
        args = node.args
        if args.vararg is not None:
            raise NotImplementedError(
                "*args is not yet supported in pyvelox")
        if args.kwarg is not None:
            raise NotImplementedError(
                "**kwargs is not yet supported in pyvelox")
        if args.kwonlyargs:
            raise NotImplementedError(
                "Keyword-only arguments (after `*`) are not yet "
                "supported in pyvelox")
        if getattr(args, 'posonlyargs', None):
            raise NotImplementedError(
                "Positional-only arguments (the `/` separator) are not "
                "yet supported in pyvelox")

        if not args.defaults:
            return

        if is_lambda:
            raise NotImplementedError(
                "Default arguments on `lambda` are not supported. Use "
                "a `def` for the same signature.")

        # Validate each default and record the AST node. Encoding
        # happens lazily at the first call site that needs to pad
        # this default in (see `_get_function_defaults`), keeping
        # the pre-pass from mutating bc's string/symbol tables.
        # Defaults align right: the last len(args.defaults) positional
        # args are the ones with defaults.
        n_args = len(args.args)
        n_defaults = len(args.defaults)
        nodes: List[Optional[ast.expr]] = []
        for i in range(n_args):
            if i < n_args - n_defaults:
                nodes.append(None)
                continue
            default_node = args.defaults[i - (n_args - n_defaults)]
            self._validate_literal_default(default_node)
            nodes.append(default_node)

        self._default_nodes[self.get_safe_name(node.name)] = nodes

    def _validate_literal_default(self, default_node: ast.expr) -> None:
        """Default values must be literal constants — anything else
        would either re-evaluate side effects at every call site or
        introduce mutable-default footguns."""
        if not isinstance(default_node, ast.Constant):
            raise NotImplementedError(
                f"Default argument values must be literal constants "
                f"(got {type(default_node).__name__}). Defaults are "
                f"materialised at each call site, so non-literal "
                f"defaults would re-evaluate their expression every "
                f"time.")
        value = default_node.value
        if not isinstance(value, (int, str, bool)) and value is not None:
            raise NotImplementedError(
                f"Default argument values must be int, bool, str, or "
                f"None (got {type(value).__name__})")

    def _get_function_defaults(self, safe_name: str
                               ) -> Optional[List[Optional[bytes]]]:
        """Return the encoded default-bytes list for `safe_name`, or
        None if the function has no recorded defaults. Encoded form
        is cached after first emission so subsequent call sites reuse
        the same bytes (and avoid re-adding string-table entries)."""
        if safe_name not in self._default_nodes:
            return None
        cached = self._default_bytes.get(safe_name)
        if cached is None:
            cached = [self.translate_constant(node) if node is not None else None
                      for node in self._default_nodes[safe_name]]
            self._default_bytes[safe_name] = cached
        return cached

    @_located_dispatch
    def translate_stmt(self, stmt: ast.stmt) -> bytes:
        """Translate a statement node."""
        if isinstance(stmt, ast.Expr):
            # Expression statement (e.g., function call)
            return self.translate_expr(stmt.value)
        elif isinstance(stmt, ast.Assign):
            return self.translate_assign(stmt)
        elif isinstance(stmt, ast.AugAssign):
            return self.translate_aug_assign(stmt)
        elif isinstance(stmt, ast.FunctionDef):
            return self.translate_function_def(stmt)
        elif isinstance(stmt, ast.Return):
            return self.translate_return(stmt)
        elif isinstance(stmt, ast.If):
            return self.translate_if_stmt(stmt)
        elif isinstance(stmt, ast.For):
            return self.translate_for(stmt)
        elif isinstance(stmt, ast.While):
            return self.translate_while(stmt)
        elif isinstance(stmt, ast.Try):
            return self.translate_try(stmt)
        elif isinstance(stmt, ast.Raise):
            return self.translate_raise(stmt)
        elif isinstance(stmt, ast.Import):
            return self.translate_import(stmt)
        elif isinstance(stmt, ast.Pass):
            # Pass translates to #f (false) - no-op
            return encode_boolean(False)
        elif isinstance(stmt, ast.Nonlocal):
            # Nonlocal declarations affect scoping (handled in
            # collect_assigned_vars and the box analysis), but emit no
            # runtime bytecode of their own.
            return encode_boolean(False)
        elif isinstance(stmt, ast.Global):
            # Global declarations are also a scoping signal; subsequent
            # reads/writes of the named variables resolve to the
            # program-wide symbol bindings rather than to a local.
            return encode_boolean(False)
        elif isinstance(stmt, ast.Break):
            if self._loop_depth == 0:
                raise NotImplementedError(
                    "'break' outside loop")
            return self._raise_sentinel(self.BREAK_SENTINEL)
        elif isinstance(stmt, ast.Continue):
            if self._loop_depth == 0:
                raise NotImplementedError(
                    "'continue' outside loop")
            return self._raise_sentinel(self.CONTINUE_SENTINEL)
        else:
            raise NotImplementedError(f"Statement type not supported: {type(stmt).__name__}")

    @_located_dispatch
    def translate_expr(self, expr: ast.expr) -> bytes:
        """Translate an expression node."""
        if isinstance(expr, ast.Constant):
            return self.translate_constant(expr)
        elif isinstance(expr, ast.Name):
            return self.translate_name(expr)
        elif isinstance(expr, ast.BinOp):
            return self.translate_binop(expr)
        elif isinstance(expr, ast.UnaryOp):
            return self.translate_unaryop(expr)
        elif isinstance(expr, ast.Compare):
            return self.translate_compare(expr)
        elif isinstance(expr, ast.BoolOp):
            return self.translate_boolop(expr)
        elif isinstance(expr, ast.Call):
            return self.translate_call(expr)
        elif isinstance(expr, ast.List):
            return self.translate_list(expr)
        elif isinstance(expr, ast.Tuple):
            return self.translate_tuple(expr)
        elif isinstance(expr, ast.Dict):
            return self.translate_dict(expr)
        elif isinstance(expr, ast.Subscript):
            return self.translate_subscript(expr)
        elif isinstance(expr, ast.Lambda):
            return self.translate_lambda(expr)
        elif isinstance(expr, ast.IfExp):
            return self.translate_ifexp(expr)
        elif isinstance(expr, ast.ListComp):
            return self.translate_list_comp(expr)
        elif isinstance(expr, ast.Attribute):
            return self.translate_attribute(expr)
        elif isinstance(expr, ast.JoinedStr):
            return self.translate_joined_str(expr)
        else:
            raise NotImplementedError(f"Expression type not supported: {type(expr).__name__}")

    def translate_constant(self, node: ast.Constant) -> bytes:
        """Translate a constant literal (int, bool, str, None)."""
        value = node.value

        if isinstance(value, bool):
            return encode_boolean(value)
        elif isinstance(value, int):
            return encode_integer(value)
        elif isinstance(value, str):
            return encode_string(value, self.bc)
        elif value is None:
            # None -> empty list or #f
            return encode_boolean(False)
        elif isinstance(value, float):
            # Float: convert to integer for now (VeloxVM has optional float support)
            # TODO: Implement proper float encoding if VM_ENABLE_REALS is set
            return encode_integer(int(value))
        else:
            raise NotImplementedError(f"Constant type not supported: {type(value)}")

    def translate_joined_str(self, node: ast.JoinedStr) -> bytes:
        """Translate an f-string into a chain of `string_append` calls.

        `f"hello {name}!"` parses as a JoinedStr whose `values` are a
        mix of `ast.Constant` (the literal segments) and
        `ast.FormattedValue` (the `{...}` interpolations). Each
        formatted value is routed through our `str()` conversion
        (literal fast paths plus runtime stringp/booleanp dispatch),
        and everything is concatenated with `string_append`.

        Format specs (`f"{x:.2f}"`) and conversions (`f"{x!r}"`) are
        not yet supported and raise at compile time so they can't
        silently produce wrong output.
        """
        parts: List[bytes] = []
        for piece in node.values:
            if isinstance(piece, ast.Constant) and isinstance(piece.value, str):
                # An empty segment (e.g. `f"{x}{y}"` between the two
                # placeholders) contributes nothing — skip it so the
                # `string_append` call doesn't include zero-length args.
                if piece.value:
                    parts.append(encode_string(piece.value, self.bc))
            elif isinstance(piece, ast.FormattedValue):
                if piece.conversion != -1:
                    raise NotImplementedError(
                        "f-string conversions (`!s`, `!r`, `!a`) "
                        "are not yet supported")
                if piece.format_spec is not None:
                    raise NotImplementedError(
                        "f-string format specs (`:.2f`, `:>5`, ...) "
                        "are not yet supported")
                # Reuse the str() pipeline: literals constant-fold,
                # variables get the runtime type-dispatch.
                parts.append(self.translate_str([piece.value]))
            else:
                raise NotImplementedError(
                    f"Unexpected f-string component: "
                    f"{type(piece).__name__}")

        if not parts:
            return encode_string("", self.bc)
        if len(parts) == 1:
            # No concatenation needed — a single segment is already a
            # string-typed expression.
            return parts[0]
        return create_inline_call('string_append', parts, self.bc)

    def translate_list(self, node) -> bytes:
        """Translate `[1, 2, 3]` or `(1, 2, 3)` to `(list 1 2 3)`.

        Tuples are represented as lists in VeloxVM (both are
        immutable from the user's perspective), so list and tuple
        literals share this handler. Complex elements are stored
        separately so the inline form's argc cap doesn't bite.
        """
        elem_bytecode = [self.translate_expr_with_ref(e) for e in node.elts]
        return create_inline_call('list', elem_bytecode, self.bc)

    translate_tuple = translate_list

    def translate_dict(self, node: ast.Dict) -> bytes:
        """
        Translate a dict literal to association list.
        {'a': 1, 'b': 2} -> (list (cons 'a 1) (cons 'b 2))
        Complex keys/values stored separately.
        """
        pairs = []

        for key, value in zip(node.keys, node.values):
            key_bytes = self.translate_expr_with_ref(key)
            val_bytes = self.translate_expr_with_ref(value)

            # Create (cons key value)
            pair_bytes = create_inline_call('cons', [key_bytes, val_bytes], self.bc)
            # Store each cons pair separately
            pairs.append(self._hoist(pair_bytes))

        # Wrap in (list pair1 pair2 ...)
        return create_inline_call('list', pairs, self.bc)

    def translate_name(self, node: ast.Name) -> bytes:
        """Translate a variable reference."""
        safe_name = self.get_safe_name(node.id)
        return self._emit_name_load(safe_name)

    def translate_assign(self, node: ast.Assign) -> bytes:
        """Dispatch `target = value` to a per-target-shape helper.

        Supported target shapes:
        - `ast.Subscript` — `d['k'] = v`, handled as a dict rebind by
          `translate_dict_subscript_assign`.
        - `ast.Tuple`     — `a, b = pair`, lowered to a sequence of
          `(define <name> (list-ref value <i>))`.
        - `ast.Name`      — `x = expr`, emitted as `define`/`set!`/
          `box-set!` depending on scope and whether the binding is
          boxed for closure aliasing.

        Multi-target assignment (`a = b = 5`) is refused.
        """
        if len(node.targets) != 1:
            raise NotImplementedError("Multiple assignment targets not supported")

        target = node.targets[0]

        if isinstance(target, ast.Subscript):
            return self.translate_dict_subscript_assign(target, node.value)
        if isinstance(target, ast.Tuple):
            return self._assign_tuple_unpack(target, node.value)
        if isinstance(target, ast.Name):
            return self._assign_simple(target, node.value)
        raise NotImplementedError(
            f"Assignment target type not supported: {type(target).__name__}")

    def _assign_tuple_unpack(self, target: ast.Tuple,
                             value_expr: ast.expr) -> bytes:
        """Lower `a, b, ... = value` to
        `(begin (define a (list-ref value 0))
                (define b (list-ref value 1))
                ...)`.

        Each unpacked name gets its own `define`. The element
        expressions are stored separately by create_inline_call's
        nested-inline-form auto-hoist, so the outer `begin` stays a
        single inline form.
        """
        if not all(isinstance(elt, ast.Name) for elt in target.elts):
            raise NotImplementedError(
                "Tuple unpacking only supports simple variable names")
        target_names = [self.get_safe_name(elt.id) for elt in target.elts]
        value_bytes = self.translate_expr_with_ref(value_expr)

        current_scope = self.scope_stack[-1]
        defines = []
        for i, name in enumerate(target_names):
            list_ref = create_inline_call(
                'list_ref', [value_bytes, encode_integer(i)], self.bc)
            defines.append(create_inline_call(
                'define', [encode_symbol(name, self.bc), list_ref], self.bc))
            current_scope.add(name)
        return create_inline_call('begin', defines, self.bc)

    def _assign_simple(self, target: ast.Name,
                       value_expr: ast.expr) -> bytes:
        """Lower `x = expr` to one of `define` / `set!` / `box-set!`.

        - The first time `x` is bound in the current scope, emit
          `define`.
        - Re-assignment in the current scope or an outer scope emits
          `set!` (to mutate, not shadow). When the binding is in a
          heap box because an inner closure captures and mutates it,
          route through `box-set!` instead.
        """
        safe_name = self.get_safe_name(target.id)
        value_bytes = self.translate_expr_with_ref(value_expr)
        sym = encode_symbol(safe_name, self.bc)

        current_scope = self.scope_stack[-1]
        in_outer_scope = any(safe_name in scope
                             for scope in self.scope_stack[:-1])

        if safe_name in current_scope or in_outer_scope:
            op = 'box_set' if self._is_boxed(safe_name) else 'set'
        else:
            current_scope.add(safe_name)
            op = 'define'
        return create_inline_call(op, [sym, value_bytes], self.bc)

    def translate_aug_assign(self, node: ast.AugAssign) -> bytes:
        """
        Translate augmented assignment: x += 5 -> (set! x (+ x 5)).
        The operation result is stored separately.
        """
        if not isinstance(node.target, ast.Name):
            raise NotImplementedError("Aug assignment only supports simple variables")

        var_name = node.target.id
        safe_name = self.get_safe_name(var_name)

        # Map operator
        op_map = {
            ast.Add: 'add',
            ast.Sub: 'subtract',
            ast.Mult: 'multiply',
            ast.Div: 'divide',
            ast.FloorDiv: 'quotient',
            ast.Mod: 'modulo',
        }

        op = op_map.get(type(node.op))
        if not op:
            raise NotImplementedError(f"Aug assign operator not supported: {type(node.op).__name__}")

        # (op var value) -- read of var goes through _emit_name_load so a
        # boxed binding emits (box-ref var).
        var_bytes = self._emit_name_load(safe_name)
        value_bytes = self.translate_expr_with_ref(node.value)
        result_bytes = create_inline_call(op, [var_bytes, value_bytes], self.bc)

        # Store the operation separately and reference it
        result_ref = self._hoist(result_bytes)

        # (set! var result) or (box-set! var result) when boxed.
        write_op = 'box_set' if self._is_boxed(safe_name) else 'set'
        return create_inline_call(write_op,
                                  [encode_symbol(safe_name, self.bc), result_ref],
                                  self.bc)

    def _analyze_body(self,
                      body_stmts: List[ast.stmt],
                      local_names: Set[str],
                      outer_env: Set[str]) -> "tuple[List[str], Set[str]]":
        """One AST walk that returns everything `translate_function_def`
        and `translate_lambda` need about their body's free-variable
        and binding behaviour:

        - **captures** — free references (Load of a name not bound in
          this function), filtered to names that exist in `outer_env`
          and ordered by first reference. Top-level globals are
          excluded by leaving them out of `outer_env`, so they resolve
          via program-wide symbol bindings rather than as captures.

        - **needs_box** — the subset of `local_names` that has to live
          in a heap-allocated box: captured by some nested closure
          AND mutated somewhere. Pure value captures and mutated-but-
          uncaptured locals are fine as plain bindings; only the
          intersection has the alias-sharing problem.

        Folds three walkers into one. The bookkeeping splits into
        `bound` (all bindings visible at the current point, used to
        detect free names) and `inner_local` (bindings introduced
        AFTER entering a nested function, used to detect shadowing of
        our locals from inside a nested closure).
        """
        free: List[str] = []
        seen: Set[str] = set()
        captured_by_inner: Set[str] = set()
        mutated: Set[str] = set()

        def record_target(target, inner_local: Set[str]) -> None:
            if isinstance(target, ast.Name):
                safe = self.get_safe_name(target.id)
                if safe in local_names and safe not in inner_local:
                    mutated.add(safe)
            elif isinstance(target, ast.Tuple):
                for elt in target.elts:
                    record_target(elt, inner_local)

        def walk(node, bound: Set[str], inside: bool,
                 inner_local: Set[str]) -> None:
            if isinstance(node, ast.Name):
                if isinstance(node.ctx, ast.Load):
                    safe = self.get_safe_name(node.id)
                    if safe not in bound and safe not in seen:
                        seen.add(safe)
                        free.append(safe)
                    if (inside and safe in local_names
                            and safe not in inner_local):
                        captured_by_inner.add(safe)
                return

            if isinstance(node, ast.Assign):
                for t in node.targets:
                    record_target(t, inner_local)
                walk(node.value, bound, inside, inner_local)
                return
            if isinstance(node, ast.AugAssign):
                record_target(node.target, inner_local)
                walk(node.value, bound, inside, inner_local)
                return

            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                inner_params = {self.get_safe_name(a.arg)
                                for a in node.args.args}
                inner_locals = self.collect_assigned_vars(node.body)
                new_bound = bound | inner_params | inner_locals
                new_inner_local = inner_local | inner_params | inner_locals
                for stmt in node.body:
                    walk(stmt, new_bound, True, new_inner_local)
                return

            if isinstance(node, ast.Lambda):
                inner_params = {self.get_safe_name(a.arg)
                                for a in node.args.args}
                walk(node.body,
                     bound | inner_params,
                     True,
                     inner_local | inner_params)
                return

            for child in ast.iter_child_nodes(node):
                walk(child, bound, inside, inner_local)

        for stmt in body_stmts:
            walk(stmt, local_names, False, set())

        captures = [n for n in free if n in outer_env]
        needs_box = captured_by_inner & mutated
        return captures, needs_box

    def _is_boxed(self, safe_name: str) -> bool:
        """
        True if safe_name resolves to a binding that lives in a heap box at
        this point in translation. Walks the scope stack inward-to-outward
        and checks the box flag of the innermost scope that contains the
        name -- so a shadowing local in an inner scope correctly hides an
        outer boxed binding.
        """
        for i in range(len(self.scope_stack) - 1, -1, -1):
            if safe_name in self.scope_stack[i]:
                return safe_name in self.boxed_stack[i]
        return False

    def _emit_name_load(self, safe_name: str) -> bytes:
        """
        Emit bytecode for a Load of safe_name. If the binding is boxed, wrap
        in (box_ref name); otherwise emit the bare symbol.
        """
        if self._is_boxed(safe_name):
            return create_inline_call(
                'box_ref', [encode_symbol(safe_name, self.bc)], self.bc)
        return encode_symbol(safe_name, self.bc)

    def _hoist(self, call_bytes: bytes) -> bytes:
        """Stash `call_bytes` in the expression table and return the
        form-ref that references it. Used wherever a complex inline
        form has to appear in a context that only accepts a single
        token (an `if` branch, a nested-form argument, etc.).
        """
        return encode_form_ref(self.bc.add_expression(call_bytes))

    def _mutate_simple_name(self, receiver: ast.expr, label: str,
                            build_new_value: Callable[[bytes], bytes]) -> bytes:
        """Scaffolding for handlers that mutate a simple-named
        variable: `lst.append(x)`, `d['k'] = v`, etc.

        Validates that `receiver` is an `ast.Name`, encodes its safe
        symbol token, calls `build_new_value(var_token)` to produce
        the replacement value, and emits `(set! var new_value)`. The
        receiver-validation message is `"<label> only supports simple
        variable targets"`.

        Used so the receiver-Name restriction lives in one place
        (where we'll later either tighten it or lift it).
        """
        if not isinstance(receiver, ast.Name):
            raise NotImplementedError(
                f"{label} only supports simple variable targets")
        var_token = encode_symbol(
            self.get_safe_name(receiver.id), self.bc)
        new_value = build_new_value(var_token)
        return create_inline_call('set', [var_token, new_value], self.bc)

    @contextlib.contextmanager
    def _function_scope(self, local_set: Set[str],
                        body_stmts: List[ast.stmt]):
        """Enter a fresh function-level scope to translate `body_stmts`.

        Runs the closure/box analysis (`_analyze_body`) and yields
        `(captures, boxed_names)` for the body builder to use. Pushes
        scope_stack and boxed_stack, zeros out _loop_depth (loops
        don't cross function boundaries), and unwinds all of that
        when the context exits.

        Used by translate_function_def and translate_lambda to share
        their identical setup/teardown.
        """
        outer_env: Set[str] = (set().union(*self.scope_stack[1:])
                               if len(self.scope_stack) > 1 else set())
        captures, boxed_names = self._analyze_body(
            body_stmts, local_set, outer_env)

        self.scope_stack.append(local_set)
        self.boxed_stack.append(boxed_names)
        saved_loop_depth = self._loop_depth
        self._loop_depth = 0
        try:
            yield captures, boxed_names
        finally:
            self.scope_stack.pop()
            self.boxed_stack.pop()
            self._loop_depth = saved_loop_depth

    def _emit_lambda(self, params: List[str], body: bytes, *,
                     is_function: bool = False,
                     captures: Optional[List[str]] = None) -> bytes:
        """Build a `(bind[_function] params... body)` form, store it
        in the expression table, and return its lambda form-ref.

        `is_function=True` marks the lambda as a Python function
        boundary (used by the `return` primitive to know what to
        unwind to); plain lambdas used for control flow stay False.

        `captures`, if given, is a list of safe names whose IDs are
        recorded against the lambda's expression for the runtime
        closure machinery.
        """
        bind_bytes = create_bind_form(
            params, body, self.bc, is_function=is_function)
        expr_id = self.bc.add_expression(bind_bytes)
        if captures:
            capture_ids = [self.bc.symbol_table.add_symbol(name)
                           for name in captures]
            self.bc.record_captures(expr_id, capture_ids)
        return encode_form_lambda(expr_id)

    def _emit_type_dispatch(self, arg_token: bytes,
                            branches: List["tuple[str, bytes]"],
                            fallback: bytes) -> bytes:
        """Build a chain of `(if (test arg_token) branch ...)` ending
        in `fallback`. `branches` is `[(test-op-name, branch-bytes),
        ...]` from outermost test to innermost.

        Used by `int(x)` and `str(x)` to do their runtime type
        dispatch without hand-rolling the same nested-if scaffold.
        Each test call is hoisted into the expression table; the
        intermediate `if` forms are too. The outermost `if` is
        returned inline, leaving its placement up to the caller.
        """
        # Build innermost-to-outermost so each subsequent `if` gets
        # the previous chain as its else branch.
        result = fallback
        last = len(branches) - 1
        for i, (test_op, branch_bytes) in enumerate(reversed(branches)):
            test_ref = self._hoist(create_inline_call(
                test_op, [arg_token], self.bc))
            if_form = create_inline_call(
                'if', [test_ref, branch_bytes, result], self.bc)
            # Hoist every intermediate so the outer `if` sees a
            # single-token else branch. The final iteration (the
            # outermost `if`) is what we return — not hoisted.
            result = if_form if i == last else self._hoist(if_form)
        return result

    def _evaluate_once(self, arg_node: ast.expr,
                       build: Callable[[bytes], bytes]) -> bytes:
        """Evaluate `arg_node` exactly once, then run the bytecode
        produced by `build(arg_token)`.

        For single-token args (Constant, Name, Lambda) the value
        token has no side effects on re-read, so we hand it straight
        to `build`. For everything else — function calls, arithmetic,
        comprehensions, etc. — we wrap in `((lambda (_t) <build(_t)>)
        <arg>)` so the argument's side effects fire once and `_t` is
        a plain symbol thereafter.

        Used by handlers that emit a runtime type-dispatch
        (`int`, `str`, `abs`, ...) where the dispatch references the
        argument from multiple branches.
        """
        if self.encodes_as_single_token(arg_node):
            return build(self.translate_expr(arg_node))

        arg_bytes = self.translate_expr_with_ref(arg_node)
        temp_name = self.bc.get_unique_var_name("_t")
        temp_token = encode_symbol(temp_name, self.bc)
        lambda_ref = self._emit_lambda([temp_name], build(temp_token))
        return create_inline_call(lambda_ref, [arg_bytes], self.bc)

    def _make_box_init_bytes(self, safe_name: str) -> bytes:
        """
        Emit (set! name (box name)) -- the function-entry wrap that replaces
        the initial value of name with a box containing it. Subsequent reads
        of name will be rewritten to (box-ref name) by _emit_name_load. The
        inner (box name) is a plain symbol reference; we cannot route it
        through _emit_name_load because at this point name still holds its
        unboxed value.
        """
        box_call = create_inline_call('box',
                                      [encode_symbol(safe_name, self.bc)],
                                      self.bc)
        return create_inline_call('set',
                                  [encode_symbol(safe_name, self.bc), box_call],
                                  self.bc)

    def collect_assigned_vars(self, stmts: List[ast.stmt]) -> Set[str]:
        """
        Collect all variable names that are assigned within a list of statements.
        This is used for Python's function-level scoping - all variables must be
        hoisted to the top of the function.

        Names declared with `nonlocal` or `global` are excluded from the
        result: nonlocal names refer to bindings in an enclosing function,
        and global names refer to module-level (program-wide) bindings.
        Neither should be hoisted as a function-local.

        Returns safe names (with 'py_' prefix if they conflict with VM primitives).
        """
        assigned = set()
        nonlocal_names = set()
        global_names = set()

        def visit_node(node):
            if isinstance(node, ast.Nonlocal):
                for name in node.names:
                    nonlocal_names.add(self.get_safe_name(name))
                return
            if isinstance(node, ast.Global):
                for name in node.names:
                    global_names.add(self.get_safe_name(name))
                return
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        assigned.add(self.get_safe_name(target.id))
            elif isinstance(node, ast.AugAssign):
                if isinstance(node.target, ast.Name):
                    assigned.add(self.get_safe_name(node.target.id))
            elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                # A nested def binds its name in the enclosing function's
                # scope. We do not recurse into the def's body since that's
                # a separate scope.
                assigned.add(self.get_safe_name(node.name))
            elif isinstance(node, ast.For):
                if isinstance(node.target, ast.Name):
                    assigned.add(self.get_safe_name(node.target.id))
                for stmt in node.body:
                    visit_node(stmt)
                for stmt in node.orelse:
                    visit_node(stmt)
            elif isinstance(node, ast.While):
                for stmt in node.body:
                    visit_node(stmt)
                for stmt in node.orelse:
                    visit_node(stmt)
            elif isinstance(node, ast.If):
                for stmt in node.body:
                    visit_node(stmt)
                for stmt in node.orelse:
                    visit_node(stmt)
            elif isinstance(node, ast.Try):
                for stmt in node.body:
                    visit_node(stmt)
                for handler in node.handlers:
                    if handler.name:
                        assigned.add(self.get_safe_name(handler.name))
                    for stmt in handler.body:
                        visit_node(stmt)
                for stmt in node.orelse:
                    visit_node(stmt)
                for stmt in node.finalbody:
                    visit_node(stmt)

        for stmt in stmts:
            visit_node(stmt)

        return assigned - nonlocal_names - global_names

    def translate_function_def(self, node: ast.FunctionDef) -> bytes:
        """Translate `def f(a, b): ...` to `(define f (lambda (a b) ...))`.

        Python has function-level scoping, so all variables assigned
        in the body are hoisted to the top of the lambda via the
        let-expansion in `translate_function_body`; subsequent
        assignments use `set!`.
        """
        safe_func_name = self.get_safe_name(node.name)
        safe_params = [self.get_safe_name(a.arg) for a in node.args.args]

        # Encode the function name symbol FIRST so it appears in the
        # symbol table before any of its locals.
        func_name_symbol = encode_symbol(safe_func_name, self.bc)

        # Body's local vars get hoisted by the let-expansion in
        # translate_function_body — drop the params, which are already
        # bound by the lambda's bind form.
        local_vars = (self.collect_assigned_vars(node.body)
                      - set(safe_params))
        local_set = set(safe_params) | local_vars
        is_nested = len(self.scope_stack) > 1

        with self._function_scope(local_set, node.body) as (captures, boxed_names):
            body_bytes = self.translate_function_body(
                node.body, local_vars, box_inits=boxed_names)
            # is_function=True marks this as a Python function boundary
            # so the `return` primitive knows what to unwind to.
            lambda_bytes = self._emit_lambda(
                safe_params, body_bytes,
                is_function=True, captures=captures)

        # Top-level defs become global bindings via `define`. Nested
        # defs bind into the enclosing function's let-expansion local
        # (collect_assigned_vars hoists the name), so we emit `set!`
        # instead. set!'s mid-arg evaluation gives the scheduler a
        # chance to materialize a closure for a lambda with free
        # variables.
        op = 'set' if is_nested else 'define'
        return create_inline_call(op, [func_name_symbol, lambda_bytes], self.bc)

    def translate_lambda(self, node: ast.Lambda) -> bytes:
        """Translate `lambda x: expr` — Python lambdas are real
        functions (like `def`), so they get `bind_function`. Unlike
        `def`, no variable hoisting — the body is a single expression.
        """
        safe_params = [self.get_safe_name(a.arg) for a in node.args.args]
        # _analyze_body takes a statement list; wrap the body in an
        # Expr so the same analyser handles lambdas and defs.
        body_stmts = [ast.Expr(value=node.body)]

        with self._function_scope(set(safe_params), body_stmts) as (captures, boxed_names):
            body_bytes = self.translate_expr(node.body)
            if boxed_names:
                # Prepend (set! p (box p)) wraps for each boxed param,
                # joining them with the body in a begin form.
                wraps = [self._make_box_init_bytes(n)
                         for n in sorted(boxed_names)]
                body_bytes = create_inline_call(
                    'begin', wraps + [body_bytes], self.bc)
            return self._emit_lambda(
                safe_params, body_bytes,
                is_function=True, captures=captures)

    def translate_return(self, node: ast.Return) -> bytes:
        """Translate `return value` to `(return value)`.

        The VM's `return` primitive distinguishes `bind_function` frames
        (actual functions) from regular `bind` frames (while loops,
        let-expansion), so it unwinds to the nearest enclosing function.
        """
        if node.value:
            value_bytes = self.translate_expr_with_ref(node.value)
        else:
            value_bytes = encode_boolean(False)

        return create_inline_call('return', [value_bytes], self.bc)

    def translate_call(self, node: ast.Call) -> bytes:
        """Translate function call.

        Dispatch order:
          1. Method calls (`obj.m(args)`) — checked against
             `_METHOD_HANDLERS`. If `m` isn't recognised, the call
             falls through to the generic-call path so an unknown
             method can still resolve through the symbol table.
          2. Built-in name calls (`name(args)`) — checked against
             `_BUILTIN_HANDLERS`.
          3. Otherwise, generic call: emit `(callee args...)`.
        """
        # Special-case 1: known method calls.
        if isinstance(node.func, ast.Attribute):
            handler = self._METHOD_HANDLERS.get(node.func.attr)
            if handler is not None:
                return handler(self, node.func.value, node.args)

        # Resolve a Name callee once. callee_name is the source-level
        # name (or None for compound callees); func_bytes is the
        # encoded operator to emit.
        if isinstance(node.func, ast.Name):
            callee_name = node.func.id
            # Special-case 2: known built-ins.
            handler_name = self._BUILTIN_HANDLERS.get(callee_name)
            if handler_name is not None:
                return getattr(self, handler_name)(node.args)
            # Resolve the callee carefully: when a user binding shadows
            # a primitive (tracked in renamed_vars by get_safe_name),
            # call the user's py_-prefixed function; otherwise, if the
            # name matches a primitive, emit the primitive ID directly
            # instead of going through the variable path (which would
            # rename thread_sleep -> py_thread_sleep and produce an
            # unbound app symbol).
            if callee_name in self.renamed_vars:
                func_bytes = encode_symbol(self.renamed_vars[callee_name], self.bc)
            elif self.is_vm_primitive(callee_name):
                func_bytes = encode_symbol(callee_name, self.bc)
            else:
                func_bytes = self.translate_expr(node.func)
        else:
            # Compound callee (e.g. ((f x) y), or (obj.method() ...)).
            # Lift to a form-ref so the runtime sees a single token —
            # inlining would corrupt byte parsing. encodes_as_single_token
            # exempts ast.Lambda, so a literal ((lambda ...) ...) still
            # inlines.
            callee_name = None
            func_bytes = self.translate_expr_with_ref(node.func)

        arg_bytes = [self.translate_expr_with_ref(arg) for arg in node.args]

        # If this call names a function with recorded defaults, pad
        # missing trailing arguments with the defaults' encoded bytes.
        # The pre-pass restricts defaults to literals, so reusing the
        # same bytes at every call site is safe.
        if callee_name is not None:
            defaults = self._get_function_defaults(self.get_safe_name(callee_name))
            if defaults is not None:
                n_provided = len(arg_bytes)
                n_total = len(defaults)
                if n_provided > n_total:
                    raise NotImplementedError(
                        f"{callee_name}() takes at most {n_total} "
                        f"positional arguments but {n_provided} were given")
                for i in range(n_provided, n_total):
                    if defaults[i] is None:
                        raise NotImplementedError(
                            f"{callee_name}() missing required "
                            f"positional argument at index {i}")
                    arg_bytes.append(defaults[i])

        # Emit (callee args...). create_inline_call accepts pre-encoded
        # operator bytes and handles the nested-inline-form auto-hoist
        # for arguments — no need to open-code the form construction.
        return create_inline_call(func_bytes, arg_bytes, self.bc)

    def translate_binop(self, node: ast.BinOp) -> bytes:
        """
        Translate binary operation: a + b -> (+ a b).
        Complex operands are stored separately.
        String concatenation: "a" + "b" -> (string-append "a" "b")
        """
        # Special handling for string and list concatenation
        if isinstance(node.op, ast.Add):
            # Check if both operands are string literals
            if (isinstance(node.left, ast.Constant) and isinstance(node.left.value, str) and
                isinstance(node.right, ast.Constant) and isinstance(node.right.value, str)):
                # String concatenation
                left_bytes = self.translate_expr_with_ref(node.left)
                right_bytes = self.translate_expr_with_ref(node.right)
                return create_inline_call('string_append', [left_bytes, right_bytes], self.bc)

            # Check if either operand is a list literal (for list concatenation)
            if isinstance(node.left, ast.List) or isinstance(node.right, ast.List):
                # List concatenation
                left_bytes = self.translate_expr_with_ref(node.left)
                right_bytes = self.translate_expr_with_ref(node.right)
                return create_inline_call('append', [left_bytes, right_bytes], self.bc)

        op_map = {
            ast.Add: 'add',
            ast.Sub: 'subtract',
            ast.Mult: 'multiply',
            ast.Div: 'divide',
            ast.FloorDiv: 'quotient',
            ast.Mod: 'modulo',
            ast.BitAnd: 'bit_and',
            ast.BitOr: 'bit_or',
            ast.BitXor: 'bit_xor',
            ast.LShift: 'bit_shift',  # (bit_shift a b) for a << b
            ast.RShift: 'bit_shift',  # (bit_shift a -b) for a >> b
        }

        op = op_map.get(type(node.op))
        if not op:
            raise NotImplementedError(f"Binary operator not supported: {type(node.op).__name__}")

        left_bytes = self.translate_expr_with_ref(node.left)

        if isinstance(node.op, ast.RShift):
            # Right shift: negate the shift amount
            right_expr_bytes = self.translate_expr_with_ref(node.right)
            right_bytes = self._hoist(
                create_inline_call('subtract',
                                   [encode_integer(0), right_expr_bytes],
                                   self.bc))
        else:
            right_bytes = self.translate_expr_with_ref(node.right)

        return create_inline_call(op, [left_bytes, right_bytes], self.bc)

    def translate_unaryop(self, node: ast.UnaryOp) -> bytes:
        """Translate unary operation. Complex operands are stored separately."""
        operand_bytes = self.translate_expr_with_ref(node.operand)

        if isinstance(node.op, ast.Not):
            return create_inline_call('not', [operand_bytes], self.bc)
        elif isinstance(node.op, ast.USub):
            # Unary minus: (- 0 x)
            return create_inline_call('subtract', [encode_integer(0), operand_bytes], self.bc)
        elif isinstance(node.op, ast.UAdd):
            # Unary plus: just return the operand
            return operand_bytes
        elif isinstance(node.op, ast.Invert):
            # Bitwise NOT: (bit_invert x)
            return create_inline_call('bit_invert', [operand_bytes], self.bc)
        else:
            raise NotImplementedError(f"Unary operator not supported: {type(node.op).__name__}")

    def _emit_eq(self, left_bytes: bytes, right_bytes: bytes,
                 negate: bool) -> bytes:
        """Emit Python-style equality.

        Routes through the VM's `equalp` (deep, type-aware) primitive
        rather than the numeric-only `equal`. `equal` silently returns
        false for any non-numeric operand, which made `"a" == "a"` and
        `True == True` evaluate to false. `equalp` returns false on
        type mismatch, matching Python `==`'s cross-type behaviour.
        """
        eq_call = create_inline_call('equalp', [left_bytes, right_bytes], self.bc)
        if not negate:
            return eq_call
        return create_inline_call('not', [self._hoist(eq_call)], self.bc)

    def translate_compare(self, node: ast.Compare) -> bytes:
        """
        Translate comparison.

        Supports chaining: a < b < c -> (and (< a b) (< b c))
        Complex operands are stored separately.
        """
        # `==` and `!=` are handled separately because they need to work
        # for strings, booleans, lists, etc. — not just numbers. Other
        # relational ops stay on the numeric primitives.
        op_map = {
            ast.Lt: 'less_than',
            ast.LtE: 'less_than_equal',
            ast.Gt: 'greater_than',
            ast.GtE: 'greater_than_equal',
        }

        if len(node.comparators) == 1:
            # Simple comparison
            op_type = type(node.ops[0])

            # Special handling for 'in' and 'not in'
            if op_type == ast.In:
                left_bytes = self.translate_expr_with_ref(node.left)
                right_bytes = self.translate_expr_with_ref(node.comparators[0])

                # If comparator is a Dict literal, we know it's a dict - use assoc
                # Otherwise, use member (works for lists; for dicts use .get() method)
                is_dict_literal = isinstance(node.comparators[0], ast.Dict)

                if is_dict_literal:
                    # key in dict-literal -> (if (assoc key dict) #t #f)
                    assoc_call = create_inline_call('assoc', [left_bytes, right_bytes], self.bc)
                    assoc_ref = self._hoist(assoc_call)
                    return create_inline_call('if', [assoc_ref, encode_boolean(True), encode_boolean(False)], self.bc)
                else:
                    # x in container -> (if (member x container) #t #f)
                    # Note: For dict variables, use d.get(key) is not None instead
                    member_call = create_inline_call('member', [left_bytes, right_bytes], self.bc)
                    member_ref = self._hoist(member_call)
                    return create_inline_call('if', [member_ref, encode_boolean(True), encode_boolean(False)], self.bc)

            elif op_type == ast.NotIn:
                left_bytes = self.translate_expr_with_ref(node.left)
                right_bytes = self.translate_expr_with_ref(node.comparators[0])

                # If comparator is a Dict literal, use assoc
                is_dict_literal = isinstance(node.comparators[0], ast.Dict)

                if is_dict_literal:
                    # key not in dict-literal -> (not (assoc key dict))
                    assoc_call = create_inline_call('assoc', [left_bytes, right_bytes], self.bc)
                    assoc_ref = self._hoist(assoc_call)
                    return create_inline_call('not', [assoc_ref], self.bc)
                else:
                    # x not in container -> (not (member x container))
                    member_call = create_inline_call('member', [left_bytes, right_bytes], self.bc)
                    member_ref = self._hoist(member_call)
                    return create_inline_call('not', [member_ref], self.bc)

            left_bytes = self.translate_expr_with_ref(node.left)
            right_bytes = self.translate_expr_with_ref(node.comparators[0])

            if op_type is ast.Eq:
                return self._emit_eq(left_bytes, right_bytes, negate=False)
            if op_type is ast.NotEq:
                return self._emit_eq(left_bytes, right_bytes, negate=True)

            op = op_map.get(op_type)
            if not op:
                raise NotImplementedError(f"Comparison operator not supported: {op_type.__name__}")

            return create_inline_call(op, [left_bytes, right_bytes], self.bc)
        else:
            # Chained comparison: a OP1 b OP2 c ...  ->  (and (OP1 a b) (OP2 b c) ...).
            # An intermediate operand (the b above) appears in two
            # comparisons. translate_expr_with_ref hands back a form-ref
            # that re-evaluates the underlying expression each time it
            # is read, so a side-effecting intermediate would fire
            # twice. Bind such operands to let-temps so each evaluates
            # exactly once. Endpoints (used in only one comparison) and
            # single-token operands (re-reading the token has no side
            # effects) stay inline.
            operands = [node.left, *node.comparators]
            wrap_params: List[str] = []
            wrap_args: List[bytes] = []
            tokens: List[bytes] = []
            for i, operand in enumerate(operands):
                used_twice = 0 < i < len(operands) - 1
                if used_twice and not self.encodes_as_single_token(operand):
                    temp = self.bc.get_unique_var_name("_t")
                    wrap_params.append(temp)
                    wrap_args.append(self.translate_expr_with_ref(operand))
                    tokens.append(encode_symbol(temp, self.bc))
                else:
                    tokens.append(self.translate_expr_with_ref(operand))

            comparisons = []
            for i, op_node in enumerate(node.ops):
                left_t, right_t = tokens[i], tokens[i + 1]
                op_type = type(op_node)
                if op_type is ast.Eq:
                    comp_bytes = self._emit_eq(left_t, right_t, negate=False)
                elif op_type is ast.NotEq:
                    comp_bytes = self._emit_eq(left_t, right_t, negate=True)
                else:
                    op = op_map.get(op_type)
                    if not op:
                        raise NotImplementedError(f"Comparison operator not supported: {op_type.__name__}")
                    comp_bytes = create_inline_call(op, [left_t, right_t], self.bc)
                comparisons.append(self._hoist(comp_bytes))

            and_chain = create_inline_call('and', comparisons, self.bc)
            if not wrap_params:
                return and_chain
            lambda_ref = self._emit_lambda(wrap_params, and_chain)
            return create_inline_call(lambda_ref, wrap_args, self.bc)

    def translate_boolop(self, node: ast.BoolOp) -> bytes:
        """Translate boolean operation: a and b -> (and a b). Complex operands stored separately."""
        op_map = {
            ast.And: 'and',
            ast.Or: 'or',
        }

        op = op_map.get(type(node.op))
        if not op:
            raise NotImplementedError(f"Boolean operator not supported: {type(node.op).__name__}")

        value_bytes = [self.translate_expr_with_ref(v) for v in node.values]
        return create_inline_call(op, value_bytes, self.bc)

    def translate_if_stmt(self, node: ast.If) -> bytes:
        """
        Translate if statement: if test: body else: alt -> (if test body alt).
        Test, body, and alt are all stored separately.
        """
        test_bytes = self.translate_expr_with_ref(node.test)
        body_bytes = self.translate_block(node.body)

        # Store body as separate expression
        body_ref = self._hoist(body_bytes)

        if node.orelse:
            alt_bytes = self.translate_block(node.orelse)
            # Store alt as separate expression
            alt_ref = self._hoist(alt_bytes)
        else:
            alt_ref = encode_boolean(False)

        return create_inline_call('if', [test_bytes, body_ref, alt_ref], self.bc)

    def translate_ifexp(self, node: ast.IfExp) -> bytes:
        """Translate if expression: x if test else y -> (if test x y). Complex parts stored separately."""
        test_bytes = self.translate_expr_with_ref(node.test)
        body_bytes = self.translate_expr_with_ref(node.body)
        alt_bytes = self.translate_expr_with_ref(node.orelse)

        return create_inline_call('if', [test_bytes, body_bytes, alt_bytes], self.bc)

    def _raise_sentinel(self, sentinel_name: str) -> bytes:
        """Emit `(raise (quote <sentinel>))`. raise evaluates its
        argument, so the symbol must be quoted to survive lookup."""
        sym_bytes = create_inline_call(
            'quote', [encode_symbol(sentinel_name, self.bc)], self.bc)
        return create_inline_call('raise', [sym_bytes], self.bc)

    def _wrap_with_sentinel_guard(self, body_bytes: bytes,
                                  sentinel_name: str) -> bytes:
        """Wrap body in a guard that swallows raises of `sentinel_name`
        and re-raises everything else:

            (guard _exc
              (if (eqp _exc (quote <sentinel>)) #f (raise _exc))
              <body>)

        The exception variable name is fixed (`_exc`); the VM doesn't
        evaluate it, so the same name can nest without conflict."""
        exc_name = '_exc'
        sentinel_quoted = create_inline_call(
            'quote', [encode_symbol(sentinel_name, self.bc)], self.bc)
        eq_check = create_inline_call(
            'eqp',
            [encode_symbol(exc_name, self.bc), sentinel_quoted],
            self.bc)
        eq_ref = self._hoist(eq_check)

        rethrow = create_inline_call(
            'raise', [encode_symbol(exc_name, self.bc)], self.bc)
        rethrow_ref = self._hoist(rethrow)

        handler = create_inline_call(
            'if', [eq_ref, encode_boolean(False), rethrow_ref], self.bc)
        handler_ref = self._hoist(handler)

        body_ref = self._hoist(body_bytes)

        return create_inline_call(
            'guard',
            [encode_symbol(exc_name, self.bc), handler_ref, body_ref],
            self.bc)

    def _translate_loop_body(self, body_stmts: List[ast.stmt]) -> bytes:
        """Translate `body_stmts` as a loop body: bumps `_loop_depth`
        so `break`/`continue` can identify themselves as in-loop, then
        wraps the result in a continue-guard. Caller still installs
        the surrounding break-guard around the whole loop."""
        self._loop_depth += 1
        try:
            body_bytes = self.translate_block(body_stmts)
        finally:
            self._loop_depth -= 1
        return self._wrap_with_sentinel_guard(
            body_bytes, self.CONTINUE_SENTINEL)

    def translate_for(self, node: ast.For) -> bytes:
        """Translate `for x in iterable: body` to
        `(for_each (lambda (x) body) iterable)` and similarly for
        tuple-unpacking targets, with surrounding break/continue
        guards so the loop-control sentinels exit cleanly.
        """
        body_bytes = self._translate_loop_body(node.body)

        if isinstance(node.target, ast.Name):
            safe_param = self.get_safe_name(node.target.id)
            body_lambda = self._emit_lambda(
                [safe_param], self._hoist(body_bytes))
        elif isinstance(node.target, ast.Tuple):
            if not all(isinstance(elt, ast.Name) for elt in node.target.elts):
                raise NotImplementedError(
                    "For loop tuple unpacking only supports simple variable names")
            target_names = [self.get_safe_name(elt.id) for elt in node.target.elts]

            # Nested-lambda shape:
            #   (for_each (lambda (_item)
            #               ((lambda (x y ...) body)
            #                (list_ref _item 0) (list_ref _item 1) ...))
            #             iterable)
            # The inner call evaluates the list_refs before binding to
            # the unpacked parameters.
            item_param = self.bc.get_unique_var_name("_item")
            inner_lambda = self._emit_lambda(target_names, body_bytes)
            list_refs = [
                create_inline_call(
                    'list_ref',
                    [encode_symbol(item_param, self.bc), encode_integer(i)],
                    self.bc)
                for i in range(len(target_names))
            ]
            inner_call_ref = self._hoist(
                create_inline_call(inner_lambda, list_refs, self.bc))
            body_lambda = self._emit_lambda([item_param], inner_call_ref)
        else:
            raise NotImplementedError(
                f"For loop target type not supported: "
                f"{type(node.target).__name__}")

        # (for_each <body_lambda> <iterable>) wrapped in break-guard.
        iterable_bytes = self.translate_expr_with_ref(node.iter)
        for_each_call = create_inline_call(
            'for_each', [body_lambda, iterable_bytes], self.bc)
        return self._wrap_with_sentinel_guard(
            for_each_call, self.BREAK_SENTINEL)

    def translate_while(self, node: ast.While) -> bytes:
        """
        Translate while loop using closure-based recursion:
        Loop variables are CAPTURED from outer scope, not passed as parameters.

        while test: body
        ->
        (define _loop (lambda ()
          (if test
            (begin body (loop))
            #f)))
        (loop)

        This matches Python semantics: loop variables are mutated via set!.
        """
        loop_name = self.bc.get_unique_loop_name()

        # Translate the test condition - INLINE it, don't use form ref
        # This ensures it's evaluated in the closure's context
        test_bytes = self.translate_expr(node.test)

        # Translate loop body — bumps loop depth so break/continue
        # know they're inside a loop, then wraps in a continue-guard
        # so a `continue` raises out of the body and lets the
        # recursive call below fire the next iteration.
        body_bytes = self._translate_loop_body(node.body)

        # Create recursive call with NO arguments (closure captures variables)
        recurse_bytes = create_inline_call(loop_name, [], self.bc)

        # Combine body with recursive call: (begin body (loop))
        body_with_recurse = create_inline_call('begin', [body_bytes, recurse_bytes], self.bc)

        # Create if: (if test (begin body (loop)) #f)
        if_bytes = create_inline_call('if', [test_bytes, body_with_recurse, encode_boolean(False)], self.bc)

        # Create lambda with NO parameters: (lambda () if-bytes)
        # INLINE the body, don't use form ref - this preserves variable capture
        lambda_bytes = self._emit_lambda([], if_bytes)

        # Define the loop function: (define loop (lambda () ...))
        define_bytes = create_inline_call('define', [encode_symbol(loop_name, self.bc), lambda_bytes], self.bc)

        # Initial call with NO arguments: (loop)
        call_bytes = create_inline_call(loop_name, [], self.bc)

        # Wrap the define and call in a BEGIN form
        # because bind form expects ONE expression, and we have two (define + call)
        # Pattern: ((lambda () (begin (define loop ...) (loop))))
        wrapper_body_begin = create_inline_call('begin', [define_bytes, call_bytes], self.bc)

        # Create wrapper bind with the begin form as body
        wrapper_lambda = self._emit_lambda([], wrapper_body_begin)

        # Call the wrapper lambda: ((lambda () (define loop ...) (loop)))
        loop_call = create_inline_call(wrapper_lambda, [], self.bc)
        # Wrap the entire while construct so `break` exits cleanly.
        return self._wrap_with_sentinel_guard(
            loop_call, self.BREAK_SENTINEL)

    def translate_try(self, node: ast.Try) -> bytes:
        """
        Translate try/except:
        try: body except Exception as e: handler
        ->
        (guard exc handler body)

        VeloxVM's `guard` takes exactly three arguments — the bound
        exception variable, the handler, and the body — and runs the
        handler on every exception. Only a single bare `except:` or
        `except Exception:` is currently implementable; typed handlers
        (`except KeyError:`) and multiple handlers would silently
        become catch-alls if we translated them, so we refuse them at
        compile time instead.
        """
        body_bytes = self.translate_block(node.body)

        if not node.handlers:
            # No handlers, just execute body
            return body_bytes

        if len(node.handlers) > 1:
            raise NotImplementedError(
                "pyvelox does not yet support multiple except clauses; use a "
                "single bare `except:` or `except Exception:` handler."
            )

        handler = node.handlers[0]
        if handler.type is not None and not (
            isinstance(handler.type, ast.Name) and handler.type.id == 'Exception'
        ):
            raise NotImplementedError(
                f"pyvelox does not yet filter on exception types "
                f"(`except {ast.unparse(handler.type)}:`). Every handler "
                f"currently catches every exception, so typed handlers would "
                f"be misleading. Use a bare `except:` for now."
            )
        exc_var = handler.name if handler.name else 'obj'

        # Handler body
        handler_bytes = self.translate_block(handler.body)

        # Inside a loop, an `except:` written by the user shouldn't
        # accidentally swallow our break/continue sentinels. Prepend a
        # filter that re-raises if the bound exception is one of those
        # sentinels:
        #   (if (or (eqp exc 'break) (eqp exc 'continue)) (raise exc) <user-handler>)
        if self._loop_depth > 0:
            handler_bytes = self._filter_loop_sentinels(exc_var, handler_bytes)

        # Store body and handler as separate expressions
        body_ref = self._hoist(body_bytes)

        handler_ref = self._hoist(handler_bytes)

        # (guard exc_var handler body) — guard's min/max argc is 3.
        return create_inline_call('guard', [
            encode_symbol(exc_var, self.bc),
            handler_ref,
            body_ref
        ], self.bc)

    def _filter_loop_sentinels(self, exc_var: str,
                               handler_bytes: bytes) -> bytes:
        """Return `(if (or (eqp exc 'break) (eqp exc 'continue))
                       (raise exc) <handler>)` so loop-control
        exceptions raised inside a try-block get propagated rather
        than absorbed by a user-written `except:` clause."""
        break_quoted = create_inline_call(
            'quote', [encode_symbol(self.BREAK_SENTINEL, self.bc)], self.bc)
        cont_quoted = create_inline_call(
            'quote', [encode_symbol(self.CONTINUE_SENTINEL, self.bc)], self.bc)

        is_break = create_inline_call(
            'eqp', [encode_symbol(exc_var, self.bc), break_quoted], self.bc)
        is_break_ref = self._hoist(is_break)

        is_cont = create_inline_call(
            'eqp', [encode_symbol(exc_var, self.bc), cont_quoted], self.bc)
        is_cont_ref = self._hoist(is_cont)

        is_sentinel = create_inline_call(
            'or', [is_break_ref, is_cont_ref], self.bc)
        is_sentinel_ref = self._hoist(is_sentinel)

        rethrow = create_inline_call(
            'raise', [encode_symbol(exc_var, self.bc)], self.bc)
        rethrow_ref = self._hoist(rethrow)

        handler_ref = self._hoist(handler_bytes)

        return create_inline_call(
            'if', [is_sentinel_ref, rethrow_ref, handler_ref], self.bc)

    def translate_import(self, node: ast.Import) -> bytes:
        """Translate `import lib` to the VM's `(import "lib")` primitive.

        The VM's import loads a port-specific library (e.g. "sensors",
        "leds") that contributes operators to the current program's
        symbol table. `import a, b` becomes a begin-sequence.
        `import x as y` is not supported."""
        calls = []
        for alias in node.names:
            if alias.asname is not None:
                raise NotImplementedError(
                    "pyvelox does not support `import ... as ...`; "
                    "port libraries are loaded by their canonical name."
                )
            name_bytes = encode_string(alias.name, self.bc)
            calls.append(create_inline_call('import', [name_bytes], self.bc))

        if len(calls) == 1:
            return calls[0]
        return create_inline_call('begin', calls, self.bc)

    def translate_raise(self, node: ast.Raise) -> bytes:
        """Translate raise statement: raise ValueError -> (raise 'ValueError)."""
        if node.exc:
            if isinstance(node.exc, ast.Call):
                exc_name = node.exc.func.id if isinstance(node.exc.func, ast.Name) else 'Exception'
            elif isinstance(node.exc, ast.Name):
                exc_name = node.exc.id
            else:
                exc_name = 'Exception'
        else:
            exc_name = 'Exception'

        # raise evaluates its argument before invocation. Without
        # quoting, the exception type name would be looked up as a
        # variable and fail with "Undefined symbol". Wrap the symbol in
        # (quote ...) so it survives evaluation as a symbol literal.
        exc_bytes = create_inline_call(
            'quote', [encode_symbol(exc_name, self.bc)], self.bc)
        return create_inline_call('raise', [exc_bytes], self.bc)

    def translate_subscript(self, node: ast.Subscript) -> bytes:
        """
        Translate subscripting:
        lst[idx] -> (list-ref lst idx)
        lst[start:end] -> slice implementation
        dict[key] -> (cdr (assoc key dict)) for string keys
        Complex operands stored separately.
        """
        value_bytes = self.translate_expr_with_ref(node.value)

        # Check if this is a slice operation
        if isinstance(node.slice, ast.Slice):
            return self.translate_slice(value_bytes, node.slice)

        index_bytes = self.translate_expr_with_ref(node.slice)

        # Detect dict access: if index is a string, use assoc
        # d['key'] -> (cdr (assoc 'key d))
        # lst[0] -> (list-ref lst 0)
        is_string_key = isinstance(node.slice, ast.Constant) and isinstance(node.slice.value, str)

        if is_string_key:
            # Dict access: (cdr (assoc key dict))
            assoc_bytes = create_inline_call('assoc', [index_bytes, value_bytes], self.bc)
            # Store assoc call separately
            assoc_ref = self._hoist(assoc_bytes)
            # Get the value from the pair with cdr
            return create_inline_call('cdr', [assoc_ref], self.bc)
        else:
            # List access: (list-ref lst idx)
            return create_inline_call('list_ref', [value_bytes, index_bytes], self.bc)

    def translate_slice(self, value_bytes: bytes, slice_node: ast.Slice) -> bytes:
        """
        Translate slicing for lists, strings, and vectors.

        The slice primitive works for all sequence types:
        - seq[start:end] -> (slice seq start end)
        - seq[start:] -> (slice seq start (length seq))
        - seq[:end] -> (slice seq 0 end)
        - seq[:] -> seq (identity - full copy)

        Handles negative indices (Python-style):
        - seq[-3:] -> last 3 elements
        - seq[:-2] -> all but last 2 elements
        - seq[-5:-2] -> elements from -5 to -2

        Step parameter (seq[::2]) not yet supported.
        """
        if slice_node.step is not None:
            raise NotImplementedError("Slice step parameter not yet supported (e.g., lst[::2])")

        # Handle start index
        if slice_node.lower is None:
            start_bytes = encode_integer(0)
            has_start = False
        else:
            start_bytes = self.translate_expr_with_ref(slice_node.lower)
            has_start = True

        # Handle end index
        has_end = slice_node.upper is not None

        # Determine if we're slicing a string, list, or vector based on context
        # The slice primitive works for all three types

        if not has_end:
            # lst[start:] or s[start:]
            if has_start:
                # For slicing without end, use slice with length as end
                # This works for lists, strings, and vectors (list-tail doesn't work for strings)
                # lst[start:] -> (slice lst start (length lst))
                # s[start:] -> (slice s start (length s))
                length_call = create_inline_call('length', [value_bytes], self.bc)
                length_ref = self._hoist(length_call)

                return create_inline_call('slice', [value_bytes, start_bytes, length_ref], self.bc)
            else:
                # lst[:] -> entire list/string (identity)
                return value_bytes
        else:
            # lst[start:end] or lst[:end]
            end_bytes = self.translate_expr_with_ref(slice_node.upper)

            # Use slice primitive: (slice value start end)
            # This handles negative indices and works for lists, strings, and vectors
            return create_inline_call('slice', [value_bytes, start_bytes, end_bytes], self.bc)

    def translate_attribute(self, node: ast.Attribute) -> bytes:
        """
        Translate attribute access: obj.attr
        This is used for accessing attributes that are not method calls.
        For method calls, translate_call handles them.
        """
        raise NotImplementedError(f"Attribute access '{node.attr}' not supported in this context")

    _RANGE_LITERAL_LIMIT = 16

    def translate_function_body(self,
                                stmts: List[ast.stmt],
                                local_vars: Set[str] = None,
                                box_inits: Set[str] = None) -> bytes:
        """
        Translate function body with granular expression storage.

        Python has function-level scoping. All local variables are hoisted
        using the let-expansion pattern: ((lambda (vars...) body) #f #f ...)

        For the final return statement, we use implicit return semantics
        (just evaluate the value). For early returns, we use the return
        primitive to unwind the stack.

        Example:
          def f(x):
              if x < 0:
                  y = 10
                  return 999
              return 42

        Becomes:
          ((lambda (y)
             (set! y 10)
             (if (< x 0) (return 999) #f)
             42)
           #f)  ; Immediately invoked with #f as initial value
        """
        if local_vars is None:
            local_vars = set()
        if box_inits is None:
            box_inits = set()

        # Build a list of (form-ref or inline) bytes for each step the body
        # runs, prepended by the box-init wraps for any params/locals that
        # need to live in heap boxes. The wraps must execute first so that
        # subsequent reads/writes (already routed through box-ref/box-set!
        # by translate_name and translate_assign) see a box.
        wrap_refs = [self._hoist(self._make_box_init_bytes(name))
                     for name in sorted(box_inits)]

        # Translate the function body statements
        # All returns (including final ones) now use exception-based unwinding
        if len(stmts) == 0:
            stmt_bytes_list = [encode_boolean(False)]
        else:
            stmt_bytes_list = [self.translate_stmt(stmt) for stmt in stmts]

        # Combine: if there are wraps OR multiple statements, use a begin
        # over (wraps + stmts) where each stmt is hoisted to its own
        # expression (form-ref). The wraps are already form-refs.
        all_stmt_refs = list(wrap_refs)
        if len(stmt_bytes_list) == 1 and not wrap_refs:
            inner_body_bytes = stmt_bytes_list[0]
        else:
            all_stmt_refs.extend(self._hoist(s) for s in stmt_bytes_list)
            inner_body_bytes = create_inline_call('begin', all_stmt_refs, self.bc)

        # LET-EXPANSION: ((lambda (vars...) body) #f #f ...)
        if local_vars:
            local_var_list = sorted(local_vars)  # Sort for deterministic output

            # Create bind form for the let-lambda: (bind x y ... body)
            # Use inner_body_bytes directly WITHOUT storing as form ref first
            let_lambda_bytes = self._emit_lambda(local_var_list, inner_body_bytes)

            # Create arguments: #f for each local variable
            false_args = [encode_boolean(False) for _ in local_var_list]

            # Create the call: ((lambda (x y ...) body) #f #f ...)
            let_call_bytes = create_inline_call(let_lambda_bytes, false_args, self.bc)

            # Return the let-expansion call directly, do NOT store as expression
            # Storing it and returning a form ref creates nested inline forms which is malformed bytecode
            # The bind form should contain the let-call bytes directly inline
            return let_call_bytes
        else:
            return inner_body_bytes

    def translate_block(self, stmts: List[ast.stmt]) -> bytes:
        """Translate a block of statements to (begin ...) or single expression."""
        if len(stmts) == 0:
            return encode_boolean(False)
        elif len(stmts) == 1:
            return self.translate_stmt(stmts[0])
        else:
            stmt_bytes = [self.translate_stmt(s) for s in stmts]
            return create_inline_call('begin', stmt_bytes, self.bc)
