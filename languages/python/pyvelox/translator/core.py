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
        # None (the i-th positional argument is required) or the
        # already-encoded bytes of its literal default value. Used by
        # translate_call to pad missing arguments at the call site.
        self._function_defaults: Dict[str, List[Optional[bytes]]] = {}

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

        # Encode each default. Defaults align right: the last
        # len(args.defaults) positional args are the ones with defaults.
        n_args = len(args.args)
        n_defaults = len(args.defaults)
        encoded: List[Optional[bytes]] = []
        for i in range(n_args):
            if i < n_args - n_defaults:
                encoded.append(None)
                continue
            default_node = args.defaults[i - (n_args - n_defaults)]
            encoded.append(self._encode_literal_default(default_node))

        self._function_defaults[self.get_safe_name(node.name)] = encoded

    def _encode_literal_default(self, default_node: ast.expr) -> bytes:
        """Default values must be literal constants so they can be
        emitted at every call site without re-evaluating side effects.
        Mutable defaults (`x=[]`) would be a footgun even if we
        supported them, so we don't try."""
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
        return self.translate_constant(default_node)

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
        """
        Translate assignment: x = 5 -> (define x 5) or (set! x 5).

        Also handles:
        - Dict subscript assignment: d['key'] = val -> (set! d (cons (cons 'key val) (filter ... d)))
        - Tuple unpacking: x, y = (1, 2) -> (begin (define _temp value) (define x (car _temp)) (define y (cdr _temp)))

        Uses 'define' for the first assignment to a variable in the current scope,
        and 'set!' for assignments to variables from outer scopes (to avoid shadowing).
        """
        if len(node.targets) != 1:
            raise NotImplementedError("Multiple assignment targets not supported")

        target = node.targets[0]

        # Handle dict subscript assignment: d['key'] = value
        if isinstance(target, ast.Subscript):
            return self.translate_dict_subscript_assign(target, node.value)

        # Handle tuple unpacking: x, y = value
        if isinstance(target, ast.Tuple):
            # Extract target variable names
            if not all(isinstance(elt, ast.Name) for elt in target.elts):
                raise NotImplementedError("Tuple unpacking only supports simple variable names")

            target_names = [self.get_safe_name(elt.id) for elt in target.elts]

            # Translate the value expression (evaluate it once)
            value_bytes = self.translate_expr_with_ref(node.value)

            # Simple approach: (begin (define a (list-ref val 0)) (define b (list-ref val 1)))
            # create_inline_call will automatically handle nested inline forms by storing them

            define_calls = []
            current_scope = self.scope_stack[-1]

            for i, target_name in enumerate(target_names):
                index_bytes = encode_integer(i)
                # (list-ref value i) - This will be stored as expression by create_inline_call
                list_ref_call = create_inline_call('list_ref', [value_bytes, index_bytes], self.bc)

                # (define target (list-ref value i))
                define_call = create_inline_call('define',
                                                [encode_symbol(target_name, self.bc), list_ref_call],
                                                self.bc)
                define_calls.append(define_call)
                current_scope.add(target_name)

            # (begin (define a ...) (define b ...))
            return create_inline_call('begin', define_calls, self.bc)

        # Handle simple name assignment
        if not isinstance(target, ast.Name):
            raise NotImplementedError(f"Assignment target type not supported: {type(target).__name__}")

        var_name = target.id
        safe_name = self.get_safe_name(var_name)
        value_bytes = self.translate_expr_with_ref(node.value)

        # Check if variable is already defined in current scope
        # NOTE: We track SAFE names in scope_stack, not original names
        current_scope = self.scope_stack[-1]

        # Check if variable exists in any outer scope
        in_outer_scope = any(safe_name in scope for scope in self.scope_stack[:-1])

        if safe_name in current_scope:
            # Re-assignment in current scope: set! (or box-set! if boxed).
            op = 'box_set' if self._is_boxed(safe_name) else 'set'
            return create_inline_call(op, [encode_symbol(safe_name, self.bc), value_bytes], self.bc)
        elif in_outer_scope:
            # Assignment to variable from outer scope: set! to modify, not shadow.
            # If the outer binding is boxed, route through box-set!.
            op = 'box_set' if self._is_boxed(safe_name) else 'set'
            return create_inline_call(op, [encode_symbol(safe_name, self.bc), value_bytes], self.bc)
        else:
            # First assignment in current scope - use define
            current_scope.add(safe_name)
            return create_inline_call('define', [encode_symbol(safe_name, self.bc), value_bytes], self.bc)

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
        """
        Translate function definition:
        def f(a, b): return a + b
        ->
        (define f (lambda (a b) (+ a b)))

        Python has function-level scoping, so we hoist all variable
        definitions to the top of the function, then use set! for all assignments.
        """
        func_name = node.name
        safe_func_name = self.get_safe_name(func_name)
        params = [arg.arg for arg in node.args.args]
        safe_params = [self.get_safe_name(p) for p in params]

        # Encode the function name symbol FIRST, before compiling the body
        # This ensures function names appear before local variables in the symbol table
        func_name_symbol = encode_symbol(safe_func_name, self.bc)

        # Collect all variables assigned in the function body (for hoisting)
        assigned_vars = self.collect_assigned_vars(node.body)
        # Remove parameters - they're already defined by the bind form
        # NOTE: We compare SAFE names here because collect_assigned_vars returns safe names
        local_vars = assigned_vars - set(safe_params)

        # Free-variable analysis: capture names referenced by the body that
        # come from an enclosing function's scope. The global scope (index 0)
        # is excluded because top-level names resolve through program-wide
        # symbol bindings rather than via captures.
        local_set = set(safe_params) | local_vars
        is_nested = len(self.scope_stack) > 1
        outer_env: Set[str] = set().union(*self.scope_stack[1:]) \
            if is_nested else set()
        # Single AST walk that returns both the body's free-variable
        # captures and the locals that must live in heap boxes
        # (captured by an inner closure AND mutated). Computed before
        # we push scope/box state so it sees only THIS function's
        # body shape.
        captures, boxed_names = self._analyze_body(
            node.body, local_set, outer_env)

        # Push a new scope for the function body
        # Include both params AND local vars so they're all treated as "already defined"
        # NOTE: Store SAFE names in scope_stack
        self.scope_stack.append(local_set)
        self.boxed_stack.append(boxed_names)
        # Loops don't cross function boundaries: a function defined
        # inside a loop has its own break/continue scope (which is
        # empty until the function itself contains a loop).
        saved_loop_depth = self._loop_depth
        self._loop_depth = 0

        try:
            # Compile body with granular expression storage (like Racket compiler)
            # Each statement is stored as a separate expression, then referenced in a begin form
            body_bytes = self.translate_function_body(
                node.body, local_vars, box_inits=boxed_names)

            # `is_function=True` marks the lambda as a Python function
            # boundary so the `return` primitive knows what to unwind to.
            lambda_bytes = self._emit_lambda(
                safe_params, body_bytes,
                is_function=True, captures=captures)

            # Top-level defs become global bindings via 'define'. Defs nested
            # inside another function bind into the enclosing function's
            # let-expansion local (collect_assigned_vars hoists the name),
            # so we emit 'set!' instead. set!'s mid-arg evaluation gives the
            # scheduler a chance to materialize a closure when the lambda
            # has free-variable captures.
            op = 'set' if is_nested else 'define'
            return create_inline_call(op, [func_name_symbol, lambda_bytes], self.bc)
        finally:
            # Pop the function scope
            self.scope_stack.pop()
            self.boxed_stack.pop()
            self._loop_depth = saved_loop_depth

    def translate_lambda(self, node: ast.Lambda) -> bytes:
        """
        Translate lambda expression.

        Lambda expressions in Python are true functions (like def), so we use
        bind_function to mark them as function boundaries for proper call semantics.

        Unlike def, lambda has no variable hoisting - it's a single expression.
        """
        params = [arg.arg for arg in node.args.args]
        safe_params = [self.get_safe_name(p) for p in params]

        # Free-variable + box analysis in one pass. Lambda bodies are
        # single expressions but can still close over enclosing
        # locals; their own params can still be captured + mutated by
        # an inner closure that uses nonlocal. _analyze_body expects a
        # statement list, so wrap the body in an Expr.
        local_set = set(safe_params)
        outer_env: Set[str] = set().union(*self.scope_stack[1:]) \
            if len(self.scope_stack) > 1 else set()
        captures, boxed_names = self._analyze_body(
            [ast.Expr(value=node.body)], local_set, outer_env)

        # Lambdas don't currently hoist locals, so the new scope is just the
        # parameters. Pushing it makes inner lambdas/defs see them as outer.
        self.scope_stack.append(local_set)
        self.boxed_stack.append(boxed_names)
        saved_loop_depth = self._loop_depth
        self._loop_depth = 0
        try:
            # Translate body directly - lambdas are single expressions.
            body_bytes = self.translate_expr(node.body)
            if boxed_names:
                # Prepend (set! p (box p)) wraps for each boxed param. We
                # build (begin wrap1 ... wrapN body) and use that as the
                # bind body.
                wraps = [self._make_box_init_bytes(name) for name in sorted(boxed_names)]
                body_bytes = create_inline_call('begin', wraps + [body_bytes], self.bc)

            # Lambdas are real functions, so is_function=True.
            return self._emit_lambda(
                safe_params, body_bytes,
                is_function=True, captures=captures)
        finally:
            self.scope_stack.pop()
            self.boxed_stack.pop()
            self._loop_depth = saved_loop_depth

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
            defaults = self._function_defaults.get(self.get_safe_name(callee_name))
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

    def translate_for(self, node: ast.For) -> bytes:
        """
        Translate for loop: for x in iterable: body -> (for-each (lambda (x) body) iterable).

        Supports tuple unpacking:
        for x, y in pairs: body -> (for-each (lambda (pair) (let ((x (car pair)) (y (cdr pair))) body)) pairs)

        Lambda body stored separately and referenced.
        """
        # Handle simple variable target
        if isinstance(node.target, ast.Name):
            param = node.target.id
            safe_param = self.get_safe_name(param)

            self._loop_depth += 1
            try:
                body_bytes = self.translate_block(node.body)
            finally:
                self._loop_depth -= 1

            # Wrap each iteration's body so `continue` returns from the
            # body lambda without escaping for_each.
            body_bytes = self._wrap_with_sentinel_guard(
                body_bytes, self.CONTINUE_SENTINEL)

            # Store body as separate expression
            body_ref = self._hoist(body_bytes)

            # Create lambda for loop body with body reference
            lambda_bytes = self._emit_lambda([safe_param], body_ref)

            # Translate iterable
            iterable_bytes = self.translate_expr_with_ref(node.iter)

            # (for-each lambda iterable)
            for_each_call = create_inline_call(
                'for_each', [lambda_bytes, iterable_bytes], self.bc)
            # Wrap the whole loop so `break` exits it without
            # propagating further.
            return self._wrap_with_sentinel_guard(
                for_each_call, self.BREAK_SENTINEL)

        # Handle tuple unpacking: for x, y in pairs
        elif isinstance(node.target, ast.Tuple):
            # Extract target variable names
            if not all(isinstance(elt, ast.Name) for elt in node.target.elts):
                raise NotImplementedError("For loop tuple unpacking only supports simple variable names")

            target_names = [self.get_safe_name(elt.id) for elt in node.target.elts]

            # Use nested lambda approach:
            # (for-each (lambda (item)
            #             ((lambda (x y) body) (list-ref item 0) (list-ref item 1)))
            #           iterable)
            # This ensures list-ref calls are evaluated before binding to lambda parameters

            # Create a temp parameter for the outer lambda (the list item)
            item_param = self.bc.get_unique_var_name("_item")

            # Translate the loop body
            self._loop_depth += 1
            try:
                body_bytes = self.translate_block(node.body)
            finally:
                self._loop_depth -= 1
            body_bytes = self._wrap_with_sentinel_guard(
                body_bytes, self.CONTINUE_SENTINEL)

            # Create inner lambda that takes unpacked variables as parameters
            # (lambda (x y) body)
            inner_lambda_ref = self._emit_lambda(target_names, body_bytes)

            # Create list-ref calls for each position
            # (list-ref item 0), (list-ref item 1), ...
            list_ref_calls = []
            for i in range(len(target_names)):
                index_bytes = encode_integer(i)
                list_ref_call = create_inline_call('list_ref',
                                                   [encode_symbol(item_param, self.bc), index_bytes],
                                                   self.bc)
                list_ref_calls.append(list_ref_call)

            # Call inner lambda with list-ref expressions
            # (inner_lambda (list-ref item 0) (list-ref item 1))
            inner_call = create_inline_call(inner_lambda_ref, list_ref_calls, self.bc)
            inner_call_ref = self._hoist(inner_call)

            # Create outer lambda: (lambda (item) inner_call_ref)
            outer_lambda_ref = self._emit_lambda([item_param], inner_call_ref)

            # Translate iterable
            iterable_bytes = self.translate_expr_with_ref(node.iter)

            # (for-each outer_lambda iterable)
            for_each_call = create_inline_call(
                'for_each', [outer_lambda_ref, iterable_bytes], self.bc)
            return self._wrap_with_sentinel_guard(
                for_each_call, self.BREAK_SENTINEL)

        else:
            raise NotImplementedError(f"For loop target type not supported: {type(node.target).__name__}")

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

        # Translate loop body (which may contain set! for variables)
        self._loop_depth += 1
        try:
            body_bytes = self.translate_block(node.body)
        finally:
            self._loop_depth -= 1

        # `continue` raises a sentinel that this guard swallows, so the
        # body lambda returns #f and the recursive call below fires the
        # next iteration.
        body_bytes = self._wrap_with_sentinel_guard(
            body_bytes, self.CONTINUE_SENTINEL)

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
