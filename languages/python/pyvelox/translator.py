"""
Python AST to VeloxVM Bytecode Translator

This module translates Python AST nodes to VeloxVM bytecode using
the encoding functions. It implements a visitor pattern to handle
different Python constructs.
"""

import ast
import functools
import sys
from typing import List, Optional, Set, Dict
from .bytecode import Bytecode
from .encoder import (
    encode_integer, encode_boolean, encode_string, encode_symbol,
    encode_inline_form, encode_form_ref, encode_form_lambda,
    create_inline_call, create_inline_call_direct, create_bind_form
)
from .errors import PyveloxCompileError
from .primitives import get_primitive_id


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


class PythonTranslator:
    """
    Translates Python AST to VeloxVM bytecode.

    Handles:
    - Literals (int, bool, str, list, dict)
    - Variables (assignment and reference)
    - Functions (def, lambda, calls)
    - Control flow (if, for, while)
    - Operators (arithmetic, comparison, logical, bitwise)
    - Exceptions (try/except/raise)
    - Built-in functions (print, len, etc.)
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

    def is_simple_expr(self, expr: ast.expr) -> bool:
        """
        Check if an expression is simple enough to inline.

        Simple expressions:
        - Literals (int, bool, str, None)
        - Variable references
        - Lambda expressions (they're already lambda forms, not expressions to evaluate)

        Complex expressions (should be stored separately):
        - Binary operations, function calls, lists, etc.
        """
        return isinstance(expr, (ast.Constant, ast.Name, ast.Lambda))

    def translate_expr_with_ref(self, expr: ast.expr) -> bytes:
        """
        Translate an expression and store it separately if it's complex.

        Returns either:
        - Inlined bytecode for simple expressions
        - Form reference for complex expressions
        """
        if self.is_simple_expr(expr):
            # Simple expression - inline it
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
        accumulated = bytearray()

        for stmt in module.body:
            bytecode = self.translate_stmt(stmt)
            accumulated.extend(bytecode)

        return bytes(accumulated)

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
        else:
            raise NotImplementedError(f"Expression type not supported: {type(expr).__name__}")

    # === Literals ===

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

    def translate_list(self, node: ast.List) -> bytes:
        """Translate a list literal: [1, 2, 3] -> (list 1 2 3). Complex elements stored separately."""
        elem_bytecode = [self.translate_expr_with_ref(e) for e in node.elts]
        return create_inline_call('list', elem_bytecode, self.bc)

    def translate_tuple(self, node: ast.Tuple) -> bytes:
        """
        Translate a tuple literal: (1, 2, 3) -> (list 1 2 3).
        Tuples are represented as lists in VeloxVM since both are immutable.
        Complex elements stored separately.
        """
        elem_bytecode = [self.translate_expr_with_ref(e) for e in node.elts]
        return create_inline_call('list', elem_bytecode, self.bc)

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
            pair_id = self.bc.add_expression(pair_bytes)
            pairs.append(encode_form_ref(pair_id))

        # Wrap in (list pair1 pair2 ...)
        return create_inline_call('list', pairs, self.bc)

    # === Variables ===

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
        result_id = self.bc.add_expression(result_bytes)
        result_ref = encode_form_ref(result_id)

        # (set! var result) or (box-set! var result) when boxed.
        write_op = 'box_set' if self._is_boxed(safe_name) else 'set'
        return create_inline_call(write_op,
                                  [encode_symbol(safe_name, self.bc), result_ref],
                                  self.bc)

    # === Functions ===

    def _compute_captures(self,
                          body_stmts: List[ast.stmt],
                          local_names: Set[str],
                          outer_env: Set[str]) -> List[str]:
        """
        Compute the captures for a function or lambda body.

        A captured name is one that is referenced (Load context) inside
        body_stmts -- including nested functions/lambdas whose own binders
        don't shadow it -- but is not bound by this function's params or
        local assignments, and is bound in some enclosing function (outer_env).

        Top-level globals are deliberately excluded from outer_env at the
        call site, so they resolve through the program-wide symbol bindings
        rather than being captured.

        Returns a deduplicated list of safe names in first-reference order.
        """
        free: List[str] = []
        seen: Set[str] = set()

        def walk(node, bound: Set[str]):
            if isinstance(node, ast.Name):
                if isinstance(node.ctx, ast.Load):
                    safe = self.get_safe_name(node.id)
                    if safe not in bound and safe not in seen:
                        seen.add(safe)
                        free.append(safe)
                return

            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                inner_params = {self.get_safe_name(a.arg) for a in node.args.args}
                inner_locals = self.collect_assigned_vars(node.body)
                inner_bound = bound | inner_params | inner_locals
                for stmt in node.body:
                    walk(stmt, inner_bound)
                return

            if isinstance(node, ast.Lambda):
                inner_params = {self.get_safe_name(a.arg) for a in node.args.args}
                walk(node.body, bound | inner_params)
                return

            for child in ast.iter_child_nodes(node):
                walk(child, bound)

        for stmt in body_stmts:
            walk(stmt, local_names)

        return [n for n in free if n in outer_env]

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

    def _params_captured_by_inner(self,
                                  body_stmts: List[ast.stmt],
                                  owned_names: Set[str]) -> Set[str]:
        """
        Names from owned_names that are referenced (Load) inside any nested
        function or lambda whose binders don't shadow them. These are the
        names that an inner closure would capture from this function.
        """
        hits: Set[str] = set()

        def walk(node, inside: bool, local: Set[str]):
            if isinstance(node, ast.Name):
                if isinstance(node.ctx, ast.Load):
                    safe = self.get_safe_name(node.id)
                    if inside and safe in owned_names and safe not in local:
                        hits.add(safe)
                return

            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                inner_params = {self.get_safe_name(a.arg) for a in node.args.args}
                inner_locals = self.collect_assigned_vars(node.body)
                inner_local = local | inner_params | inner_locals
                for stmt in node.body:
                    walk(stmt, True, inner_local)
                return

            if isinstance(node, ast.Lambda):
                inner_params = {self.get_safe_name(a.arg) for a in node.args.args}
                walk(node.body, True, local | inner_params)
                return

            for child in ast.iter_child_nodes(node):
                walk(child, inside, local)

        for stmt in body_stmts:
            walk(stmt, False, set())

        return hits

    def _params_mutated_in(self,
                           body_stmts: List[ast.stmt],
                           owned_names: Set[str]) -> Set[str]:
        """
        Names from owned_names that are the target of an assignment somewhere
        in body_stmts, where intermediate binders don't rebind the name. A
        name declared `nonlocal` inside a nested function does NOT rebind it
        (collect_assigned_vars subtracts nonlocal declarations), so writes
        through that nested function count as mutations of the outer name.
        """
        hits: Set[str] = set()

        def record_target(t, local: Set[str]):
            if isinstance(t, ast.Name):
                safe = self.get_safe_name(t.id)
                if safe in owned_names and safe not in local:
                    hits.add(safe)
            elif isinstance(t, ast.Tuple):
                for elt in t.elts:
                    record_target(elt, local)

        def walk(node, local: Set[str]):
            if isinstance(node, ast.Assign):
                for t in node.targets:
                    record_target(t, local)
                walk(node.value, local)
                return
            if isinstance(node, ast.AugAssign):
                record_target(node.target, local)
                walk(node.value, local)
                return

            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                inner_params = {self.get_safe_name(a.arg) for a in node.args.args}
                inner_locals = self.collect_assigned_vars(node.body)
                inner_local = local | inner_params | inner_locals
                for stmt in node.body:
                    walk(stmt, inner_local)
                return

            if isinstance(node, ast.Lambda):
                inner_params = {self.get_safe_name(a.arg) for a in node.args.args}
                walk(node.body, local | inner_params)
                return

            for child in ast.iter_child_nodes(node):
                walk(child, local)

        for stmt in body_stmts:
            walk(stmt, set())

        return hits

    def _params_needing_box(self,
                            body_stmts: List[ast.stmt],
                            owned_names: Set[str]) -> Set[str]:
        """
        Subset of owned_names that need to live in heap-allocated boxes:
        captured by an inner function AND mutated somewhere. A captured-only
        name is fine as a value capture; a mutated-but-not-captured name is
        fine as a plain local. Only the intersection has the alias-sharing
        problem (writes through one path must be visible through all).
        """
        captured = self._params_captured_by_inner(body_stmts, owned_names)
        mutated = self._params_mutated_in(body_stmts, owned_names)
        return captured & mutated

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
        captures = self._compute_captures(node.body, local_set, outer_env)

        # Box analysis: which params/locals must live in shared boxes because
        # they are both captured by an inner function AND mutated. Computed
        # before we push scope/box state so it sees only THIS function's
        # body shape.
        boxed_names = self._params_needing_box(node.body, local_set)

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

            # Don't store body as form ref - inline it directly in bind form
            # This matches what Racket does: (bind args body) where body is compiled directly
            # The body itself may contain form refs to individual statements or let-expansion
            body_for_bind = body_bytes

            # Create bind form: (bind_function param1 param2 ... body)
            # Use is_function=True to mark actual function boundaries for the return primitive
            bind_bytes = create_bind_form(safe_params, body_for_bind, self.bc, is_function=True)

            # Store bind in expression table
            expr_id = self.bc.add_expression(bind_bytes)

            # Record captured-symbol IDs for the runtime closure machinery.
            if captures:
                capture_ids = [self.bc.add_symbol(name) for name in captures]
                self.bc.record_captures(expr_id, capture_ids)

            # Create lambda reference
            lambda_bytes = encode_form_lambda(expr_id)

            # Top-level defs become global bindings via 'define'. Defs nested
            # inside another function bind into the enclosing function's
            # let-expansion local (collect_assigned_vars hoists the name),
            # so we emit 'set!' instead. set!'s mid-arg evaluation gives the
            # scheduler a chance to materialize a closure when the lambda
            # has free-variable captures.
            #
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

        # Free-variable analysis. Lambda bodies are single expressions but may
        # still close over enclosing-function locals (the most common case).
        local_set = set(safe_params)
        outer_env: Set[str] = set().union(*self.scope_stack[1:]) \
            if len(self.scope_stack) > 1 else set()
        # _compute_captures expects a list of statements; wrap the lambda body
        # in an Expr statement so the same analyzer applies.
        captures = self._compute_captures(
            [ast.Expr(value=node.body)], local_set, outer_env)

        # A lambda's body is a single expression, so the only assignments it
        # can contribute are inside nested functions/lambdas. Its own params
        # could still be captured + mutated by an inner closure that uses
        # nonlocal -- so the box analysis still applies.
        boxed_names = self._params_needing_box(
            [ast.Expr(value=node.body)], local_set)

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

            # Create bind_function form (lambdas are real functions, like def)
            bind_bytes = create_bind_form(safe_params, body_bytes, self.bc, is_function=True)

            # Store in expression table
            expr_id = self.bc.add_expression(bind_bytes)

            if captures:
                capture_ids = [self.bc.add_symbol(name) for name in captures]
                self.bc.record_captures(expr_id, capture_ids)

            # Return lambda reference
            return encode_form_lambda(expr_id)
        finally:
            self.scope_stack.pop()
            self.boxed_stack.pop()
            self._loop_depth = saved_loop_depth

    def translate_return(self, node: ast.Return) -> bytes:
        """
        Translate return statement using the VM's return primitive (now fixed!).

        The VM's return primitive has been fixed to distinguish between bind_function
        frames (actual functions) and regular bind frames (control flow like while loops).
        It now properly unwinds the stack back to the nearest bind_function frame.

        return value  ->  (return value)
        """
        if node.value:
            value_bytes = self.translate_expr_with_ref(node.value)
        else:
            value_bytes = encode_boolean(False)

        return create_inline_call('return', [value_bytes], self.bc)

    def translate_call(self, node: ast.Call) -> bytes:
        """Translate function call."""
        # Check for method calls (e.g., d.keys(), d.get(k))
        if isinstance(node.func, ast.Attribute):
            method_name = node.func.attr
            obj_expr = node.func.value

            # Dict methods
            if method_name == 'keys':
                return self.translate_dict_keys(obj_expr)
            elif method_name == 'values':
                return self.translate_dict_values(obj_expr)
            elif method_name == 'items':
                return self.translate_dict_items(obj_expr)
            elif method_name == 'get':
                return self.translate_dict_get(obj_expr, node.args)

            # List methods
            elif method_name == 'append':
                return self.translate_list_append(obj_expr, node.args)
            elif method_name == 'extend':
                return self.translate_list_extend(obj_expr, node.args)
            elif method_name == 'pop':
                return self.translate_list_pop(obj_expr, node.args)
            elif method_name == 'remove':
                return self.translate_list_remove(obj_expr, node.args)
            elif method_name == 'reverse':
                return self.translate_list_reverse(obj_expr)
            elif method_name == 'count':
                return self.translate_list_count(obj_expr, node.args)
            elif method_name == 'index':
                return self.translate_list_index(obj_expr, node.args)
            elif method_name == 'insert':
                return self.translate_list_insert(obj_expr, node.args)

            # String methods
            elif method_name == 'upper':
                return self.translate_string_upper(obj_expr)
            elif method_name == 'lower':
                return self.translate_string_lower(obj_expr)
            elif method_name == 'split':
                return self.translate_string_split(obj_expr, node.args)
            elif method_name == 'join':
                return self.translate_string_join(obj_expr, node.args)
            elif method_name == 'startswith':
                return self.translate_string_startswith(obj_expr, node.args)
            elif method_name == 'endswith':
                return self.translate_string_endswith(obj_expr, node.args)
            elif method_name == 'strip':
                return self.translate_string_strip(obj_expr)
            elif method_name == 'replace':
                return self.translate_string_replace(obj_expr, node.args)

            # Fall through for other methods

        # Check for built-in functions
        if isinstance(node.func, ast.Name):
            func_name = node.func.id

            # Handle special built-ins
            if func_name == 'print':
                return self.translate_print(node.args)
            elif func_name == 'len':
                return self.translate_len(node.args)
            elif func_name == 'range':
                return self.translate_range(node.args)
            elif func_name == 'int':
                return self.translate_int(node.args)
            elif func_name == 'str':
                return self.translate_str(node.args)
            elif func_name == 'abs':
                return self.translate_abs(node.args)
            elif func_name == 'min':
                return self.translate_min(node.args)
            elif func_name == 'max':
                return self.translate_max(node.args)
            elif func_name == 'sum':
                return self.translate_sum(node.args)
            elif func_name == 'sorted':
                return self.translate_sorted(node.args)
            elif func_name == 'reversed':
                return self.translate_reversed(node.args)
            elif func_name == 'map':
                return self.translate_map(node.args)
            elif func_name == 'filter':
                return self.translate_filter(node.args)
            elif func_name == 'reduce':
                return self.translate_reduce(node.args)
            elif func_name == 'all':
                return self.translate_all(node.args)
            elif func_name == 'any':
                return self.translate_any(node.args)
            elif func_name == 'list':
                return self.translate_list_constructor(node.args)
            elif func_name == 'enumerate':
                return self.translate_enumerate(node.args)
            elif func_name == 'zip':
                return self.translate_zip(node.args)

        # Regular function call. Resolve the callee carefully: when a
        # user binding shadows a primitive (tracked in renamed_vars by
        # get_safe_name), call the user's py_-prefixed function;
        # otherwise, if the name matches a primitive, emit the
        # primitive ID directly instead of going through the variable
        # path (which would rename thread_sleep -> py_thread_sleep and
        # produce an unbound app symbol).
        if isinstance(node.func, ast.Name):
            fname = node.func.id
            if fname in self.renamed_vars:
                func_bytes = encode_symbol(self.renamed_vars[fname], self.bc)
            elif self.is_vm_primitive(fname):
                func_bytes = encode_symbol(fname, self.bc)
            else:
                func_bytes = self.translate_expr(node.func)
        else:
            # Compound operator (e.g. ((f x) y), or (obj.method() ...)). Lift
            # it into a form-ref so the runtime sees a single token. Inlining
            # a nested call here would corrupt byte parsing -- the byte
            # loader advances past only the inline header, leaving the inner
            # body to be misread as the outer call's arguments. is_simple_expr
            # exempts ast.Lambda (a single lambda-form token), so a literal
            # ((lambda ...) ...) still inlines.
            func_bytes = self.translate_expr_with_ref(node.func)
        arg_bytes = [self.translate_expr_with_ref(arg) for arg in node.args]

        # Create inline call: inline(argc) + func + args
        result = bytearray()
        argc = 1 + len(arg_bytes)
        result.extend(encode_inline_form(argc))
        result.extend(func_bytes)
        for arg in arg_bytes:
            result.extend(arg)

        return bytes(result)

    # === Operators ===

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
            right_bytes = create_inline_call('subtract', [encode_integer(0), right_expr_bytes], self.bc)
            # Store the negation separately
            negate_id = self.bc.add_expression(right_bytes)
            right_bytes = encode_form_ref(negate_id)
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
        eq_id = self.bc.add_expression(eq_call)
        return create_inline_call('not', [encode_form_ref(eq_id)], self.bc)

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
                    assoc_id = self.bc.add_expression(assoc_call)
                    assoc_ref = encode_form_ref(assoc_id)
                    return create_inline_call('if', [assoc_ref, encode_boolean(True), encode_boolean(False)], self.bc)
                else:
                    # x in container -> (if (member x container) #t #f)
                    # Note: For dict variables, use d.get(key) is not None instead
                    member_call = create_inline_call('member', [left_bytes, right_bytes], self.bc)
                    member_id = self.bc.add_expression(member_call)
                    member_ref = encode_form_ref(member_id)
                    return create_inline_call('if', [member_ref, encode_boolean(True), encode_boolean(False)], self.bc)

            elif op_type == ast.NotIn:
                left_bytes = self.translate_expr_with_ref(node.left)
                right_bytes = self.translate_expr_with_ref(node.comparators[0])

                # If comparator is a Dict literal, use assoc
                is_dict_literal = isinstance(node.comparators[0], ast.Dict)

                if is_dict_literal:
                    # key not in dict-literal -> (not (assoc key dict))
                    assoc_call = create_inline_call('assoc', [left_bytes, right_bytes], self.bc)
                    assoc_id = self.bc.add_expression(assoc_call)
                    assoc_ref = encode_form_ref(assoc_id)
                    return create_inline_call('not', [assoc_ref], self.bc)
                else:
                    # x not in container -> (not (member x container))
                    member_call = create_inline_call('member', [left_bytes, right_bytes], self.bc)
                    member_id = self.bc.add_expression(member_call)
                    member_ref = encode_form_ref(member_id)
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
            # Chained comparison
            comparisons = []
            left = node.left

            for op_node, right in zip(node.ops, node.comparators):
                op_type = type(op_node)
                left_bytes = self.translate_expr_with_ref(left)
                right_bytes = self.translate_expr_with_ref(right)

                if op_type is ast.Eq:
                    comp_bytes = self._emit_eq(left_bytes, right_bytes, negate=False)
                elif op_type is ast.NotEq:
                    comp_bytes = self._emit_eq(left_bytes, right_bytes, negate=True)
                else:
                    op = op_map.get(op_type)
                    if not op:
                        raise NotImplementedError(f"Comparison operator not supported: {op_type.__name__}")
                    comp_bytes = create_inline_call(op, [left_bytes, right_bytes], self.bc)

                # Store each comparison separately
                comp_id = self.bc.add_expression(comp_bytes)
                comparisons.append(encode_form_ref(comp_id))
                left = right

            # (and comp1 comp2 ...)
            return create_inline_call('and', comparisons, self.bc)

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

    # === Control Flow ===

    def translate_if_stmt(self, node: ast.If) -> bytes:
        """
        Translate if statement: if test: body else: alt -> (if test body alt).
        Test, body, and alt are all stored separately.
        """
        test_bytes = self.translate_expr_with_ref(node.test)
        body_bytes = self.translate_block(node.body)

        # Store body as separate expression
        body_id = self.bc.add_expression(body_bytes)
        body_ref = encode_form_ref(body_id)

        if node.orelse:
            alt_bytes = self.translate_block(node.orelse)
            # Store alt as separate expression
            alt_id = self.bc.add_expression(alt_bytes)
            alt_ref = encode_form_ref(alt_id)
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
        eq_id = self.bc.add_expression(eq_check)
        eq_ref = encode_form_ref(eq_id)

        rethrow = create_inline_call(
            'raise', [encode_symbol(exc_name, self.bc)], self.bc)
        rethrow_id = self.bc.add_expression(rethrow)
        rethrow_ref = encode_form_ref(rethrow_id)

        handler = create_inline_call(
            'if', [eq_ref, encode_boolean(False), rethrow_ref], self.bc)
        handler_id = self.bc.add_expression(handler)
        handler_ref = encode_form_ref(handler_id)

        body_id = self.bc.add_expression(body_bytes)
        body_ref = encode_form_ref(body_id)

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
            body_id = self.bc.add_expression(body_bytes)
            body_ref = encode_form_ref(body_id)

            # Create lambda for loop body with body reference
            bind_bytes = create_bind_form([safe_param], body_ref, self.bc)
            lambda_id = self.bc.add_expression(bind_bytes)
            lambda_bytes = encode_form_lambda(lambda_id)

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
            inner_lambda_bytes = create_bind_form(target_names, body_bytes, self.bc)
            inner_lambda_id = self.bc.add_expression(inner_lambda_bytes)
            inner_lambda_ref = encode_form_lambda(inner_lambda_id)

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
            inner_call = create_inline_call_direct(inner_lambda_ref, list_ref_calls, self.bc)
            inner_call_id = self.bc.add_expression(inner_call)
            inner_call_ref = encode_form_ref(inner_call_id)

            # Create outer lambda: (lambda (item) inner_call_ref)
            outer_lambda_bytes = create_bind_form([item_param], inner_call_ref, self.bc)
            outer_lambda_id = self.bc.add_expression(outer_lambda_bytes)
            outer_lambda_ref = encode_form_lambda(outer_lambda_id)

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
        lambda_bind = create_bind_form([], if_bytes, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        # Define the loop function: (define loop (lambda () ...))
        define_bytes = create_inline_call('define', [encode_symbol(loop_name, self.bc), lambda_bytes], self.bc)

        # Initial call with NO arguments: (loop)
        call_bytes = create_inline_call(loop_name, [], self.bc)

        # Wrap the define and call in a BEGIN form
        # because bind form expects ONE expression, and we have two (define + call)
        # Pattern: ((lambda () (begin (define loop ...) (loop))))
        wrapper_body_begin = create_inline_call('begin', [define_bytes, call_bytes], self.bc)

        # Create wrapper bind with the begin form as body
        wrapper_bind = create_bind_form([], wrapper_body_begin, self.bc)
        wrapper_id = self.bc.add_expression(wrapper_bind)
        wrapper_lambda = encode_form_lambda(wrapper_id)

        # Call the wrapper lambda: ((lambda () (define loop ...) (loop)))
        loop_call = create_inline_call_direct(wrapper_lambda, [], self.bc)
        # Wrap the entire while construct so `break` exits cleanly.
        return self._wrap_with_sentinel_guard(
            loop_call, self.BREAK_SENTINEL)

    # === Exceptions ===

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
        body_id = self.bc.add_expression(body_bytes)
        body_ref = encode_form_ref(body_id)

        handler_id = self.bc.add_expression(handler_bytes)
        handler_ref = encode_form_ref(handler_id)

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
        is_break_id = self.bc.add_expression(is_break)
        is_break_ref = encode_form_ref(is_break_id)

        is_cont = create_inline_call(
            'eqp', [encode_symbol(exc_var, self.bc), cont_quoted], self.bc)
        is_cont_id = self.bc.add_expression(is_cont)
        is_cont_ref = encode_form_ref(is_cont_id)

        is_sentinel = create_inline_call(
            'or', [is_break_ref, is_cont_ref], self.bc)
        is_sentinel_id = self.bc.add_expression(is_sentinel)
        is_sentinel_ref = encode_form_ref(is_sentinel_id)

        rethrow = create_inline_call(
            'raise', [encode_symbol(exc_var, self.bc)], self.bc)
        rethrow_id = self.bc.add_expression(rethrow)
        rethrow_ref = encode_form_ref(rethrow_id)

        handler_id = self.bc.add_expression(handler_bytes)
        handler_ref = encode_form_ref(handler_id)

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

    # === Subscripting ===

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
            assoc_id = self.bc.add_expression(assoc_bytes)
            assoc_ref = encode_form_ref(assoc_id)
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
                length_id = self.bc.add_expression(length_call)
                length_ref = encode_form_ref(length_id)

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

    # === Attribute Access ===

    def translate_attribute(self, node: ast.Attribute) -> bytes:
        """
        Translate attribute access: obj.attr
        This is used for accessing attributes that are not method calls.
        For method calls, translate_call handles them.
        """
        raise NotImplementedError(f"Attribute access '{node.attr}' not supported in this context")

    # === Dict Methods ===

    def translate_dict_keys(self, dict_expr: ast.expr) -> bytes:
        """
        Translate d.keys() -> (map car d)
        Extract all keys (first elements of pairs).
        """
        dict_bytes = self.translate_expr_with_ref(dict_expr)

        # Create lambda (p) (car p) to extract keys
        car_call = create_inline_call('car', [encode_symbol('p', self.bc)], self.bc)
        lambda_bind = create_bind_form(['p'], car_call, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        # (map (lambda (p) (car p)) dict)
        return create_inline_call('map', [lambda_bytes, dict_bytes], self.bc)

    def translate_dict_values(self, dict_expr: ast.expr) -> bytes:
        """
        Translate d.values() -> (map cdr d)
        Extract all values (second elements of pairs).
        """
        dict_bytes = self.translate_expr_with_ref(dict_expr)

        # Create lambda (p) (cdr p) to extract values
        cdr_call = create_inline_call('cdr', [encode_symbol('p', self.bc)], self.bc)
        lambda_bind = create_bind_form(['p'], cdr_call, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        # (map (lambda (p) (cdr p)) dict)
        return create_inline_call('map', [lambda_bytes, dict_bytes], self.bc)

    def translate_dict_items(self, dict_expr: ast.expr) -> bytes:
        """
        Translate d.items() -> d
        The dict is already a list of pairs, which is what items() returns.
        """
        return self.translate_expr_with_ref(dict_expr)

    def translate_dict_get(self, dict_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate d.get(key) or d.get(key, default)
        -> (let ((pair (assoc key d)))
             (if pair (cdr pair) default))

        If no default provided, use #f (False).
        """
        if len(args) < 1 or len(args) > 2:
            raise ValueError("dict.get() takes 1 or 2 arguments")

        dict_bytes = self.translate_expr_with_ref(dict_expr)
        key_bytes = self.translate_expr_with_ref(args[0])

        if len(args) == 2:
            default_bytes = self.translate_expr_with_ref(args[1])
        else:
            default_bytes = encode_boolean(False)

        # (assoc key dict)
        assoc_bytes = create_inline_call('assoc', [key_bytes, dict_bytes], self.bc)
        assoc_id = self.bc.add_expression(assoc_bytes)
        assoc_ref = encode_form_ref(assoc_id)

        # (cdr pair) for the true branch
        cdr_bytes = create_inline_call('cdr', [assoc_ref], self.bc)
        cdr_id = self.bc.add_expression(cdr_bytes)
        cdr_ref = encode_form_ref(cdr_id)

        # (if pair (cdr pair) default)
        return create_inline_call('if', [assoc_ref, cdr_ref, default_bytes], self.bc)

    def translate_dict_subscript_assign(self, target: ast.Subscript, value_expr: ast.expr) -> bytes:
        """
        Translate dict subscript assignment: d['key'] = val

        Since dicts are immutable (association lists), we create a new dict:
        (set! d (cons (cons 'key val) (filter (lambda (p) (not (equal (car p) 'key))) d)))

        This removes any existing pair with the same key, then adds the new pair.
        """
        # Only support simple variable dict targets (not complex expressions)
        if not isinstance(target.value, ast.Name):
            raise NotImplementedError("Dict subscript assignment only supports simple variable targets")

        dict_name = target.value.id
        safe_dict_name = self.get_safe_name(dict_name)

        # Translate key and value
        key_bytes = self.translate_expr_with_ref(target.slice)
        value_bytes = self.translate_expr_with_ref(value_expr)

        # Create (cons 'key val)
        new_pair_bytes = create_inline_call('cons', [key_bytes, value_bytes], self.bc)
        new_pair_id = self.bc.add_expression(new_pair_bytes)
        new_pair_ref = encode_form_ref(new_pair_id)

        # Create filter lambda: (lambda (p) (not (equal (car p) 'key)))
        # This filters out any existing pair with the same key

        # (car p) - extract key from pair
        car_p_bytes = create_inline_call('car', [encode_symbol('p', self.bc)], self.bc)
        car_p_id = self.bc.add_expression(car_p_bytes)
        car_p_ref = encode_form_ref(car_p_id)

        # (equal (car p) 'key)
        equal_bytes = create_inline_call('equalp', [car_p_ref, key_bytes], self.bc)
        equal_id = self.bc.add_expression(equal_bytes)
        equal_ref = encode_form_ref(equal_id)

        # (not (equal ...))
        not_bytes = create_inline_call('not', [equal_ref], self.bc)

        # (lambda (p) (not (equal (car p) 'key)))
        filter_lambda_bind = create_bind_form(['p'], not_bytes, self.bc)
        filter_lambda_id = self.bc.add_expression(filter_lambda_bind)
        filter_lambda_bytes = encode_form_lambda(filter_lambda_id)

        # (filter lambda d)
        dict_ref = encode_symbol(safe_dict_name, self.bc)
        filter_bytes = create_inline_call('filter', [filter_lambda_bytes, dict_ref], self.bc)
        filter_id = self.bc.add_expression(filter_bytes)
        filter_ref = encode_form_ref(filter_id)

        # (cons new_pair filtered_dict)
        new_dict_bytes = create_inline_call('cons', [new_pair_ref, filter_ref], self.bc)
        new_dict_id = self.bc.add_expression(new_dict_bytes)
        new_dict_ref = encode_form_ref(new_dict_id)

        # (set! d new_dict)
        return create_inline_call('set', [dict_ref, new_dict_ref], self.bc)

    # === List Methods ===

    def translate_list_append(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.append(x) -> (set! lst (append lst (list x)))

        Note: Python's append mutates in-place, but VeloxVM lists are immutable.
        We use set! to reassign the variable.
        """
        if len(args) != 1:
            raise ValueError("list.append() takes exactly 1 argument")

        # Only support simple variable list targets
        if not isinstance(list_expr, ast.Name):
            raise NotImplementedError("List append only supports simple variable targets")

        list_name = list_expr.id
        safe_list_name = self.get_safe_name(list_name)

        value_bytes = self.translate_expr_with_ref(args[0])

        # Create (list value)
        list_wrapper = create_inline_call('list', [value_bytes], self.bc)
        list_wrapper_id = self.bc.add_expression(list_wrapper)
        list_wrapper_ref = encode_form_ref(list_wrapper_id)

        # (append lst (list value))
        list_ref = encode_symbol(safe_list_name, self.bc)
        append_bytes = create_inline_call('append', [list_ref, list_wrapper_ref], self.bc)
        append_id = self.bc.add_expression(append_bytes)
        append_ref = encode_form_ref(append_id)

        # (set! lst (append ...))
        return create_inline_call('set', [list_ref, append_ref], self.bc)

    def translate_list_extend(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.extend(other) -> (set! lst (append lst other))
        """
        if len(args) != 1:
            raise ValueError("list.extend() takes exactly 1 argument")

        if not isinstance(list_expr, ast.Name):
            raise NotImplementedError("List extend only supports simple variable targets")

        list_name = list_expr.id
        safe_list_name = self.get_safe_name(list_name)

        other_bytes = self.translate_expr_with_ref(args[0])

        # (append lst other)
        list_ref = encode_symbol(safe_list_name, self.bc)
        append_bytes = create_inline_call('append', [list_ref, other_bytes], self.bc)
        append_id = self.bc.add_expression(append_bytes)
        append_ref = encode_form_ref(append_id)

        # (set! lst (append ...))
        return create_inline_call('set', [list_ref, append_ref], self.bc)

    def translate_list_pop(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.pop() -> (pop lst)

        Note: VeloxVM's pop returns the last element but doesn't modify the list.
        For mutation, we'd need: (let ((val (pop lst))) (set! lst (reverse (cdr (reverse lst)))) val)
        For now, just return the value without mutation.
        """
        if len(args) > 1:
            raise ValueError("list.pop() takes at most 1 argument")

        if len(args) == 1:
            raise NotImplementedError("list.pop(index) not yet supported, use pop() without args")

        list_bytes = self.translate_expr_with_ref(list_expr)

        # Simple version: just return last element (no mutation)
        return create_inline_call('pop', [list_bytes], self.bc)

    def translate_list_remove(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.remove(x) -> (set! lst (remove x lst))
        """
        if len(args) != 1:
            raise ValueError("list.remove() takes exactly 1 argument")

        if not isinstance(list_expr, ast.Name):
            raise NotImplementedError("List remove only supports simple variable targets")

        list_name = list_expr.id
        safe_list_name = self.get_safe_name(list_name)

        value_bytes = self.translate_expr_with_ref(args[0])

        # (remove value lst)
        list_ref = encode_symbol(safe_list_name, self.bc)
        remove_bytes = create_inline_call('remove', [value_bytes, list_ref], self.bc)
        remove_id = self.bc.add_expression(remove_bytes)
        remove_ref = encode_form_ref(remove_id)

        # (set! lst (remove ...))
        return create_inline_call('set', [list_ref, remove_ref], self.bc)

    def translate_list_reverse(self, list_expr: ast.expr) -> bytes:
        """
        Translate lst.reverse() -> (set! lst (reverse lst))
        """
        if not isinstance(list_expr, ast.Name):
            raise NotImplementedError("List reverse only supports simple variable targets")

        list_name = list_expr.id
        safe_list_name = self.get_safe_name(list_name)

        # (reverse lst)
        list_ref = encode_symbol(safe_list_name, self.bc)
        reverse_bytes = create_inline_call('reverse', [list_ref], self.bc)
        reverse_id = self.bc.add_expression(reverse_bytes)
        reverse_ref = encode_form_ref(reverse_id)

        # (set! lst (reverse ...))
        return create_inline_call('set', [list_ref, reverse_ref], self.bc)

    def translate_list_count(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.count(x) -> (count (lambda (e) (equal e x)) lst)
        """
        if len(args) != 1:
            raise ValueError("list.count() takes exactly 1 argument")

        list_bytes = self.translate_expr_with_ref(list_expr)
        value_bytes = self.translate_expr_with_ref(args[0])

        # Create lambda (e) (equal e value)
        equal_call = create_inline_call('equalp', [encode_symbol('e', self.bc), value_bytes], self.bc)
        lambda_bind = create_bind_form(['e'], equal_call, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        # (count lambda lst)
        return create_inline_call('count', [lambda_bytes, list_bytes], self.bc)

    def translate_list_index(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.index(x) -> (list-index x lst)

        Uses the new list_index VM primitive.
        Returns the index of the first occurrence, or -1 if not found.
        """
        if len(args) != 1:
            raise ValueError("list.index() takes exactly 1 argument")

        elem_bytes = self.translate_expr_with_ref(args[0])
        list_bytes = self.translate_expr_with_ref(list_expr)

        return create_inline_call('list_index', [elem_bytes, list_bytes], self.bc)

    def translate_list_insert(self, list_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate lst.insert(index, value).

        A clean Scheme expression would be
            (set! lst (append (take i lst) (cons x (drop i lst))))
        but the VM does not currently expose take/drop, so the operation is
        not yet implemented.
        """
        if len(args) != 2:
            raise ValueError("list.insert() takes exactly 2 arguments (index, value)")

        raise NotImplementedError(
            "list.insert() requires splitting the list at the insertion "
            "point (take/drop), which is not yet implemented."
        )

    # === String Methods ===

    def translate_string_upper(self, str_expr: ast.expr) -> bytes:
        """
        Translate s.upper() -> (list-to-string (map char-upcase (string-to-list s)))
        """
        str_bytes = self.translate_expr_with_ref(str_expr)

        # (string-to-list s)
        to_list_bytes = create_inline_call('string_to_list', [str_bytes], self.bc)
        to_list_id = self.bc.add_expression(to_list_bytes)
        to_list_ref = encode_form_ref(to_list_id)

        # Create lambda (c) (char-upcase c) - actually, map can take primitive directly
        # (map char-upcase (string-to-list s))
        char_upcase_symbol = encode_symbol('char_upcase', self.bc)
        map_bytes = create_inline_call('map', [char_upcase_symbol, to_list_ref], self.bc)
        map_id = self.bc.add_expression(map_bytes)
        map_ref = encode_form_ref(map_id)

        # (list-to-string (map ...))
        return create_inline_call('list_to_string', [map_ref], self.bc)

    def translate_string_lower(self, str_expr: ast.expr) -> bytes:
        """
        Translate s.lower() -> (list-to-string (map char-downcase (string-to-list s)))
        """
        str_bytes = self.translate_expr_with_ref(str_expr)

        # (string-to-list s)
        to_list_bytes = create_inline_call('string_to_list', [str_bytes], self.bc)
        to_list_id = self.bc.add_expression(to_list_bytes)
        to_list_ref = encode_form_ref(to_list_id)

        # (map char-downcase (string-to-list s))
        char_downcase_symbol = encode_symbol('char_downcase', self.bc)
        map_bytes = create_inline_call('map', [char_downcase_symbol, to_list_ref], self.bc)
        map_id = self.bc.add_expression(map_bytes)
        map_ref = encode_form_ref(map_id)

        # (list-to-string (map ...))
        return create_inline_call('list_to_string', [map_ref], self.bc)

    def translate_string_split(self, str_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate s.split(delim) -> (string-split s delim)

        VeloxVM has native string-split!
        """
        if len(args) != 1:
            raise ValueError("str.split() requires exactly 1 argument (delimiter)")

        str_bytes = self.translate_expr_with_ref(str_expr)
        delim_bytes = self.translate_expr_with_ref(args[0])

        # (string-split s delim)
        return create_inline_call('string_split', [str_bytes, delim_bytes], self.bc)

    def translate_string_join(self, str_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate sep.join(lst) -> (string-join sep lst)

        Uses the new string_join VM primitive.
        """
        if len(args) != 1:
            raise ValueError("str.join() takes exactly 1 argument")

        sep_bytes = self.translate_expr_with_ref(str_expr)
        list_bytes = self.translate_expr_with_ref(args[0])

        return create_inline_call('string_join', [sep_bytes, list_bytes], self.bc)

    def translate_string_startswith(self, str_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate s.startswith(prefix) -> compare substring

        (equal (substring s 0 (string-length prefix)) prefix)
        """
        if len(args) != 1:
            raise ValueError("str.startswith() takes exactly 1 argument")

        str_bytes = self.translate_expr_with_ref(str_expr)
        prefix_bytes = self.translate_expr_with_ref(args[0])

        # (string-length prefix)
        prefix_len_bytes = create_inline_call('string_length', [prefix_bytes], self.bc)
        prefix_len_id = self.bc.add_expression(prefix_len_bytes)
        prefix_len_ref = encode_form_ref(prefix_len_id)

        # (substring s 0 (string-length prefix))
        substring_bytes = create_inline_call('substring', [
            str_bytes,
            encode_integer(0),
            prefix_len_ref
        ], self.bc)
        substring_id = self.bc.add_expression(substring_bytes)
        substring_ref = encode_form_ref(substring_id)

        # (equal substring prefix)
        return create_inline_call('equalp', [substring_ref, prefix_bytes], self.bc)

    def translate_string_endswith(self, str_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate s.endswith(suffix) -> compare substring from end

        (let ((slen (string-length s))
              (sufflen (string-length suffix)))
          (equal (substring s (- slen sufflen) slen) suffix))

        Simplified: (equal (substring s (- (string-length s) (string-length suffix)) (string-length s)) suffix)
        """
        if len(args) != 1:
            raise ValueError("str.endswith() takes exactly 1 argument")

        str_bytes = self.translate_expr_with_ref(str_expr)
        suffix_bytes = self.translate_expr_with_ref(args[0])

        # (string-length s)
        str_len_bytes = create_inline_call('string_length', [str_bytes], self.bc)
        str_len_id = self.bc.add_expression(str_len_bytes)
        str_len_ref = encode_form_ref(str_len_id)

        # (string-length suffix)
        suffix_len_bytes = create_inline_call('string_length', [suffix_bytes], self.bc)
        suffix_len_id = self.bc.add_expression(suffix_len_bytes)
        suffix_len_ref = encode_form_ref(suffix_len_id)

        # (- (string-length s) (string-length suffix))
        start_pos_bytes = create_inline_call('subtract', [str_len_ref, suffix_len_ref], self.bc)
        start_pos_id = self.bc.add_expression(start_pos_bytes)
        start_pos_ref = encode_form_ref(start_pos_id)

        # (substring s start_pos str_len)
        substring_bytes = create_inline_call('substring', [str_bytes, start_pos_ref, str_len_ref], self.bc)
        substring_id = self.bc.add_expression(substring_bytes)
        substring_ref = encode_form_ref(substring_id)

        # (equal substring suffix)
        return create_inline_call('equalp', [substring_ref, suffix_bytes], self.bc)

    def translate_string_strip(self, str_expr: ast.expr) -> bytes:
        """
        Translate s.strip() -> remove whitespace from both ends.

        Not yet implemented; doing this cleanly needs character-class-aware
        end trimming, for which the VM does not expose the right primitive.
        """
        raise NotImplementedError(
            "str.strip() requires trimming whitespace from both ends, "
            "which is not yet implemented."
        )

    def translate_string_replace(self, str_expr: ast.expr, args: List[ast.expr]) -> bytes:
        """
        Translate s.replace(old, new).

        The natural lowering is (string-join new (string-split s old)),
        but str.join() is not yet working correctly, so this is
        currently unimplemented.
        """
        if len(args) != 2:
            raise ValueError("str.replace() takes exactly 2 arguments (old, new)")

        raise NotImplementedError(
            "str.replace() depends on str.join(), which is not yet working."
        )

    # === Built-in Functions ===

    def translate_print(self, args: List[ast.expr]) -> bytes:
        """
        Translate print(a, b, c) -> (begin (print a b c) (write-char #\newline)).

        Python's print adds a newline, but VeloxVM's print primitive doesn't,
        so we need to add a newline character explicitly.

        For print() with no arguments, just write a newline.
        """
        # Handle empty print() - just write newline
        if len(args) == 0:
            from .encoder import encode_character
            newline_char = encode_character('\n')
            return create_inline_call('write_char', [newline_char], self.bc)

        arg_bytes = [self.translate_expr_with_ref(arg) for arg in args]
        print_call = create_inline_call('print', arg_bytes, self.bc)

        # Store print call as separate expression
        print_id = self.bc.add_expression(print_call)
        print_ref = encode_form_ref(print_id)

        # Add newline: (write-char #\newline)
        from .encoder import encode_character
        newline_char = encode_character('\n')
        newline_call = create_inline_call('write_char', [newline_char], self.bc)

        # Store newline call as separate expression
        newline_id = self.bc.add_expression(newline_call)
        newline_ref = encode_form_ref(newline_id)

        # Combine with begin: (begin (print ...) (write-char #\newline))
        return create_inline_call('begin', [print_ref, newline_ref], self.bc)

    def translate_len(self, args: List[ast.expr]) -> bytes:
        """Translate len(x) -> (length x) or (string-length x)."""
        if len(args) != 1:
            raise ValueError("len() takes exactly 1 argument")

        arg_bytes = self.translate_expr_with_ref(args[0])

        # For simplicity, use 'length' (works for lists)
        # TODO: Could inspect type to choose between length/string-length/vector-length
        return create_inline_call('length', [arg_bytes], self.bc)

    def translate_range(self, args: List[ast.expr]) -> bytes:
        """
        Translate range(n) to a list: (list 0 1 2 ... n-1).

        For simplicity, only support constant ranges for now.
        """
        if len(args) != 1:
            raise NotImplementedError("range() only supports single argument for now")

        # Evaluate to get constant value (simplified)
        if isinstance(args[0], ast.Constant):
            n = args[0].value
            if not isinstance(n, int):
                raise ValueError("range() argument must be an integer")

            # Generate (list 0 1 2 ... n-1)
            elements = [encode_integer(i) for i in range(n)]
            return create_inline_call('list', elements, self.bc)
        else:
            raise NotImplementedError("range() only supports constant arguments for now")

    def translate_int(self, args: List[ast.expr]) -> bytes:
        """Translate int(x) -> (string->number x) or just x."""
        if len(args) != 1:
            raise ValueError("int() takes exactly 1 argument")

        arg_bytes = self.translate_expr_with_ref(args[0])

        # If it's already a number, return it; otherwise convert
        # For simplicity, use string->number
        return create_inline_call('string_to_number', [arg_bytes], self.bc)

    def translate_str(self, args: List[ast.expr]) -> bytes:
        """Translate `str(x)`.

        Python `str(x)` accepts any type; the VM has no single
        `to_string` primitive, so we either pick the right conversion
        at compile time (for literal arguments) or emit a small
        runtime type-dispatch:

            (if (stringp x) x
              (if (booleanp x) (if x "True" "False")
                (number-to-string x)))

        The fallback branch errors at runtime for unsupported types
        (e.g. lists), matching pre-existing behaviour. None is encoded
        as `False` in PyVelox, so `str(None)` yields "False" — a
        known divergence noted in tests/python-tests/README.md.
        """
        if len(args) != 1:
            raise ValueError("str() takes exactly 1 argument")

        arg = args[0]

        # Compile-time fast path: the source already tells us the type.
        if isinstance(arg, ast.Constant):
            value = arg.value
            if isinstance(value, str):
                return encode_string(value, self.bc)
            # bool is a subclass of int; check it before the int branch.
            if isinstance(value, bool):
                return encode_string("True" if value else "False", self.bc)
            if value is None:
                return encode_string("None", self.bc)
            if isinstance(value, int):
                return create_inline_call(
                    'number_to_string',
                    [self.translate_expr_with_ref(arg)],
                    self.bc)
            # Other constant types fall through to the runtime path.

        arg_bytes = self.translate_expr_with_ref(arg)

        def _hoist(call_bytes: bytes) -> bytes:
            return encode_form_ref(self.bc.add_expression(call_bytes))

        # Inner branch for booleans: (if x "True" "False")
        bool_branch_ref = _hoist(create_inline_call(
            'if',
            [arg_bytes,
             encode_string("True", self.bc),
             encode_string("False", self.bc)],
            self.bc))

        # Numeric/fallback branch: (number-to-string x)
        num_branch_ref = _hoist(create_inline_call(
            'number_to_string', [arg_bytes], self.bc))

        # (booleanp x)
        bool_test_ref = _hoist(create_inline_call(
            'booleanp', [arg_bytes], self.bc))

        # (if (booleanp x) bool_branch num_branch)
        inner_if_ref = _hoist(create_inline_call(
            'if', [bool_test_ref, bool_branch_ref, num_branch_ref], self.bc))

        # (stringp x)
        str_test_ref = _hoist(create_inline_call(
            'stringp', [arg_bytes], self.bc))

        # (if (stringp x) x inner_if)
        return create_inline_call(
            'if', [str_test_ref, arg_bytes, inner_if_ref], self.bc)

    def translate_abs(self, args: List[ast.expr]) -> bytes:
        """Translate abs(x) -> (if (< x 0) (- 0 x) x)."""
        if len(args) != 1:
            raise ValueError("abs() takes exactly 1 argument")

        arg_bytes = self.translate_expr_with_ref(args[0])

        # Create comparison: (< x 0)
        zero_bytes = encode_integer(0)
        cmp_bytes = create_inline_call('less_than', [arg_bytes, zero_bytes], self.bc)
        cmp_id = self.bc.add_expression(cmp_bytes)
        cmp_ref = encode_form_ref(cmp_id)

        # Create negation: (- 0 x)
        neg_bytes = create_inline_call('subtract', [zero_bytes, arg_bytes], self.bc)
        neg_id = self.bc.add_expression(neg_bytes)
        neg_ref = encode_form_ref(neg_id)

        # (if (< x 0) (- 0 x) x)
        return create_inline_call('if', [cmp_ref, neg_ref, arg_bytes], self.bc)

    def _translate_fold_min_max(self, iterable: ast.expr, cmp_op: str) -> bytes:
        """Compile min(iterable)/max(iterable) as
        (reduce (lambda (a b) (if (cmp a b) a b)) iterable).
        cmp_op is 'less_than' for min, 'greater_than' for max."""
        list_bytes = self.translate_expr_with_ref(iterable)

        a_sym = encode_symbol('a', self.bc)
        b_sym = encode_symbol('b', self.bc)

        cmp_call = create_inline_call(cmp_op, [a_sym, b_sym], self.bc)
        cmp_id = self.bc.add_expression(cmp_call)
        cmp_ref = encode_form_ref(cmp_id)

        if_form = create_inline_call('if', [cmp_ref, a_sym, b_sym], self.bc)
        lambda_bind = create_bind_form(['a', 'b'], if_form, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        return create_inline_call('reduce', [lambda_bytes, list_bytes], self.bc)

    def translate_min(self, args: List[ast.expr]) -> bytes:
        """
        Translate min(a, b, ...) to nested if comparisons.
        min(a, b) -> (if (< a b) a b)
        min(a, b, c) -> (if (< a b) (if (< a c) a c) (if (< b c) b c))
        min(iterable) -> (reduce (lambda (a b) (if (< a b) a b)) iterable)
        """
        if len(args) == 0:
            raise ValueError("min() requires at least 1 argument")

        if len(args) == 1:
            # Python min(iterable): fold with a two-arg min lambda.
            return self._translate_fold_min_max(args[0], 'less_than')

        # For 2+ arguments, recursively build min comparisons
        arg_bytes = [self.translate_expr_with_ref(arg) for arg in args]

        def build_min(items):
            if len(items) == 1:
                return items[0]
            elif len(items) == 2:
                # (if (< a b) a b)
                cmp = create_inline_call('less_than', [items[0], items[1]], self.bc)
                cmp_id = self.bc.add_expression(cmp)
                cmp_ref = encode_form_ref(cmp_id)
                return create_inline_call('if', [cmp_ref, items[0], items[1]], self.bc)
            else:
                # min(a, rest) = (if (< a min(rest)) a min(rest))
                first = items[0]
                rest_min = build_min(items[1:])
                rest_id = self.bc.add_expression(rest_min)
                rest_ref = encode_form_ref(rest_id)

                cmp = create_inline_call('less_than', [first, rest_ref], self.bc)
                cmp_id = self.bc.add_expression(cmp)
                cmp_ref = encode_form_ref(cmp_id)
                return create_inline_call('if', [cmp_ref, first, rest_ref], self.bc)

        return build_min(arg_bytes)

    def translate_max(self, args: List[ast.expr]) -> bytes:
        """
        Translate max(a, b, ...) to nested if comparisons.
        max(a, b) -> (if (> a b) a b)
        max(iterable) -> (reduce (lambda (a b) (if (> a b) a b)) iterable)
        """
        if len(args) == 0:
            raise ValueError("max() requires at least 1 argument")

        if len(args) == 1:
            # Python max(iterable): fold with a two-arg max lambda.
            return self._translate_fold_min_max(args[0], 'greater_than')

        # For 2+ arguments, recursively build max comparisons
        arg_bytes = [self.translate_expr_with_ref(arg) for arg in args]

        def build_max(items):
            if len(items) == 1:
                return items[0]
            elif len(items) == 2:
                # (if (> a b) a b)
                cmp = create_inline_call('greater_than', [items[0], items[1]], self.bc)
                cmp_id = self.bc.add_expression(cmp)
                cmp_ref = encode_form_ref(cmp_id)
                return create_inline_call('if', [cmp_ref, items[0], items[1]], self.bc)
            else:
                # max(a, rest) = (if (> a max(rest)) a max(rest))
                first = items[0]
                rest_max = build_max(items[1:])
                rest_id = self.bc.add_expression(rest_max)
                rest_ref = encode_form_ref(rest_id)

                cmp = create_inline_call('greater_than', [first, rest_ref], self.bc)
                cmp_id = self.bc.add_expression(cmp)
                cmp_ref = encode_form_ref(cmp_id)
                return create_inline_call('if', [cmp_ref, first, rest_ref], self.bc)

        return build_max(arg_bytes)

    def translate_sum(self, args: List[ast.expr]) -> bytes:
        """
        Translate sum(iterable) -> (reduce + iterable 0).
        Uses VeloxVM's reduce function.
        """
        if len(args) != 1:
            raise ValueError("sum() takes exactly 1 argument")

        iterable_bytes = self.translate_expr_with_ref(args[0])
        zero_bytes = encode_integer(0)

        # (reduce add iterable 0)
        return create_inline_call('reduce', [
            encode_symbol('add', self.bc),
            iterable_bytes,
            zero_bytes
        ], self.bc)

    def translate_sorted(self, args: List[ast.expr]) -> bytes:
        """
        Translate sorted(iterable) to a sort operation.
        For simplicity, we'll use a manual insertion sort implementation.
        """
        if len(args) != 1:
            raise ValueError("sorted() takes exactly 1 argument")

        raise NotImplementedError(
            "sorted() is not yet implemented in pyvelox. Returning the input "
            "unchanged would produce silently wrong results, so sorting must "
            "be done manually until a VM-level sort primitive is added."
        )

    def translate_reversed(self, args: List[ast.expr]) -> bytes:
        """Translate reversed(iterable) -> (reverse iterable)."""
        if len(args) != 1:
            raise ValueError("reversed() takes exactly 1 argument")

        arg_bytes = self.translate_expr_with_ref(args[0])
        return create_inline_call('reverse', [arg_bytes], self.bc)

    # === Functional-programming built-ins ===

    def translate_map(self, args: List[ast.expr]) -> bytes:
        """
        Translate map(func, iterable) -> (map func iterable).

        Python's map returns an iterator, but we return a list directly
        since VeloxVM doesn't have lazy evaluation.

        Example:
            map(lambda x: x*2, [1, 2, 3]) -> (map (lambda (x) (* x 2)) (list 1 2 3))
        """
        if len(args) != 2:
            raise ValueError("map() takes exactly 2 arguments (function, iterable)")

        func_bytes = self.translate_expr_with_ref(args[0])
        iterable_bytes = self.translate_expr_with_ref(args[1])

        return create_inline_call('map', [func_bytes, iterable_bytes], self.bc)

    def translate_filter(self, args: List[ast.expr]) -> bytes:
        """
        Translate filter(func, iterable) -> (filter func iterable).

        Python's filter returns an iterator, but we return a list directly.

        Example:
            filter(lambda x: x > 0, [-1, 0, 1, 2]) -> (filter (lambda (x) (> x 0)) lst)
        """
        if len(args) != 2:
            raise ValueError("filter() takes exactly 2 arguments (function, iterable)")

        func_bytes = self.translate_expr_with_ref(args[0])
        iterable_bytes = self.translate_expr_with_ref(args[1])

        return create_inline_call('filter', [func_bytes, iterable_bytes], self.bc)

    def translate_reduce(self, args: List[ast.expr]) -> bytes:
        """
        Translate reduce(func, iterable, initial) -> (reduce func iterable initial).

        Note: We require an initial value (Python 3 functools.reduce best practice).

        Example:
            reduce(lambda a, b: a + b, [1, 2, 3, 4], 0)
            -> (reduce (lambda (a b) (+ a b)) (list 1 2 3 4) 0)
        """
        if len(args) != 3:
            raise ValueError("reduce() requires exactly 3 arguments (function, iterable, initial)")

        func_bytes = self.translate_expr_with_ref(args[0])
        iterable_bytes = self.translate_expr_with_ref(args[1])
        initial_bytes = self.translate_expr_with_ref(args[2])

        return create_inline_call('reduce', [func_bytes, iterable_bytes, initial_bytes], self.bc)

    def translate_all(self, args: List[ast.expr]) -> bytes:
        """
        Translate all(iterable) -> (reduce (lambda (a b) (and a b)) iterable #t).

        Returns True if all elements are truthy, False otherwise.

        Example:
            all([True, True, False]) -> (reduce (lambda (a b) (and a b)) (list #t #t #f) #t)
        """
        if len(args) != 1:
            raise ValueError("all() takes exactly 1 argument")

        iterable_bytes = self.translate_expr_with_ref(args[0])
        true_bytes = encode_boolean(True)

        # Create lambda (a b) (and a b)
        and_call = create_inline_call('and', [
            encode_symbol('a', self.bc),
            encode_symbol('b', self.bc)
        ], self.bc)
        lambda_bind = create_bind_form(['a', 'b'], and_call, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        # (reduce lambda iterable #t)
        return create_inline_call('reduce', [
            lambda_bytes,
            iterable_bytes,
            true_bytes
        ], self.bc)

    def translate_any(self, args: List[ast.expr]) -> bytes:
        """
        Translate any(iterable) -> (reduce (lambda (a b) (or a b)) iterable #f).

        Returns True if any element is truthy, False otherwise.

        Example:
            any([False, False, True]) -> (reduce (lambda (a b) (or a b)) (list #f #f #t) #f)
        """
        if len(args) != 1:
            raise ValueError("any() takes exactly 1 argument")

        iterable_bytes = self.translate_expr_with_ref(args[0])
        false_bytes = encode_boolean(False)

        # Create lambda (a b) (or a b)
        or_call = create_inline_call('or', [
            encode_symbol('a', self.bc),
            encode_symbol('b', self.bc)
        ], self.bc)
        lambda_bind = create_bind_form(['a', 'b'], or_call, self.bc)
        lambda_id = self.bc.add_expression(lambda_bind)
        lambda_bytes = encode_form_lambda(lambda_id)

        # (reduce lambda iterable #f)
        return create_inline_call('reduce', [
            lambda_bytes,
            iterable_bytes,
            false_bytes
        ], self.bc)

    def translate_list_constructor(self, args: List[ast.expr]) -> bytes:
        """
        Translate list(iterable) - convert iterable to list.

        In VeloxVM, map/filter already return lists, so this is mostly
        a no-op, but we support it for Python compatibility.

        For a single iterable argument, just return it.
        For multiple arguments or no arguments, create a list.

        Example:
            list([1, 2, 3]) -> [1, 2, 3]  (identity)
            list() -> []
        """
        if len(args) == 0:
            # Empty list
            return create_inline_call('list', [], self.bc)
        elif len(args) == 1:
            # Single argument - just return it (it's already a list in VeloxVM)
            return self.translate_expr_with_ref(args[0])
        else:
            # Multiple arguments - create list from them
            elem_bytecode = [self.translate_expr_with_ref(e) for e in args]
            return create_inline_call('list', elem_bytecode, self.bc)

    def translate_enumerate(self, args: List[ast.expr]) -> bytes:
        """
        Translate enumerate(iterable) -> (list-enumerate iterable)

        Uses the new list_enumerate VM primitive.
        Returns a list of (index, element) pairs.
        """
        if len(args) != 1:
            raise ValueError("enumerate() takes exactly 1 argument")

        list_bytes = self.translate_expr_with_ref(args[0])

        return create_inline_call('list_enumerate', [list_bytes], self.bc)

    def translate_zip(self, args: List[ast.expr]) -> bytes:
        """
        Translate zip(list1, list2) -> (list-zip list1 list2)

        Uses the new list_zip VM primitive.
        Returns a list of (elem1, elem2) pairs.
        """
        if len(args) != 2:
            raise ValueError("zip() requires exactly 2 arguments")

        list1_bytes = self.translate_expr_with_ref(args[0])
        list2_bytes = self.translate_expr_with_ref(args[1])

        return create_inline_call('list_zip', [list1_bytes, list2_bytes], self.bc)

    def translate_list_comp(self, node: ast.ListComp) -> bytes:
        """
        Translate list comprehension to map/filter combination.

        Patterns:
        - [expr for x in iterable] -> (map (lambda (x) expr) iterable)
        - [expr for x in iterable if cond] -> (map (lambda (x) expr)
                                                    (filter (lambda (x) cond) iterable))
        - [expr for x in iter1 for y in iter2] -> nested maps

        Example:
            [x * 2 for x in lst] -> (map (lambda (x) (* x 2)) lst)
            [x for x in lst if x > 0] -> (map (lambda (x) x)
                                              (filter (lambda (x) (> x 0)) lst))
        """
        if len(node.generators) == 0:
            raise ValueError("List comprehension must have at least one generator")

        # For simplicity, we'll handle the most common case first:
        # Single generator with optional filters
        if len(node.generators) == 1:
            gen = node.generators[0]

            # Get the iterator
            iter_bytes = self.translate_expr_with_ref(gen.iter)

            # Get the target variable(s) - for now only support simple names
            if not isinstance(gen.target, ast.Name):
                raise NotImplementedError("List comprehension only supports simple variable targets")

            var_name = gen.target.id
            safe_var = self.get_safe_name(var_name)

            # Apply filters if any
            result_iter = iter_bytes
            for filter_expr in gen.ifs:
                # Create filter lambda: (lambda (var) filter_expr)
                filter_body = self.translate_expr(filter_expr)
                filter_bind = create_bind_form([safe_var], filter_body, self.bc)
                filter_lambda_id = self.bc.add_expression(filter_bind)
                filter_lambda_bytes = encode_form_lambda(filter_lambda_id)

                # Apply filter: (filter lambda iter)
                filtered = create_inline_call('filter', [filter_lambda_bytes, result_iter], self.bc)
                # Store result as expression
                filtered_id = self.bc.add_expression(filtered)
                result_iter = encode_form_ref(filtered_id)

            # Create map lambda: (lambda (var) elt)
            elt_body = self.translate_expr(node.elt)
            map_bind = create_bind_form([safe_var], elt_body, self.bc)
            map_lambda_id = self.bc.add_expression(map_bind)
            map_lambda_bytes = encode_form_lambda(map_lambda_id)

            # Apply map: (map lambda filtered_iter)
            return create_inline_call('map', [map_lambda_bytes, result_iter], self.bc)

        else:
            # Multiple generators: [expr for x in iter1 for y in iter2]
            # This is more complex - we need nested maps or a single map with cartesian product
            # For now, implement using nested reduce to build cartesian product

            # Reverse generators so we build from innermost outward
            gens = list(reversed(node.generators))

            # Build the cartesian product using nested reduce
            # Start with empty list
            result = encode_boolean(False)  # Will be replaced

            # Build from innermost generator outward
            for i, gen in enumerate(gens):
                if not isinstance(gen.target, ast.Name):
                    raise NotImplementedError("List comprehension only supports simple variable targets")

                var_name = gen.target.id
                safe_var = self.get_safe_name(var_name)
                iter_bytes = self.translate_expr_with_ref(gen.iter)

                if i == 0:
                    # Innermost generator: map to create singleton lists
                    # (map (lambda (var) (list (list var))) iter)
                    # But we need to apply filters first
                    filtered_iter = iter_bytes
                    for filter_expr in gen.ifs:
                        filter_body = self.translate_expr(filter_expr)
                        filter_bind = create_bind_form([safe_var], filter_body, self.bc)
                        filter_lambda_id = self.bc.add_expression(filter_bind)
                        filter_lambda_bytes = encode_form_lambda(filter_lambda_id)

                        filtered = create_inline_call('filter', [filter_lambda_bytes, filtered_iter], self.bc)
                        filtered_id = self.bc.add_expression(filtered)
                        filtered_iter = encode_form_ref(filtered_id)

                    # Create list of singleton tuples
                    singleton_body = create_inline_call('list', [encode_symbol(safe_var, self.bc)], self.bc)
                    singleton_bind = create_bind_form([safe_var], singleton_body, self.bc)
                    singleton_lambda_id = self.bc.add_expression(singleton_bind)
                    singleton_lambda = encode_form_lambda(singleton_lambda_id)

                    result = create_inline_call('map', [singleton_lambda, filtered_iter], self.bc)
                    result_id = self.bc.add_expression(result)
                    result = encode_form_ref(result_id)
                else:
                    # Outer generators: cartesian product
                    # This is getting complex - for now raise an error
                    raise NotImplementedError("Multiple generators in list comprehension not yet fully implemented")

            # Finally, map the element expression over the cartesian product
            # Extract all variables and create the final lambda
            all_vars = [self.get_safe_name(g.target.id) for g in reversed(gens)]

            # This is complex - for now just raise an error
            raise NotImplementedError("Multiple generators in list comprehension not yet fully implemented")

    # === Utilities ===

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
        wrap_refs = []
        for name in sorted(box_inits):
            wrap_bytes = self._make_box_init_bytes(name)
            wrap_id = self.bc.add_expression(wrap_bytes)
            wrap_refs.append(encode_form_ref(wrap_id))

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
            for stmt_bytes in stmt_bytes_list:
                stmt_id = self.bc.add_expression(stmt_bytes)
                all_stmt_refs.append(encode_form_ref(stmt_id))
            inner_body_bytes = create_inline_call('begin', all_stmt_refs, self.bc)

        # No need for guard wrapper - the VM's return primitive now works correctly!
        # It distinguishes between bind_function frames (actual functions) and regular
        # bind frames (control flow), so early returns properly exit the function.

        # LET-EXPANSION: ((lambda (vars...) body) #f #f ...)
        # The Racket compiler proves this pattern works, so we just need to encode it correctly
        if local_vars:
            local_var_list = sorted(local_vars)  # Sort for deterministic output

            # Create bind form for the let-lambda: (bind x y ... body)
            # Use inner_body_bytes directly WITHOUT storing as form ref first
            let_bind = create_bind_form(local_var_list, inner_body_bytes, self.bc)
            let_lambda_id = self.bc.add_expression(let_bind)
            let_lambda_bytes = encode_form_lambda(let_lambda_id)

            # Create arguments: #f for each local variable
            false_args = [encode_boolean(False) for _ in local_var_list]

            # Create the call: ((lambda (x y ...) body) #f #f ...)
            let_call_bytes = create_inline_call_direct(let_lambda_bytes, false_args, self.bc)

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


def translate_python_to_bytecode(source_code: str) -> Bytecode:
    """
    Translate Python source code to VeloxVM bytecode.

    Args:
        source_code: Python source code string

    Returns:
        Compiled bytecode
    """
    # Parse Python source to AST
    tree = ast.parse(source_code)

    # Create bytecode container
    bc = Bytecode()

    # Pre-allocate expression 0 (entry point)
    bc.add_expression(b'')

    # Pre-split the source so PyveloxCompileError can quote the offending
    # line. splitlines() drops the trailing newline, which is what we want.
    source_lines = source_code.splitlines()

    # Translate module
    translator = PythonTranslator(bc, source_lines=source_lines)
    accumulated_bytes = translator.translate_module(tree)

    # Replace expression 0 with accumulated bytecode
    bc.replace_expression(0, accumulated_bytes)

    return bc
