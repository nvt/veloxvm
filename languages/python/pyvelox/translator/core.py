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
from .closures import _ClosureAnalysis
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




class PythonTranslator(_BuiltinHandlers, _ClosureAnalysis, _MethodHandlers):
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

    # Tag and slot layout for the tagged vector that pyvelox uses to
    # represent a Python exception at runtime. Distinct from the R7RS
    # error-object tag so the two can coexist without one's predicate
    # false-positiving on the other. Slot 0 carries the tag; slots 1
    # and 2 hold the type symbol and the args list, exposed via
    # `e.type` and `e.args`.
    EXCEPTION_TAG = 'py-exception'
    EXCEPTION_SLOT_TYPE = 1
    EXCEPTION_SLOT_ARGS = 2

    # Tagged-vector layout for Python class objects and instances.
    # Class:    #(pyclass     "Name" parent method-alist)
    # Instance: #(pyinstance  class  slot-alist)
    # method-alist is `((symbol-name . closure) ...)` populated at
    # class-def time. parent is another class object or #f for a
    # root class; method-lookup walks the chain through this slot.
    # slot-alist is `((symbol-name . value) ...)`, initially empty
    # and grown by `self.x = v` writes through the
    # _pyvelox_set_attr helper. Both alists are looked up via assoc.
    CLASS_TAG = 'pyclass'
    INSTANCE_TAG = 'pyinstance'
    CLASS_SLOT_PARENT = 2
    CLASS_SLOT_METHODS = 3


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
        # Functions that accept `*args`. Maps the function's safe name
        # to the safe name of the rest parameter. Populated by the
        # signature pre-pass; consulted by translate_call so the
        # arity/default-padding checks know to allow extra actuals
        # (which the runtime's bind_function_rest soaks up as a list).
        self._vararg_funcs: Dict[str, str] = {}
        # Stack of `as e` names from currently-translating except
        # handlers. translate_raise consults this to decide whether
        # `raise NAME` should pass the bound value through (when NAME
        # matches an active handler variable) or wrap the symbol as a
        # fresh tagged exception (otherwise). translate_attribute uses
        # the same stack to allow `e.args` / `e.type` lookups on the
        # bound exception object.
        self._exception_handler_vars: List[str] = []
        # Names that resolve to a class object at module top level.
        # Populated by translate_class_def, consulted by translate_call
        # to choose between regular function dispatch and instance
        # construction. Tracks safe names (post get_safe_name).
        self._defined_classes: Set[str] = set()
        # Stack of (enclosing-class-safe-name, self-param-safe-name)
        # pairs maintained while compiling method bodies. translate_call
        # consults the top of the stack when it sees `super().m(args)`
        # so the lookup can start from the class's parent and pass
        # the right `self` value.
        self._enclosing_class_stack: List["tuple[str, str]"] = []

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
        """Refuse kwarg/kw-only/positional-only forms; for FunctionDefs
        also encode and record any literal defaults, and record the
        rest-parameter name if `*args` is present.

        Raises NotImplementedError on anything we don't handle — the
        caller wraps it with a source location."""
        args = node.args
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
        # *args + defaults is legal in CPython (`def f(a, b=1, *args)`)
        # but the call-site default-padding logic assumes a fixed-arity
        # signature. Refuse the combination for now; either feature
        # works on its own.
        if args.vararg is not None and args.defaults:
            raise NotImplementedError(
                "Combining `*args` with default arguments is not yet "
                "supported. Drop the defaults or the rest parameter.")

        if args.vararg is not None and not is_lambda:
            self._vararg_funcs[self.get_safe_name(node.name)] = (
                self.get_safe_name(args.vararg.arg))

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
        elif isinstance(stmt, ast.ClassDef):
            return self.translate_class_def(stmt)
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
        """Translate a constant literal (int, bool, str, bytes, None)."""
        value = node.value

        if isinstance(value, bool):
            return encode_boolean(value)
        elif isinstance(value, int):
            return encode_integer(value)
        elif isinstance(value, str):
            return encode_string(value, self.bc)
        elif isinstance(value, bytes):
            return self._translate_bytes_literal(value)
        elif value is None:
            # None -> empty list or #f
            return encode_boolean(False)
        elif isinstance(value, float):
            # Float: convert to integer for now (VeloxVM has optional float support)
            # TODO: Implement proper float encoding if VM_ENABLE_REALS is set
            return encode_integer(int(value))
        else:
            raise NotImplementedError(f"Constant type not supported: {type(value)}")

    def _translate_bytes_literal(self, value: bytes) -> bytes:
        """Lower a `b'...'` literal.

        - Empty literal: `(make-buffer 0)`.
        - Non-empty: emit the `_pyvelox_bytes_from_list` helper and
          call it with a literal `(list b1 b2 ...)`. The helper
          materialises a buffer-flagged vector matching the R7RS
          bytevector representation. Each byte stays a small int
          atom, so element encoding is one byte per element plus the
          list overhead — fine for typical literal sizes.
        """
        if not value:
            return create_inline_call(
                'make_buffer', [encode_integer(0)], self.bc)
        self._emit_bytes_helper()
        elem_bytecode = [encode_integer(b) for b in value]
        list_call = create_inline_call('list', elem_bytecode, self.bc)
        return create_inline_call(
            '_pyvelox_bytes_from_list', [list_call], self.bc)

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
        - `ast.Attribute` — `obj.x = v`, lowered through
          _pyvelox_set_attr against the tagged instance vector.

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
        if isinstance(target, ast.Attribute):
            return self._translate_attribute_assign(target, node.value)
        raise NotImplementedError(
            f"Assignment target type not supported: {type(target).__name__}")

    def _translate_attribute_assign(self, target: ast.Attribute,
                                    value_expr: ast.expr) -> bytes:
        """Lower `obj.attr = value` to
        `(_pyvelox_set_attr obj 'attr value)`. The helper either
        mutates an existing slot via set-cdr! or cons-prepends a new
        (name . value) pair onto the slot-alist."""
        self._emit_oop_helpers()
        recv_bytes = self.translate_expr_with_ref(target.value)
        name_quoted = create_inline_call(
            'quote',
            [encode_symbol(target.attr, self.bc)], self.bc)
        value_bytes = self.translate_expr_with_ref(value_expr)
        return create_inline_call(
            '_pyvelox_set_attr',
            [recv_bytes, name_quoted, value_bytes], self.bc)

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
                     has_rest: bool = False,
                     captures: Optional[List[str]] = None) -> bytes:
        """Build a `(bind[_function[_rest]] params... body)` form,
        store it in the expression table, and return its lambda
        form-ref.

        `is_function=True` marks the lambda as a Python function
        boundary (used by the `return` primitive to know what to
        unwind to); plain lambdas used for control flow stay False.

        `has_rest=True` selects bind_function_rest -- the last entry
        in `params` is the rest formal that soaks up trailing actuals
        as a list. Requires is_function=True.

        `captures`, if given, is a list of safe names whose IDs are
        recorded against the lambda's expression for the runtime
        closure machinery.
        """
        bind_bytes = create_bind_form(
            params, body, self.bc,
            is_function=is_function, has_rest=has_rest)
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

    def translate_function_def(self, node: ast.FunctionDef) -> bytes:
        """Translate `def f(a, b): ...` to `(define f (lambda (a b) ...))`.

        Python has function-level scoping, so all variables assigned
        in the body are hoisted to the top of the lambda via the
        let-expansion in `translate_function_body`; subsequent
        assignments use `set!`.

        `def f(*args)` extends the formal list with the rest-parameter
        name and emits bind_function_rest -- the rest formal soaks up
        trailing actuals as a list at runtime.
        """
        safe_func_name = self.get_safe_name(node.name)
        safe_params = [self.get_safe_name(a.arg) for a in node.args.args]
        rest_name = (self.get_safe_name(node.args.vararg.arg)
                     if node.args.vararg is not None else None)
        all_params = safe_params + [rest_name] if rest_name else safe_params

        # Encode the function name symbol FIRST so it appears in the
        # symbol table before any of its locals.
        func_name_symbol = encode_symbol(safe_func_name, self.bc)

        # Body's local vars get hoisted by the let-expansion in
        # translate_function_body — drop the params, which are already
        # bound by the lambda's bind form.
        local_vars = (self.collect_assigned_vars(node.body)
                      - set(all_params))
        local_set = set(all_params) | local_vars
        is_nested = len(self.scope_stack) > 1

        with self._function_scope(local_set, node.body) as (captures, boxed_names):
            body_bytes = self.translate_function_body(
                node.body, local_vars, box_inits=boxed_names)
            # is_function=True marks this as a Python function boundary
            # so the `return` primitive knows what to unwind to.
            lambda_bytes = self._emit_lambda(
                all_params, body_bytes,
                is_function=True, has_rest=rest_name is not None,
                captures=captures)

        # Top-level defs become global bindings via `define`. Nested
        # defs bind into the enclosing function's let-expansion local
        # (collect_assigned_vars hoists the name), so we emit `set!`
        # instead. set!'s mid-arg evaluation gives the scheduler a
        # chance to materialize a closure for a lambda with free
        # variables.
        op = 'set' if is_nested else 'define'
        return create_inline_call(op, [func_name_symbol, lambda_bytes], self.bc)

    def translate_class_def(self, node: ast.ClassDef) -> bytes:
        """Translate `class Foo: ...` to a top-level binding holding a
        tagged class vector.

        Class vector layout: `#(pyclass "Name" method-alist)`. Each
        method in the body becomes a closure entry in method-alist,
        keyed by the method name as a symbol. The class object is
        bound at module scope via `define`; instance construction
        (`Foo(args)`) resolves the name through the
        `_defined_classes` set populated here, and lowers to the
        _pyvelox_make_instance runtime helper.

        Out-of-scope shapes (base classes, decorators, class-level
        statements other than methods, nested classes inside
        functions) are rejected with located errors below.
        """
        if len(self.scope_stack) > 1:
            raise NotImplementedError(
                "class definitions inside functions are not yet "
                "supported in pyvelox; define classes at module scope.")
        if len(node.bases) > 1:
            raise NotImplementedError(
                "Multiple inheritance is not yet supported. Use a "
                "single base class.")
        if node.keywords:
            raise NotImplementedError(
                "Keyword class arguments (metaclass=, etc.) are not "
                "supported.")
        is_dataclass = False
        for deco in node.decorator_list:
            # Bare `@dataclass` is the only recognised class-level
            # decorator. The parameterised form `@dataclass(eq=...,
            # frozen=...)` parses as Call and is refused.
            if isinstance(deco, ast.Name) and deco.id == 'dataclass':
                is_dataclass = True
                continue
            if (isinstance(deco, ast.Call)
                    and isinstance(deco.func, ast.Name)
                    and deco.func.id == 'dataclass'):
                raise NotImplementedError(
                    "@dataclass with arguments (`@dataclass(...)`) "
                    "is not supported; only the bare form works.")
            raise NotImplementedError(
                f"Class-level decorator @{ast.unparse(deco)} is not "
                f"supported.")

        parent_bytes: bytes = encode_boolean(False)
        if node.bases:
            base = node.bases[0]
            if not isinstance(base, ast.Name):
                raise NotImplementedError(
                    f"Base class must be a simple name "
                    f"(got {type(base).__name__}).")
            # Special-case `Exception`: pyvelox auto-injects a
            # built-in Exception class definition into the program
            # prologue the first time someone references it as a base
            # class (or constructs `Exception(...)` directly via
            # translate_raise). Lets `class MyError(Exception): pass`
            # work without forcing the user to define Exception
            # themselves.
            if base.id == 'Exception':
                self._emit_exception_class()
            base_safe = self.get_safe_name(base.id)
            if base_safe not in self._defined_classes:
                raise NotImplementedError(
                    f"Base class '{base.id}' must be a class defined "
                    f"earlier in the same module.")
            parent_bytes = encode_symbol(base_safe, self.bc)

        safe_class_name = self.get_safe_name(node.name)

        # The body must be a flat sequence of method defs (plus
        # optional pass / docstring). For @dataclass we additionally
        # allow `name: type` annotations declaring fields. Anything
        # else gets refused so silent miscompiles don't sneak in.
        method_nodes: List[ast.FunctionDef] = []
        # Each entry: (field-name, default-expr-or-None). Defaulted
        # fields must come after non-defaulted ones (CPython
        # enforces this; we mirror it). The default expression is
        # restricted to a literal Constant, same as default args on
        # plain functions.
        dataclass_fields: List["tuple[str, Optional[ast.expr]]"] = []
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef):
                if stmt.decorator_list:
                    raise NotImplementedError(
                        f"Method decorators (on `{stmt.name}`) are not "
                        f"yet supported.")
                method_nodes.append(stmt)
            elif isinstance(stmt, ast.Pass):
                continue
            elif (isinstance(stmt, ast.Expr)
                    and isinstance(stmt.value, ast.Constant)
                    and isinstance(stmt.value.value, str)):
                # Leading docstring -- harmless.
                continue
            elif isinstance(stmt, ast.AnnAssign):
                if not is_dataclass:
                    raise NotImplementedError(
                        f"Class-level annotations like `{stmt.target.id}: "
                        f"...` require an @dataclass decorator. Move the "
                        f"declaration into __init__ if you don't want a "
                        f"dataclass.")
                if not isinstance(stmt.target, ast.Name):
                    raise NotImplementedError(
                        "@dataclass field targets must be simple names.")
                default_node = stmt.value
                if default_node is not None:
                    self._validate_literal_default(default_node)
                else:
                    # Non-default after default is illegal in Python's
                    # dataclass.
                    if dataclass_fields and dataclass_fields[-1][1] is not None:
                        raise NotImplementedError(
                            f"Non-default field `{stmt.target.id}` "
                            f"follows default field "
                            f"`{dataclass_fields[-1][0]}`. Reorder so "
                            f"all defaulted fields come last.")
                dataclass_fields.append((stmt.target.id, default_node))
            else:
                raise NotImplementedError(
                    f"Class body may only contain method definitions "
                    f"(got {type(stmt).__name__}). Class-level "
                    f"attributes and arbitrary statements are not "
                    f"supported in pyvelox class bodies.")

        if is_dataclass and not dataclass_fields:
            raise NotImplementedError(
                "@dataclass requires at least one annotated field "
                "(`name: type`); use a plain class otherwise.")
        if dataclass_fields and any(
                m.name == '__init__' for m in method_nodes):
            # CPython skips dataclass __init__ synthesis when the
            # user provides one. Keep the user's __init__ and ignore
            # the field list for synthesis purposes.
            dataclass_fields = []

        # Make sure the runtime helpers exist before any instantiation
        # call site runs, and register the class name as a class
        # *before* compiling method bodies. The latter matters: a
        # method that returns `Foo(args)` -- a recursive constructor
        # call or any reference to the class being defined -- needs
        # translate_call to see "Foo" in `_defined_classes` and lower
        # to instance construction. Without this ordering it would
        # fall through to regular function dispatch and try to call
        # the class vector at runtime, producing a confusing
        # undefined-symbol downstream.
        self._emit_oop_helpers()
        self._defined_classes.add(safe_class_name)
        self.scope_stack[-1].add(safe_class_name)

        # Build method-alist entries. Each entry is `(quote name) .
        # <lambda>`; the lambda is built the same way translate_function_def
        # does (full closure analysis, bind_function / _rest as
        # needed). Methods don't get hoisted into module scope, so we
        # don't emit a `define` for them -- only the class object's
        # define at the bottom.
        method_pairs: List[bytes] = []
        for method_node in method_nodes:
            method_lambda = self._compile_method_lambda(
                method_node, enclosing_class=safe_class_name)
            pair = create_inline_call(
                'cons',
                [create_inline_call(
                    'quote',
                    [encode_symbol(method_node.name, self.bc)], self.bc),
                 method_lambda],
                self.bc)
            method_pairs.append(self._hoist(pair))

        # Dataclass __init__ synthesis. dataclass_fields is non-empty
        # only when @dataclass was on the class AND the user didn't
        # already provide their own __init__. Synthesise a closure
        # that takes `self, field1, field2, ...` and writes each one
        # via _pyvelox_set_attr.
        if dataclass_fields:
            init_lambda = self._synthesize_dataclass_init(
                dataclass_fields)
            init_pair = create_inline_call(
                'cons',
                [create_inline_call(
                    'quote',
                    [encode_symbol('__init__', self.bc)], self.bc),
                 init_lambda],
                self.bc)
            method_pairs.append(self._hoist(init_pair))

        method_alist = create_inline_call('list', method_pairs, self.bc)

        class_vector = create_inline_call(
            'vector',
            [create_inline_call(
                'quote',
                [encode_symbol(self.CLASS_TAG, self.bc)], self.bc),
             encode_string(node.name, self.bc),
             parent_bytes,
             method_alist],
            self.bc)

        return create_inline_call(
            'define',
            [encode_symbol(safe_class_name, self.bc), class_vector],
            self.bc)

    def _synthesize_dataclass_init(
            self,
            fields: List["tuple[str, Optional[ast.expr]]"]) -> bytes:
        """Build the __init__ closure for a @dataclass-decorated
        class. Two shapes:

        - **No fields have defaults**: a fixed-arity closure
          `(self, f1, f2, ...)` where each parameter is stored into
          its slot via _pyvelox_set_attr. Direct, no extra
          machinery.

        - **At least one field has a default**: a variadic closure
          `(self, *_args)` plus argc-based dispatch. Each defaulted
          field's value is `(if (>= n i+1) (list-ref _args i)
          <default>)` where `n` is the actual arg count. Required
          fields (those before the first defaulted one) just take
          `(list-ref _args i)` directly -- if the user calls with
          too few args, list-ref raises at runtime.

          The arg count is computed once via a let-binding so each
          defaulted field's check doesn't re-walk the args list.

        Fields are validated upstream: defaulted fields must be
        contiguous at the tail, defaults must be literal constants,
        and the field list is guaranteed non-empty.
        """
        sym_self = lambda: encode_symbol('self', self.bc)
        any_defaults = any(d is not None for _, d in fields)

        if not any_defaults:
            # Fast path: fixed-arity init.
            body_calls: List[bytes] = []
            for fname, _default in fields:
                safe_fname = self.get_safe_name(fname)
                body_calls.append(create_inline_call(
                    '_pyvelox_set_attr',
                    [sym_self(),
                     create_inline_call(
                         'quote',
                         [encode_symbol(fname, self.bc)], self.bc),
                     encode_symbol(safe_fname, self.bc)],
                    self.bc))
            body = (body_calls[0] if len(body_calls) == 1
                    else create_inline_call(
                        'begin', body_calls, self.bc))
            params = ['self'] + [
                self.get_safe_name(f) for f, _ in fields]
            return self._emit_lambda(
                params, body, is_function=True)

        # Variadic path: argc dispatch around defaulted fields.
        sym_args = lambda: encode_symbol('_args', self.bc)
        sym_n = lambda: encode_symbol('_n', self.bc)
        body_calls = []
        for i, (fname, default_node) in enumerate(fields):
            list_ref = create_inline_call(
                'list_ref',
                [sym_args(), encode_integer(i)], self.bc)
            if default_node is None:
                value_bytes = list_ref
            else:
                default_bytes = self.translate_constant(default_node)
                value_bytes = create_inline_call(
                    'if',
                    [create_inline_call(
                        'greater_than_equal',
                        [sym_n(), encode_integer(i + 1)], self.bc),
                     list_ref,
                     default_bytes],
                    self.bc)
            body_calls.append(create_inline_call(
                '_pyvelox_set_attr',
                [sym_self(),
                 create_inline_call(
                     'quote',
                     [encode_symbol(fname, self.bc)], self.bc),
                 value_bytes],
                self.bc))
        body = (body_calls[0] if len(body_calls) == 1
                else create_inline_call('begin', body_calls, self.bc))

        # Bind n once with ((lambda (_n) <body>) (length _args)).
        n_let_lambda = self._emit_lambda(
            ['_n'], body, is_function=True)
        wrapped_body = create_inline_call(
            n_let_lambda,
            [create_inline_call('length', [sym_args()], self.bc)],
            self.bc)

        return self._emit_lambda(
            ['self', '_args'], wrapped_body,
            is_function=True, has_rest=True)

    def _compile_method_lambda(self, node: ast.FunctionDef, *,
                               enclosing_class: str) -> bytes:
        """Compile a method definition's body into a lambda form-ref,
        without binding the name at any scope. Mirrors
        translate_function_def's body-compilation half but skips the
        outer `define`. The first parameter (`self` by convention) is
        treated like any other -- the call site is responsible for
        passing the receiver as the first actual.

        The (enclosing_class, self-name) pair is pushed onto
        self._enclosing_class_stack across the body-compile so
        super().m() inside the body can find the class context."""
        # Validate signature (covers *args, defaults, etc.) and record
        # the rest-parameter name so calls into this method via the
        # method-dispatch path don't fight the default-padding logic.
        try:
            self._validate_function_signature(node, is_lambda=False)
        except NotImplementedError as exc:
            raise PyveloxCompileError(
                str(exc),
                lineno=getattr(node, 'lineno', None),
                col_offset=getattr(node, 'col_offset', None),
                source_lines=self._source_lines,
            ) from exc

        safe_params = [self.get_safe_name(a.arg) for a in node.args.args]
        rest_name = (self.get_safe_name(node.args.vararg.arg)
                     if node.args.vararg is not None else None)
        all_params = safe_params + [rest_name] if rest_name else safe_params

        local_vars = (self.collect_assigned_vars(node.body)
                      - set(all_params))
        local_set = set(all_params) | local_vars

        # Track the enclosing class + self-name for super() lookups
        # during body compilation. Methods without any parameter (a
        # questionable shape, but legal) get a placeholder self name
        # that super() lowering will refuse to use.
        self_name = safe_params[0] if safe_params else None
        self._enclosing_class_stack.append(
            (enclosing_class, self_name))
        try:
            with self._function_scope(local_set, node.body) as (captures, boxed_names):
                body_bytes = self.translate_function_body(
                    node.body, local_vars, box_inits=boxed_names)
                return self._emit_lambda(
                    all_params, body_bytes,
                    is_function=True, has_rest=rest_name is not None,
                    captures=captures)
        finally:
            self._enclosing_class_stack.pop()

    def _emit_exception_class(self) -> None:
        """Lazily emit a built-in `Exception` class definition into
        the program prologue. Triggered when the user subclasses
        Exception or directly constructs `Exception(args)`. The
        class registers itself in self._defined_classes so subsequent
        forward-reference checks for parent='Exception' succeed.

        The synthesised __init__(self, *args) shape stores the args
        list and the actual class name into the instance's slot-alist
        so handlers can read e.args and e.type:

            def __init__(self, *args):
                self.args = args
                self.type = self.__class__.__name__

        Reading the class name uses (vector-ref (vector-ref self 1) 1)
        directly -- self.__class__ has no real expression form in
        pyvelox, but we have the slot indices fixed and inheritance
        keeps the class name the *subclass*'s name, not Exception's.
        """
        if 'Exception' in self._emitted_helpers:
            return
        # Helpers that __init__ depends on must already be in the
        # prologue; emit them first so define order is correct at run
        # time.
        self._emit_oop_helpers()
        self._emitted_helpers.add('Exception')
        self._defined_classes.add(self.get_safe_name('Exception'))

        sym_self = lambda: encode_symbol('self', self.bc)
        sym_args = lambda: encode_symbol('args', self.bc)

        # (_pyvelox_set_attr self 'args args)
        set_args_call = create_inline_call(
            '_pyvelox_set_attr',
            [sym_self(),
             create_inline_call(
                 'quote',
                 [encode_symbol('args', self.bc)], self.bc),
             sym_args()],
            self.bc)
        # (vector-ref (vector-ref self 1) 1) -- the class's name string.
        class_name_lookup = create_inline_call(
            'vector_ref',
            [create_inline_call(
                'vector_ref',
                [sym_self(), encode_integer(1)], self.bc),
             encode_integer(1)],
            self.bc)
        # (_pyvelox_set_attr self 'type <class-name-string>)
        set_type_call = create_inline_call(
            '_pyvelox_set_attr',
            [sym_self(),
             create_inline_call(
                 'quote',
                 [encode_symbol('type', self.bc)], self.bc),
             class_name_lookup],
            self.bc)
        init_body = create_inline_call(
            'begin', [set_args_call, set_type_call], self.bc)
        init_lambda_ref = self._emit_lambda(
            ['self', 'args'], init_body,
            is_function=True, has_rest=True)

        init_pair = create_inline_call(
            'cons',
            [create_inline_call(
                'quote',
                [encode_symbol('__init__', self.bc)], self.bc),
             init_lambda_ref],
            self.bc)
        method_alist = create_inline_call(
            'list', [self._hoist(init_pair)], self.bc)
        class_vector = create_inline_call(
            'vector',
            [create_inline_call(
                'quote',
                [encode_symbol(self.CLASS_TAG, self.bc)], self.bc),
             encode_string('Exception', self.bc),
             encode_boolean(False),
             method_alist],
            self.bc)
        define_exc = create_inline_call(
            'define',
            [encode_symbol(self.get_safe_name('Exception'), self.bc),
             class_vector],
            self.bc)
        self._preamble.extend(define_exc)

    def _emit_oop_helpers(self) -> None:
        """Emit the small runtime library that backs class objects and
        instances. Idempotent. Helpers are written into the program
        prologue so any class definition or instance call site can
        reference them by name.

        Provided definitions:
        - _pyvelox_make_instance(class, init-args-list): allocate a
          fresh instance vector, run __init__ if defined, return the
          instance.
        - _pyvelox_lookup_method(class, name): fetch the closure for
          `name` from the class's method-alist; raise AttributeError
          via the structured-raise machinery if missing.
        - _pyvelox_get_attr(instance, name): look up `name` in the
          instance's slot-alist; raise AttributeError on miss.
        - _pyvelox_set_attr(instance, name, value): if a slot with
          `name` exists, mutate it via set-cdr!; else cons a new
          (name . value) pair onto the slot-alist (allowing dynamic
          attribute growth, matching CPython).
        """
        if '_pyvelox_make_instance' in self._emitted_helpers:
            return
        self._emitted_helpers.add('_pyvelox_make_instance')

        # All four helpers share the AttributeError-raising shape used
        # by lookup_method and get_attr; centralise it.
        def attr_error(name_sym_bytes: bytes) -> bytes:
            return create_inline_call(
                'raise',
                [create_inline_call(
                    'vector',
                    [create_inline_call(
                        'quote',
                        [encode_symbol(self.EXCEPTION_TAG, self.bc)],
                        self.bc),
                     encode_string('AttributeError', self.bc),
                     create_inline_call('list',
                                        [name_sym_bytes], self.bc)],
                    self.bc)],
                self.bc)

        sym_class = lambda: encode_symbol('class', self.bc)
        sym_instance = lambda: encode_symbol('instance', self.bc)
        sym_name = lambda: encode_symbol('name', self.bc)
        sym_value = lambda: encode_symbol('value', self.bc)
        sym_slot = lambda: encode_symbol('slot', self.bc)
        sym_init = lambda: encode_symbol('init', self.bc)
        sym_args = lambda: encode_symbol('args', self.bc)

        # _pyvelox_lookup_method(class, name)
        # Walks the parent chain. Returns the closure on the first
        # class that defines `name`; raises AttributeError when the
        # chain bottoms out at #f without a hit.
        lookup_recurse = create_inline_call(
            '_pyvelox_lookup_method',
            [create_inline_call(
                'vector_ref',
                [sym_class(), encode_integer(self.CLASS_SLOT_PARENT)],
                self.bc),
             sym_name()],
            self.bc)
        lookup_in_methods = create_inline_call(
            'if',
            [create_inline_call(
                'assoc',
                [sym_name(),
                 create_inline_call(
                     'vector_ref',
                     [sym_class(),
                      encode_integer(self.CLASS_SLOT_METHODS)],
                     self.bc)],
                self.bc),
             create_inline_call(
                 'cdr',
                 [create_inline_call(
                     'assoc',
                     [sym_name(),
                      create_inline_call(
                          'vector_ref',
                          [sym_class(),
                           encode_integer(self.CLASS_SLOT_METHODS)],
                          self.bc)],
                     self.bc)],
                 self.bc),
             lookup_recurse],
            self.bc)
        lookup_body = create_inline_call(
            'if',
            [sym_class(), lookup_in_methods, attr_error(sym_name())],
            self.bc)
        lookup_lambda_ref = self._emit_lambda(
            ['class', 'name'], lookup_body, is_function=True)
        define_lookup = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_lookup_method', self.bc),
             lookup_lambda_ref],
            self.bc)
        self._preamble.extend(define_lookup)

        # _pyvelox_get_attr(instance, name)
        # Two receiver shapes are recognised:
        #
        # 1. py-exception tagged vector (the shape `raise X(args)`
        #    builds when X isn't a defined class). Slot 1 holds the
        #    type string, slot 2 the args list; we surface those as
        #    `.type` / `.args`. Anything else on a py-exception
        #    raises AttributeError.
        #
        # 2. pyinstance tagged vector (class instances, including
        #    custom exception subclasses whose __init__ stored
        #    args/type into the slot-alist). Looks up `name` via
        #    assoc on slot 2.
        #
        # Anything else (a number, list, etc.) falls through to
        # AttributeError too. This unifies the two exception
        # representations under one access path so user code can
        # read `e.args` and `e.type` regardless of which shape the
        # exception was raised in.
        py_exc_args = create_inline_call(
            'vector_ref',
            [sym_instance(),
             encode_integer(self.EXCEPTION_SLOT_ARGS)], self.bc)
        py_exc_type = create_inline_call(
            'vector_ref',
            [sym_instance(),
             encode_integer(self.EXCEPTION_SLOT_TYPE)], self.bc)
        py_exc_dispatch = create_inline_call(
            'if',
            [create_inline_call(
                'eqp',
                [sym_name(),
                 create_inline_call(
                     'quote',
                     [encode_symbol('args', self.bc)], self.bc)],
                self.bc),
             py_exc_args,
             create_inline_call(
                 'if',
                 [create_inline_call(
                     'eqp',
                     [sym_name(),
                      create_inline_call(
                          'quote',
                          [encode_symbol('type', self.bc)],
                          self.bc)],
                     self.bc),
                  py_exc_type,
                  attr_error(sym_name())],
                 self.bc)],
            self.bc)

        # pyinstance dispatch: assoc on slot 2.
        slot_alist = create_inline_call(
            'vector_ref',
            [sym_instance(), encode_integer(2)], self.bc)
        instance_dispatch = create_inline_call(
            'if',
            [create_inline_call('assoc', [sym_name(), slot_alist], self.bc),
             create_inline_call(
                 'cdr',
                 [create_inline_call(
                     'assoc', [sym_name(), slot_alist], self.bc)],
                 self.bc),
             attr_error(sym_name())],
            self.bc)

        # Outer tag check: same py-exception shape predicate as
        # _pyvelox_str uses. and short-circuits, so vector-ref only
        # fires after vectorp confirms the receiver is a vector.
        is_py_exc = create_inline_call(
            'and',
            [create_inline_call('vectorp', [sym_instance()], self.bc),
             create_inline_call(
                 'equal',
                 [create_inline_call(
                     'vector_length', [sym_instance()], self.bc),
                  encode_integer(3)],
                 self.bc),
             create_inline_call(
                 'eqp',
                 [create_inline_call(
                     'vector_ref',
                     [sym_instance(), encode_integer(0)], self.bc),
                  create_inline_call(
                      'quote',
                      [encode_symbol(self.EXCEPTION_TAG, self.bc)],
                      self.bc)],
                 self.bc)],
            self.bc)
        get_body = create_inline_call(
            'if', [is_py_exc, py_exc_dispatch, instance_dispatch],
            self.bc)
        get_lambda_ref = self._emit_lambda(
            ['instance', 'name'], get_body, is_function=True)
        define_get = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_get_attr', self.bc),
             get_lambda_ref],
            self.bc)
        self._preamble.extend(define_get)

        # _pyvelox_set_attr(instance, name, value)
        # If slot exists, set-cdr! it; else cons a new pair onto the
        # slot-alist and write it back via vector-set! at slot 2.
        set_existing = create_inline_call(
            'set_cdr', [sym_slot(), sym_value()], self.bc)
        set_new = create_inline_call(
            'vector_set',
            [sym_instance(), encode_integer(2),
             create_inline_call(
                 'cons',
                 [create_inline_call(
                     'cons', [sym_name(), sym_value()], self.bc),
                  create_inline_call(
                      'vector_ref',
                      [sym_instance(), encode_integer(2)], self.bc)],
                 self.bc)],
            self.bc)
        # let ((slot (assoc name (vector-ref instance 2))))
        #   (if slot (set-cdr! slot value) (vector-set! instance 2 ...))
        set_let_body = create_inline_call(
            'if', [sym_slot(), set_existing, set_new], self.bc)
        set_let_inner_lambda = self._emit_lambda(
            ['slot'], set_let_body, is_function=True)
        slot_value = create_inline_call(
            'assoc',
            [sym_name(),
             create_inline_call(
                 'vector_ref',
                 [sym_instance(), encode_integer(2)], self.bc)],
            self.bc)
        set_body = create_inline_call(
            set_let_inner_lambda, [slot_value], self.bc)
        set_lambda_ref = self._emit_lambda(
            ['instance', 'name', 'value'], set_body, is_function=True)
        define_set = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_set_attr', self.bc),
             set_lambda_ref],
            self.bc)
        self._preamble.extend(define_set)

        # _pyvelox_make_instance(class, init-args-list)
        # Allocate the instance, then look up + apply __init__ inside
        # a guard so that a class chain with no __init__ silently
        # falls back to "no init" rather than propagating the
        # AttributeError that lookup_method raises on a miss. Other
        # exceptions raised by __init__ itself are re-raised by the
        # handler (which discriminates on the AttributeError shape).
        # The lookup_method call must be *inside* the guard, not in a
        # let-binding value -- otherwise the lookup throws before the
        # guard is on the stack.
        make_instance_alloc = create_inline_call(
            'vector',
            [create_inline_call(
                'quote',
                [encode_symbol(self.INSTANCE_TAG, self.bc)], self.bc),
             sym_class(),
             create_inline_call('list', [], self.bc)],
            self.bc)
        init_quoted = create_inline_call(
            'quote',
            [encode_symbol('__init__', self.bc)], self.bc)
        guard_var = '_exc'
        is_attr_err = create_inline_call(
            'and',
            [create_inline_call(
                'vectorp', [encode_symbol(guard_var, self.bc)], self.bc),
             create_inline_call(
                 'equal',
                 [create_inline_call(
                     'vector_length',
                     [encode_symbol(guard_var, self.bc)], self.bc),
                  encode_integer(3)],
                 self.bc),
             create_inline_call(
                 'equalp',
                 [create_inline_call(
                     'vector_ref',
                     [encode_symbol(guard_var, self.bc),
                      encode_integer(self.EXCEPTION_SLOT_TYPE)],
                     self.bc),
                  encode_string('AttributeError', self.bc)],
                 self.bc)],
            self.bc)
        rethrow = create_inline_call(
            'raise', [encode_symbol(guard_var, self.bc)], self.bc)
        # On AttributeError (init missing), evaluate to instance
        # unchanged; on any other exception, re-raise.
        guard_handler = create_inline_call(
            'if', [is_attr_err, sym_instance(), rethrow], self.bc)
        guard_handler_ref = self._hoist(guard_handler)
        # Try body: do the lookup + apply, return instance.
        init_lookup = create_inline_call(
            '_pyvelox_lookup_method',
            [sym_class(), init_quoted], self.bc)
        apply_init = create_inline_call(
            'apply',
            [init_lookup,
             create_inline_call(
                 'cons', [sym_instance(), sym_args()], self.bc)],
            self.bc)
        try_body = create_inline_call(
            'begin', [apply_init, sym_instance()], self.bc)
        try_body_ref = self._hoist(try_body)
        guarded = create_inline_call(
            'guard',
            [encode_symbol(guard_var, self.bc),
             guard_handler_ref,
             try_body_ref],
            self.bc)
        # ((lambda (instance) <guarded>) <alloc>)
        outer_let_lambda = self._emit_lambda(
            ['instance'], guarded, is_function=True)
        make_body = create_inline_call(
            outer_let_lambda, [make_instance_alloc], self.bc)
        make_lambda_ref = self._emit_lambda(
            ['class', 'args'], make_body, is_function=True)
        define_make = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_make_instance', self.bc),
             make_lambda_ref],
            self.bc)
        self._preamble.extend(define_make)

        # _pyvelox_isinstance(instance, class)
        # True iff instance is a tagged pyinstance whose class chain
        # contains class. Walks parent slots until it finds a match
        # or bottoms out at #f.
        sym_iclass = lambda: encode_symbol('iclass', self.bc)
        sym_target = lambda: encode_symbol('target', self.bc)
        # Inner walker: _pyvelox_class_extends(iclass, target)
        extends_recurse = create_inline_call(
            '_pyvelox_class_extends',
            [create_inline_call(
                'vector_ref',
                [sym_iclass(), encode_integer(self.CLASS_SLOT_PARENT)],
                self.bc),
             sym_target()],
            self.bc)
        extends_match = create_inline_call(
            'if',
            [create_inline_call(
                'eqp', [sym_iclass(), sym_target()], self.bc),
             encode_boolean(True),
             extends_recurse],
            self.bc)
        extends_body = create_inline_call(
            'if',
            [sym_iclass(), extends_match, encode_boolean(False)],
            self.bc)
        extends_lambda_ref = self._emit_lambda(
            ['iclass', 'target'], extends_body, is_function=True)
        define_extends = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_class_extends', self.bc),
             extends_lambda_ref],
            self.bc)
        self._preamble.extend(define_extends)

        # Outer: confirm instance shape, then walk.
        is_pyinstance = create_inline_call(
            'and',
            [create_inline_call(
                'vectorp', [sym_instance()], self.bc),
             create_inline_call(
                 'equal',
                 [create_inline_call(
                     'vector_length', [sym_instance()], self.bc),
                  encode_integer(3)],
                 self.bc),
             create_inline_call(
                 'eqp',
                 [create_inline_call(
                     'vector_ref',
                     [sym_instance(), encode_integer(0)], self.bc),
                  create_inline_call(
                      'quote',
                      [encode_symbol(self.INSTANCE_TAG, self.bc)],
                      self.bc)],
                 self.bc)],
            self.bc)
        isinstance_walk = create_inline_call(
            '_pyvelox_class_extends',
            [create_inline_call(
                'vector_ref',
                [sym_instance(), encode_integer(1)], self.bc),
             sym_target()],
            self.bc)
        isinstance_body = create_inline_call(
            'if',
            [is_pyinstance, isinstance_walk, encode_boolean(False)],
            self.bc)
        isinstance_lambda_ref = self._emit_lambda(
            ['instance', 'target'], isinstance_body, is_function=True)
        define_isinstance = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_isinstance', self.bc),
             isinstance_lambda_ref],
            self.bc)
        self._preamble.extend(define_isinstance)

    def translate_lambda(self, node: ast.Lambda) -> bytes:
        """Translate `lambda x: expr` — Python lambdas are real
        functions (like `def`), so they get `bind_function`. Unlike
        `def`, no variable hoisting — the body is a single expression.
        `lambda *args: ...` extends the formal list with the rest
        name and emits bind_function_rest, same as a def.
        """
        safe_params = [self.get_safe_name(a.arg) for a in node.args.args]
        rest_name = (self.get_safe_name(node.args.vararg.arg)
                     if node.args.vararg is not None else None)
        all_params = safe_params + [rest_name] if rest_name else safe_params
        # _analyze_body takes a statement list; wrap the body in an
        # Expr so the same analyser handles lambdas and defs.
        body_stmts = [ast.Expr(value=node.body)]

        with self._function_scope(set(all_params), body_stmts) as (captures, boxed_names):
            body_bytes = self.translate_expr(node.body)
            if boxed_names:
                # Prepend (set! p (box p)) wraps for each boxed param,
                # joining them with the body in a begin form.
                wraps = [self._make_box_init_bytes(n)
                         for n in sorted(boxed_names)]
                body_bytes = create_inline_call(
                    'begin', wraps + [body_bytes], self.bc)
            return self._emit_lambda(
                all_params, body_bytes,
                is_function=True, has_rest=rest_name is not None,
                captures=captures)

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

    def _translate_instance_construction(self, class_name: str,
                                         arg_nodes: List[ast.expr],
                                         starred_idxs: List[int]) -> bytes:
        """Lower `Foo(args)` (where Foo names a class) to a call into
        _pyvelox_make_instance. Trailing *args is supported because the
        helper takes the args as a single list -- we just splice the
        starred value onto the prefix at the call site, mirroring the
        forwarding-via-apply path. No call-site default-padding here:
        any defaults declared by a synthesised __init__ are handled
        inside that closure via its own argc dispatch."""
        if starred_idxs and starred_idxs[0] != len(arg_nodes) - 1:
            raise NotImplementedError(
                "Positional arguments after *-argument are not "
                "supported in class instantiation either.")
        if starred_idxs:
            fixed_nodes = arg_nodes[:-1]
            rest_value = self.translate_expr_with_ref(
                arg_nodes[-1].value)
            arg_list = rest_value
            for fixed in reversed(fixed_nodes):
                fixed_bytes = self.translate_expr_with_ref(fixed)
                arg_list = create_inline_call(
                    'cons', [fixed_bytes, arg_list], self.bc)
        else:
            arg_list = create_inline_call(
                'list',
                [self.translate_expr_with_ref(a) for a in arg_nodes],
                self.bc)
        return create_inline_call(
            '_pyvelox_make_instance',
            [encode_symbol(self.get_safe_name(class_name), self.bc),
             arg_list],
            self.bc)

    def _translate_user_method_call(self, recv_node: ast.expr,
                                    method_name: str,
                                    arg_nodes: List[ast.expr]) -> bytes:
        """Lower `obj.method(args)` for an unknown method name to a
        runtime method-lookup-and-call:

            ((_pyvelox_lookup_method (vector-ref obj 1) 'method)
             obj arg1 arg2 ...)

        The receiver is hoisted into the expression table once and
        the form-ref appears in two positions (the class fetch and
        the self argument); each form-ref re-evaluation just
        dereferences the cached expression, so this is safe even for
        side-effecting receivers."""
        # Reject *args here -- forwarding through method dispatch
        # would need an extra apply, which the lookup already uses.
        if any(isinstance(a, ast.Starred) for a in arg_nodes):
            raise NotImplementedError(
                "Method calls don't yet support *-arguments")

        recv_token = self.translate_expr_with_ref(recv_node)
        method_quoted = create_inline_call(
            'quote', [encode_symbol(method_name, self.bc)], self.bc)
        class_lookup = create_inline_call(
            'vector_ref',
            [recv_token, encode_integer(1)], self.bc)
        method_closure = create_inline_call(
            '_pyvelox_lookup_method',
            [class_lookup, method_quoted], self.bc)
        method_ref = self._hoist(method_closure)
        arg_bytes = [recv_token] + [
            self.translate_expr_with_ref(a) for a in arg_nodes]
        return create_inline_call(method_ref, arg_bytes, self.bc)

    def _translate_pseudo_module_call(self, attr_node: ast.Attribute,
                                      arg_nodes: List[ast.expr]
                                      ) -> Optional[bytes]:
        """Recognise `math.isqrt(...)` and other pseudo-module calls.
        Returns the lowered bytecode if the receiver is a recognised
        pseudo-module name, or None to let the regular method
        dispatch take over.

        Tying these to specific Name receivers (rather than
        registering them as builtin methods like .upper) means a
        local variable named `math` can still mask the pseudo-module:
        the receiver has to be a literal Name reference and the name
        has to actually be in _PSEUDO_MODULES."""
        if not (isinstance(attr_node.value, ast.Name)
                and attr_node.value.id in self._PSEUDO_MODULES):
            return None
        # User shadowing -- `math = ...` then `math.isqrt(...)` --
        # falls back to instance method dispatch as if math weren't a
        # pseudo-module. The shadowed variable wins.
        if attr_node.value.id in self.scope_stack[-1]:
            return None
        if attr_node.value.id == 'math':
            return self._translate_math_call(attr_node.attr, arg_nodes)
        return None

    def _translate_math_call(self, name: str,
                             arg_nodes: List[ast.expr]
                             ) -> bytes:
        """Lower `math.NAME(args)` for the supported subset.

        Currently implemented: math.isqrt(n) -- integer square root,
        Newton's method via a runtime helper. Other math.* names are
        refused so the user sees a friendly error rather than a
        runtime "math.X has no method" surprise."""
        if name == 'isqrt':
            if len(arg_nodes) != 1:
                raise ValueError(
                    f"math.isqrt() takes exactly 1 argument "
                    f"({len(arg_nodes)} given)")
            self._emit_isqrt_helper()
            arg_bytes = self.translate_expr_with_ref(arg_nodes[0])
            return create_inline_call(
                '_pyvelox_isqrt', [arg_bytes], self.bc)
        raise NotImplementedError(
            f"math.{name} is not supported in pyvelox; only "
            f"math.isqrt is currently recognised.")

    def _emit_isqrt_helper(self) -> None:
        """Emit `_pyvelox_isqrt` and `_pyvelox_isqrt_loop` into the
        program prologue. Idempotent.

        Newton's method on integers, mirroring R7RS
        exact-integer-sqrt (commit eadc26b on the Scheme side).
        Negative input raises a structured ValueError so handlers
        can catch it as `except Exception as e:` and read e.type.

            (define (_pyvelox_isqrt n)
              (if (< n 0)
                  (raise <ValueError py-exception>)
                  (if (< n 2) n (_pyvelox_isqrt_loop n n))))

            (define (_pyvelox_isqrt_loop n x)
              (let ((y (quotient (+ x (quotient n x)) 2)))
                (if (>= y x) x (_pyvelox_isqrt_loop n y))))
        """
        if '_pyvelox_isqrt' in self._emitted_helpers:
            return
        self._emitted_helpers.add('_pyvelox_isqrt')

        sym_n = lambda: encode_symbol('n', self.bc)
        sym_x = lambda: encode_symbol('x', self.bc)
        sym_y = lambda: encode_symbol('y', self.bc)

        # _pyvelox_isqrt_loop(n, x):
        # let y = (n/x + x) / 2
        # if y >= x then x else recurse with x=y
        next_y = create_inline_call(
            'quotient',
            [create_inline_call(
                'add',
                [create_inline_call(
                    'quotient', [sym_n(), sym_x()], self.bc),
                 sym_x()],
                self.bc),
             encode_integer(2)],
            self.bc)
        recurse = create_inline_call(
            '_pyvelox_isqrt_loop', [sym_n(), sym_y()], self.bc)
        loop_decision = create_inline_call(
            'if',
            [create_inline_call(
                'greater_than_equal', [sym_y(), sym_x()], self.bc),
             sym_x(), recurse],
            self.bc)
        # ((lambda (y) <decision>) <next-y>)
        loop_let_inner = self._emit_lambda(
            ['y'], loop_decision, is_function=True)
        loop_body = create_inline_call(
            loop_let_inner, [next_y], self.bc)
        loop_lambda_ref = self._emit_lambda(
            ['n', 'x'], loop_body, is_function=True)
        define_loop = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_isqrt_loop', self.bc),
             loop_lambda_ref],
            self.bc)
        self._preamble.extend(define_loop)

        # _pyvelox_isqrt(n):
        # if n < 0 -> raise ValueError; n < 2 -> n; else loop.
        # Build the ValueError directly as a py-exception vector --
        # using the legacy shape avoids depending on whether the
        # OOP helpers were emitted (the user might call math.isqrt
        # without ever defining a class). Handler-side e.args / str(e)
        # access still works because _pyvelox_get_attr / _pyvelox_str
        # recognise both shapes.
        value_error = self._make_exception_obj(
            'ValueError',
            [ast.Constant(
                value="isqrt() argument must be nonnegative")])
        raise_neg = create_inline_call(
            'raise', [value_error], self.bc)
        small_branch = create_inline_call(
            'if',
            [create_inline_call(
                'less_than',
                [sym_n(), encode_integer(2)], self.bc),
             sym_n(),
             create_inline_call(
                 '_pyvelox_isqrt_loop',
                 [sym_n(), sym_n()], self.bc)],
            self.bc)
        isqrt_body = create_inline_call(
            'if',
            [create_inline_call(
                'less_than',
                [sym_n(), encode_integer(0)], self.bc),
             raise_neg, small_branch],
            self.bc)
        isqrt_lambda_ref = self._emit_lambda(
            ['n'], isqrt_body, is_function=True)
        define_isqrt = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_isqrt', self.bc),
             isqrt_lambda_ref],
            self.bc)
        self._preamble.extend(define_isqrt)

    def _is_super_call(self, node) -> bool:
        """True if `node` is a `super()` call expression with no
        arguments. Used by translate_call to detect the
        `super().method(args)` pattern before generic method
        dispatch."""
        return (isinstance(node, ast.Call)
                and isinstance(node.func, ast.Name)
                and node.func.id == 'super'
                and not node.args
                and not node.keywords)

    def _translate_super_method_call(self, method_name: str,
                                     arg_nodes: List[ast.expr]) -> bytes:
        """Lower `super().method(args)` to a parent-class method
        lookup followed by an explicit-self call. Requires an active
        enclosing class context (push/pop happens in
        _compile_method_lambda); refuses the call otherwise.

        Lowering shape:
            ((_pyvelox_lookup_method (vector-ref <Class> 2)
                                     (quote method))
             <self> arg1 arg2 ...)

        where <Class> is the symbol for the enclosing class object
        and <self> is the first parameter of the enclosing method.
        Slot 2 is the parent slot (CLASS_SLOT_PARENT), so the lookup
        starts from the parent and walks further if needed."""
        if not self._enclosing_class_stack:
            raise NotImplementedError(
                "super() can only be used inside a method body.")
        enclosing_class, self_name = self._enclosing_class_stack[-1]
        if self_name is None:
            raise NotImplementedError(
                "super() needs the enclosing method's first "
                "parameter (typically `self`); the method has no "
                "parameters.")
        if any(isinstance(a, ast.Starred) for a in arg_nodes):
            raise NotImplementedError(
                "super() calls don't yet support *-arguments.")

        method_quoted = create_inline_call(
            'quote', [encode_symbol(method_name, self.bc)], self.bc)
        parent_class = create_inline_call(
            'vector_ref',
            [encode_symbol(enclosing_class, self.bc),
             encode_integer(self.CLASS_SLOT_PARENT)],
            self.bc)
        method_closure = create_inline_call(
            '_pyvelox_lookup_method',
            [parent_class, method_quoted], self.bc)
        method_ref = self._hoist(method_closure)
        arg_bytes = [encode_symbol(self_name, self.bc)] + [
            self.translate_expr_with_ref(a) for a in arg_nodes]
        return create_inline_call(method_ref, arg_bytes, self.bc)

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

        `f(*args)` (a call with one trailing Starred argument) is
        special: it lowers to (apply func arg_list), where arg_list
        cons-prepends any leading fixed actuals onto the starred
        value. Method calls and built-in calls don't currently accept
        Starred -- their handlers don't know how to forward.
        """
        # Validate Starred up front: most shapes are out of scope and
        # the regular dispatch would otherwise fail in a confusing
        # spot (e.g. inside the builtin handler, on the inner Starred
        # element).
        starred_idxs = [i for i, a in enumerate(node.args)
                        if isinstance(a, ast.Starred)]
        if len(starred_idxs) > 1:
            raise NotImplementedError(
                "Multiple *-arguments at a single call site are not "
                "yet supported")
        if starred_idxs and starred_idxs[0] != len(node.args) - 1:
            raise NotImplementedError(
                "Positional arguments after *-argument are not yet "
                "supported (the rest list must be the last actual)")
        if starred_idxs:
            if isinstance(node.func, ast.Attribute):
                raise NotImplementedError(
                    "Method calls don't yet support *-arguments")
            if isinstance(node.func, ast.Name) and (
                    node.func.id in self._BUILTIN_HANDLERS):
                raise NotImplementedError(
                    f"Built-in {node.func.id}() doesn't yet support "
                    f"*-arguments")

        # Special-case 1: known method calls.
        if isinstance(node.func, ast.Attribute):
            # super().method(args) takes priority over both the
            # builtin-method registry and the generic instance dispatch:
            # it has its own lowering that starts the method walk from
            # the enclosing class's parent.
            if self._is_super_call(node.func.value):
                return self._translate_super_method_call(
                    node.func.attr, node.args)
            # Pseudo-module calls (`math.isqrt(...)`) are dispatched
            # syntactically -- the compiler emits a runtime helper
            # rather than going through method-on-an-instance.
            module_call = self._translate_pseudo_module_call(
                node.func, node.args)
            if module_call is not None:
                return module_call
            handler = self._METHOD_HANDLERS.get(node.func.attr)
            if handler is not None:
                return handler(self, node.func.value, node.args)
            # Unknown method name -> assume the receiver is an instance
            # of a user-defined class and dispatch via the runtime
            # method-lookup helper. The receiver evaluates once because
            # it's the helper's first argument.
            return self._translate_user_method_call(
                node.func.value, node.func.attr, node.args)

        # Resolve a Name callee once. callee_name is the source-level
        # name (or None for compound callees); func_bytes is the
        # encoded operator to emit.
        if isinstance(node.func, ast.Name):
            callee_name = node.func.id
            # Reject bare super() / super(args) -- only super().method(args)
            # is supported, which is intercepted on the Attribute path
            # above. Reaching the Name path means the user wrote super()
            # standalone, which would return a super-proxy object we
            # don't model.
            if callee_name == 'super':
                raise NotImplementedError(
                    "Bare super() is not supported -- only "
                    "`super().method(args)` is recognised. The 2-arg "
                    "form `super(Class, self)` is not supported either.")
            # Special-case 2: known built-ins.
            handler_name = self._BUILTIN_HANDLERS.get(callee_name)
            if handler_name is not None:
                return getattr(self, handler_name)(node.args)
            # Special-case 2b: instance construction. `Foo(args)` for
            # a name registered by translate_class_def lowers through
            # the make_instance helper, which allocates the instance
            # vector and runs __init__ if present. The same name
            # lookup that translate_class_def already did at class-def
            # time is replayed here so a class shadowed by a later
            # `def foo` still dispatches as the function.
            if (self.get_safe_name(callee_name) in self._defined_classes
                    and callee_name not in self.renamed_vars):
                return self._translate_instance_construction(
                    callee_name, node.args, starred_idxs)
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

        # Forwarding via *args -- emit (apply func arg_list). Built
        # the list at the call site by cons-prepending any leading
        # fixed actuals onto the starred value, so a wrapper like
        # `def w(a, *args): inner(a, *args)` doesn't lose the prefix.
        # We skip the default-padding logic because we can't tell at
        # compile time how many elements the rest list will hold;
        # bind_function / bind_function_rest will check arity at
        # runtime.
        if starred_idxs:
            fixed_nodes = node.args[:-1]
            rest_node = node.args[-1].value  # Starred.value
            arg_list = self.translate_expr_with_ref(rest_node)
            for fixed in reversed(fixed_nodes):
                fixed_bytes = self.translate_expr_with_ref(fixed)
                arg_list = create_inline_call(
                    'cons', [fixed_bytes, arg_list], self.bc)
            return create_inline_call(
                'apply', [func_bytes, arg_list], self.bc)

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

    # Synthetic name bound by the VM's `guard` to the caught
    # exception. User-named `as e:` clauses introduce their own
    # let-style scope around the handler body so the user's name
    # binds to this same value.
    _GUARD_EXC_VAR = '_exc'

    def translate_try(self, node: ast.Try) -> bytes:
        """
        Translate try/except, including typed handlers and multiple
        clauses:

            try:
                body
            except SomeError as e:
                clause1
            except (Other1, Other2):    # tuple form -- not supported yet
                clause2
            except Exception:
                catch_all

        lowers to a single guard whose handler body chains
        type-check `if`s, falling through to a re-raise when no
        clause matches:

            (guard _exc
              (if <type-check-1>
                  ((lambda (e) clause1) _exc)
                  (if <type-check-2>
                      clause2
                      (if <type-check-3>
                          catch_all
                          (raise _exc))))
              body)

        Type-check rules per clause:

        - No type, or `except Exception:` -- always matches. Treated
          as a catch-all alias so the existing `except Exception:`
          shape keeps absorbing both pyinstance exceptions and the
          legacy py-exception tagged-vector form.
        - `except SomeClass:` where SomeClass is a known class
          (registered in self._defined_classes) -- emits
          `(_pyvelox_isinstance _exc SomeClass)`.
        - Anything else (tuples, attribute access, undefined names)
          is refused at compile time.

        Each clause's `as e:` binding is local to that clause's
        body via an immediate-lambda wrapping. _exception_handler_vars
        is pushed/popped per clause so translate_raise's
        re-raise-passthrough finds the right name.

        Loop-sentinel filter is applied once, at the guard level,
        before any user clause runs -- a break/continue sentinel
        re-raises immediately rather than threading through every
        type check.
        """
        body_bytes = self.translate_block(node.body)

        if not node.handlers:
            return body_bytes

        guard_var = self._GUARD_EXC_VAR

        # Build the dispatch chain bottom-up so each `if` has the
        # next clause (or the final re-raise) as its `else` branch.
        chain = create_inline_call(
            'raise', [encode_symbol(guard_var, self.bc)], self.bc)

        # Track whether we've seen a catch-all -- subsequent clauses
        # would be unreachable. Refuse like CPython does.
        seen_catch_all = False
        for handler in reversed(node.handlers):
            if seen_catch_all:
                # Working bottom-up means the catch-all is *later* in
                # source order than this clause; clauses *after* a
                # catch-all are unreachable. Detect via a forward
                # pass instead of bottom-up. Pre-pass below handles
                # this; nothing to do here.
                pass
            check = self._build_except_type_check(
                handler.type, guard_var)
            body = self._build_except_handler_body(
                handler, guard_var)
            chain = create_inline_call(
                'if', [check, body, chain], self.bc)

        # Pre-pass already done: now check forward order for an
        # earlier catch-all that hides later clauses.
        for i, handler in enumerate(node.handlers[:-1]):
            if self._is_catch_all_handler_type(handler.type):
                raise NotImplementedError(
                    f"`except:` / `except Exception:` clause at "
                    f"position {i + 1} catches everything; later "
                    f"`except` clauses are unreachable.")

        # Loop-sentinel filter wraps the whole dispatch so a
        # break/continue sentinel re-raises before any user clause
        # gets a chance to absorb it.
        if self._loop_depth > 0:
            chain = self._filter_loop_sentinels(guard_var, chain)

        body_ref = self._hoist(body_bytes)
        chain_ref = self._hoist(chain)
        return create_inline_call('guard', [
            encode_symbol(guard_var, self.bc),
            chain_ref,
            body_ref
        ], self.bc)

    def _is_catch_all_handler_type(self, type_node) -> bool:
        """True for `except:` (no type) and `except Exception:`
        (the catch-all alias). Used by translate_try to detect a
        catch-all that would shadow later clauses."""
        if type_node is None:
            return True
        return (isinstance(type_node, ast.Name)
                and type_node.id == 'Exception')

    def _build_except_type_check(self, type_node,
                                 guard_var: str) -> bytes:
        """Return the bytecode for the type-check predicate of a
        single except clause. See translate_try docstring for the
        rules."""
        if self._is_catch_all_handler_type(type_node):
            return encode_boolean(True)
        if isinstance(type_node, ast.Name):
            class_name = type_node.id
            safe = self.get_safe_name(class_name)
            if safe not in self._defined_classes:
                raise NotImplementedError(
                    f"`except {class_name}:` requires {class_name} to "
                    f"be a class defined earlier in the same module. "
                    f"Use `except Exception:` to catch the legacy "
                    f"`raise UnknownName(...)` shape.")
            self._emit_oop_helpers()
            return create_inline_call(
                '_pyvelox_isinstance',
                [encode_symbol(guard_var, self.bc),
                 encode_symbol(safe, self.bc)],
                self.bc)
        if isinstance(type_node, ast.Tuple):
            raise NotImplementedError(
                "`except (A, B):` (tuple of exception types) is not "
                "yet supported. Use one `except` clause per type, or "
                "an `except Exception:` plus `if isinstance(e, ...)` "
                "inside the body.")
        raise NotImplementedError(
            f"Unsupported `except` type filter: "
            f"{type(type_node).__name__}.")

    def _build_except_handler_body(self, handler,
                                   guard_var: str) -> bytes:
        """Translate a single except clause's body. If the handler
        has an `as <name>` binding, wrap the body in
        `((lambda (<name>) body) <guard-var>)` so the name is bound
        per-clause without conflicting across multiple clauses (each
        clause's `as` introduces its own scope)."""
        bound_name = handler.name
        # Use the user's name when present; otherwise push a
        # synthetic 'obj' marker so translate_raise's re-raise-
        # passthrough behaves consistently with pre-typed-handler
        # versions of the compiler. The synthetic name isn't
        # actually written into a let (no AST reference exists for
        # it).
        push_name = bound_name if bound_name else 'obj'
        self._exception_handler_vars.append(push_name)
        try:
            body_bytes = self.translate_block(handler.body)
        finally:
            self._exception_handler_vars.pop()
        if bound_name is None:
            return body_bytes
        # Wrap in immediate-lambda so the user's `as e:` name is in
        # scope only for this clause's body.
        bind_lambda_ref = self._emit_lambda(
            [self.get_safe_name(bound_name)], body_bytes,
            is_function=True)
        return create_inline_call(
            bind_lambda_ref,
            [encode_symbol(guard_var, self.bc)], self.bc)

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

    # Pseudo-modules whose names pyvelox handles syntactically rather
    # than via the VM's library loader. `import math` is a no-op so
    # `math.isqrt(n)` (recognised in translate_call) still works
    # idiomatically.
    _PSEUDO_MODULES = {'math'}

    def translate_import(self, node: ast.Import) -> bytes:
        """Translate `import lib` to the VM's `(import "lib")` primitive.

        The VM's import loads a port-specific library (e.g. "sensors",
        "leds") that contributes operators to the current program's
        symbol table. `import a, b` becomes a begin-sequence.
        `import x as y` is not supported.

        A handful of pseudo-modules (`math`) are handled by the
        compiler directly via attribute-call dispatch and don't go
        through the VM loader; their import statement compiles to a
        no-op so the standard `import math; math.isqrt(...)` shape
        works without needing the VM to define a math library."""
        calls = []
        for alias in node.names:
            if alias.asname is not None:
                raise NotImplementedError(
                    "pyvelox does not support `import ... as ...`; "
                    "port libraries are loaded by their canonical name."
                )
            if alias.name in self._PSEUDO_MODULES:
                # No runtime work needed; the compiler handles
                # math.* calls syntactically.
                continue
            name_bytes = encode_string(alias.name, self.bc)
            calls.append(create_inline_call('import', [name_bytes], self.bc))

        if not calls:
            return encode_boolean(False)
        if len(calls) == 1:
            return calls[0]
        return create_inline_call('begin', calls, self.bc)

    def translate_raise(self, node: ast.Raise) -> bytes:
        """Translate raise into a structured exception object.

        The wire format is a 3-slot tagged vector:
        `#(py-exception type-sym args-list)` -- distinct from the
        R7RS error-object tag so the two coexist. `type-sym` carries
        the exception class name; `args-list` is the positional
        argument list passed to the constructor (CPython's `e.args`).

        Recognised forms:
        - `raise X(a, b, ...)`: build the tagged vector, raise it.
        - `raise X` (bare class name): same shape with empty args.
        - `raise e` where `e` names an exception currently bound by
          an enclosing `except as e:`: passthrough -- raise the
          already-tagged value the handler caught, no re-wrapping.
        - `raise` (bare re-raise): no exception state is tracked
          across handlers, so we synthesise a generic Exception with
          empty args. (Bare re-raise inside a handler with `as e:`
          would be cleaner via `raise e`.)
        - Anything else (e.g. `raise some_func()`): refuse.

        Boundary with break/continue: the loop sentinels still go
        through `_raise_sentinel` directly and stay as bare quoted
        symbols. The tagged-vector shape is only for Python-visible
        exceptions, so the sentinel filter (which compares with eqp)
        keeps working.
        """
        # Bare `raise` -- generic Exception with empty args. When the
        # user has actually defined Exception (or any class extending
        # it), prefer the instance shape; otherwise fall back to the
        # legacy py-exception vector.
        if node.exc is None:
            return create_inline_call(
                'raise', [self._make_exception_value('Exception', [])],
                self.bc)

        # `raise NAME`. Decide passthrough vs class-construction by
        # looking at the active handler stack.
        if isinstance(node.exc, ast.Name):
            if node.exc.id in self._exception_handler_vars:
                # Re-raise of a bound exception variable. Pass the
                # caught value through unchanged.
                return create_inline_call(
                    'raise',
                    [encode_symbol(self.get_safe_name(node.exc.id), self.bc)],
                    self.bc)
            return create_inline_call(
                'raise',
                [self._make_exception_value(node.exc.id, [])], self.bc)

        # `raise NAME(args...)` -- construct a fresh exception object.
        if isinstance(node.exc, ast.Call) and isinstance(node.exc.func, ast.Name):
            return create_inline_call(
                'raise',
                [self._make_exception_value(
                    node.exc.func.id, node.exc.args)],
                self.bc)

        raise NotImplementedError(
            f"raise of {type(node.exc).__name__} is not supported; use "
            f"`raise SomeException(args...)` or `raise <bound name>`.")

    def _make_exception_value(self, type_name: str,
                              arg_nodes: List[ast.expr]) -> bytes:
        """Pick the right wire shape for `raise TYPE(args)`.

        When TYPE names a class registered in self._defined_classes
        (typically Exception or a user subclass thereof), construct
        an instance through _pyvelox_make_instance. The instance's
        slot-alist will receive args/type via Exception's __init__,
        and handler-side `e.args` / `e.type` lookups go through
        _pyvelox_get_attr's instance branch.

        Otherwise (TYPE is a name not associated with a class --
        legacy `raise SomeName("msg")` patterns), fall back to the
        py-exception tagged vector. The handler-side access still
        works because _pyvelox_get_attr recognises both shapes.

        Special case: TYPE == 'Exception' triggers lazy emission of
        the built-in Exception class so even programs that don't
        define classes themselves can use it.
        """
        if type_name == 'Exception':
            self._emit_exception_class()
        safe = self.get_safe_name(type_name)
        if safe in self._defined_classes and type_name not in self.renamed_vars:
            return self._translate_instance_construction(
                type_name, arg_nodes, [])
        return self._make_exception_obj(type_name, arg_nodes)

    def _make_exception_obj(self, type_name: str,
                            arg_nodes: List[ast.expr]) -> bytes:
        """Build `(vector 'py-exception "TYPE" (list args...))` for
        the given type name and positional-argument AST nodes. Each
        arg node is translated through translate_expr_with_ref so
        complex argument expressions get hoisted into the expression
        table.

        The type is stored as a string -- not a symbol -- so user
        code can compare with a plain string literal (`e.type ==
        'ValueError'`); the VM's equalp keeps symbols and strings
        distinct, so a symbol on either side would silently fail.
        The tag in slot 0 is still a symbol because it's an internal
        marker, never compared by user code.
        """
        tag_bytes = create_inline_call(
            'quote', [encode_symbol(self.EXCEPTION_TAG, self.bc)], self.bc)
        type_bytes = encode_string(type_name, self.bc)
        arg_bytes = [self.translate_expr_with_ref(a) for a in arg_nodes]
        args_list = create_inline_call('list', arg_bytes, self.bc)
        return create_inline_call(
            'vector', [tag_bytes, type_bytes, args_list], self.bc)

    def translate_subscript(self, node: ast.Subscript) -> bytes:
        """
        Translate subscripting:
        lst[idx] -> (list-ref lst idx)
        lst[start:end] -> slice implementation
        dict[key] -> (cdr (assoc key dict)) for string keys
        b[i] (bytes) -> char->integer of vector-ref.
        Complex operands stored separately.
        """
        value_bytes = self.translate_expr_with_ref(node.value)

        # Check if this is a slice operation
        if isinstance(node.slice, ast.Slice):
            return self.translate_slice(value_bytes, node.slice)

        # Detect dict access: if index is a string, use assoc
        # d['key'] -> (cdr (assoc 'key d))
        # lst[0] -> (list-ref lst 0)
        is_string_key = isinstance(node.slice, ast.Constant) and isinstance(node.slice.value, str)

        if is_string_key:
            index_bytes = self.translate_expr_with_ref(node.slice)
            # Dict access: (cdr (assoc key dict))
            assoc_bytes = create_inline_call('assoc', [index_bytes, value_bytes], self.bc)
            # Store assoc call separately
            assoc_ref = self._hoist(assoc_bytes)
            # Get the value from the pair with cdr
            return create_inline_call('cdr', [assoc_ref], self.bc)

        # Sequence access. `list-ref` handles lists and strings;
        # `bytes` is a buffer-flagged vector and needs `vector-ref`,
        # which on a buffer returns a character — convert back to the
        # int the Python user expects. Dispatch on `vectorp` so
        # `lst[i]` and `b[i]` both work. Both the receiver and the
        # index are read in two branches plus the predicate, so each
        # is wrapped in `_evaluate_once` to avoid re-executing the
        # underlying expression three times.
        def build_with_idx(idx_token: bytes) -> bytes:
            def build_with_recv(recv_token: bytes) -> bytes:
                vec_ref = create_inline_call(
                    'vector_ref', [recv_token, idx_token], self.bc)
                vec_branch = create_inline_call(
                    'char_to_integer', [vec_ref], self.bc)
                list_branch = create_inline_call(
                    'list_ref', [recv_token, idx_token], self.bc)
                return self._emit_type_dispatch(
                    recv_token,
                    [('vectorp', vec_branch)],
                    list_branch)
            return self._evaluate_once(node.value, build_with_recv)
        return self._evaluate_once(node.slice, build_with_idx)

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
                # For slicing without end, use slice with length as end.
                # Works for lists, strings, vectors, and bytes — but
                # the bare `length` primitive rejects vectors, so we
                # dispatch on `vectorp` (true for buffers and regular
                # vectors alike) and fall through to `length` for
                # lists and strings. Same shape as `translate_len`.
                length_call = create_inline_call(
                    'if',
                    [create_inline_call('vectorp', [value_bytes], self.bc),
                     create_inline_call('vector_length',
                                        [value_bytes], self.bc),
                     create_inline_call('length',
                                        [value_bytes], self.bc)],
                    self.bc)
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

        All attribute reads route through the `_pyvelox_get_attr`
        runtime helper, which inspects the receiver's tag and
        chooses the right backing storage:

        - py-exception tagged vector (legacy `raise X(args)` shape
          for non-class names): slot 1 holds the type string, slot
          2 the args list, exposed as `.type` / `.args`.
        - pyinstance tagged vector (class instances, including
          custom exception classes): looks `name` up in the
          slot-alist.
        - anything else: AttributeError raised through the
          structured-exception machinery.

        Method-table lookup is *not* part of this path; bound-method
        values aren't supported, only `obj.method(args)` in call
        position via translate_call's user-method dispatch.

        Emit the OOP helpers if they aren't already in the prologue
        -- a program that only catches exceptions and reads `e.args`
        wouldn't otherwise pull them in.
        """
        self._emit_oop_helpers()
        recv_bytes = self.translate_expr_with_ref(node.value)
        name_quoted = create_inline_call(
            'quote',
            [encode_symbol(node.attr, self.bc)], self.bc)
        return create_inline_call(
            '_pyvelox_get_attr', [recv_bytes, name_quoted], self.bc)

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
