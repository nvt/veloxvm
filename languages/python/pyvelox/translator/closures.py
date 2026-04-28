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

"""Lexical-scope analysis and box-rewrite for PythonTranslator.

A coherent subsystem covering:

- `_analyze_body` — one AST walk that returns a function/lambda
  body's free-variable captures and the locals that must live in
  heap boxes (captured by an inner closure AND mutated).
- `collect_assigned_vars` — names a body assigns to, used for
  function-local hoisting and as input to `_analyze_body`'s nested-
  function bookkeeping.
- `_is_boxed` / `_emit_name_load` — runtime side of box-rewrite:
  decides whether a Load needs to go through `(box_ref name)` based
  on whether the binding's current scope flagged it as boxed.
- `_make_box_init_bytes` — function-entry `(set! name (box name))`
  wrap emitted for each boxed parameter or local.

The class is a mixin: PythonTranslator inherits from it and uses
the inherited methods as if they were defined directly on the class.
"""

import ast
from typing import List, Set
from ..encoder import (
    encode_symbol, create_inline_call,
)


class _ClosureAnalysis:
    """See module docstring."""

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

    def collect_assigned_vars(self, stmts: List[ast.stmt]) -> Set[str]:
        """Names that a list of statements assigns to.

        Used for Python's function-level scoping — every assigned
        name needs to be hoisted to the top of the enclosing
        function's let-expansion, even if the assignment is buried
        inside an `if`, `for`, or `try`.

        Names declared `nonlocal` or `global` are excluded: they
        refer to bindings in an enclosing function or to module-
        level bindings, not to a function-local.

        Returns safe names (with `py_` prefix if they collide with a
        VM primitive).
        """
        assigned: Set[str] = set()
        nonlocal_names: Set[str] = set()
        global_names: Set[str] = set()

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
                # A nested def binds its name in the enclosing
                # function's scope; we don't recurse into the def's
                # body — that's a separate scope.
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

    def _is_boxed(self, safe_name: str) -> bool:
        """True if `safe_name` resolves to a binding that lives in a
        heap box at this point in translation. Walks the scope stack
        inward-to-outward and checks the box flag of the innermost
        scope that contains the name — so a shadowing local in an
        inner scope correctly hides an outer boxed binding.
        """
        for i in range(len(self.scope_stack) - 1, -1, -1):
            if safe_name in self.scope_stack[i]:
                return safe_name in self.boxed_stack[i]
        return False

    def _emit_name_load(self, safe_name: str) -> bytes:
        """Emit bytecode for a Load of `safe_name`. If the binding is
        boxed, wrap in `(box_ref name)`; otherwise emit the bare
        symbol.
        """
        if self._is_boxed(safe_name):
            return create_inline_call(
                'box_ref', [encode_symbol(safe_name, self.bc)], self.bc)
        return encode_symbol(safe_name, self.bc)

    def _make_box_init_bytes(self, safe_name: str) -> bytes:
        """Emit `(set! name (box name))` — the function-entry wrap
        that replaces the initial value of `name` with a box
        containing it. Subsequent reads of `name` are rewritten to
        `(box_ref name)` by `_emit_name_load`. The inner `(box name)`
        uses a plain symbol reference; we can't route that through
        `_emit_name_load` because at this point `name` still holds
        its unboxed value.
        """
        box_call = create_inline_call(
            'box', [encode_symbol(safe_name, self.bc)], self.bc)
        return create_inline_call(
            'set',
            [encode_symbol(safe_name, self.bc), box_call],
            self.bc)
