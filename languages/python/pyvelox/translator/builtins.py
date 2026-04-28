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


"""Per-built-in handlers for PythonTranslator.

Holds the dispatch table and translation logic for top-level
built-in functions (`print`, `len`, `range`, `int`, `str`, `abs`,
`min`/`max`, the higher-order trio, comprehensions, ...). The class
is a mixin: PythonTranslator inherits from it and uses the inherited
methods as if they were defined directly on the class.
"""

import ast
from typing import Callable, Dict, List, Optional, Set
from ..encoder import (
    encode_integer, encode_boolean, encode_string, encode_symbol,
    encode_character, encode_form_ref, create_inline_call,
)


class _BuiltinHandlers:
    """See module docstring."""

    _BUILTIN_HANDLERS: Dict[str, str] = {
        'print':     'translate_print',
        'len':       'translate_len',
        'range':     'translate_range',
        'int':       'translate_int',
        'str':       'translate_str',
        'abs':       'translate_abs',
        'min':       'translate_min',
        'max':       'translate_max',
        'sum':       'translate_sum',
        'sorted':    'translate_sorted',
        'reversed':  'translate_reversed',
        'map':       'translate_map',
        'filter':    'translate_filter',
        'reduce':    'translate_reduce',
        'all':       'translate_all',
        'any':       'translate_any',
        'list':      'translate_list_constructor',
        'enumerate': 'translate_enumerate',
        'zip':       'translate_zip',
    }

    def translate_print(self, args: List[ast.expr]) -> bytes:
        """
        Translate print(a, b, c) -> (begin (print a b c) (write-char #\newline)).

        Python's print adds a newline, but VeloxVM's print primitive doesn't,
        so we need to add a newline character explicitly.

        For print() with no arguments, just write a newline.
        """
        # Handle empty print() - just write newline
        if len(args) == 0:
            newline_char = encode_character('\n')
            return create_inline_call('write_char', [newline_char], self.bc)

        arg_bytes = [self.translate_expr_with_ref(arg) for arg in args]
        print_call = create_inline_call('print', arg_bytes, self.bc)

        # Store print call as separate expression
        print_ref = self._hoist(print_call)

        # Add newline: (write-char #\newline)
        newline_char = encode_character('\n')
        newline_call = create_inline_call('write_char', [newline_char], self.bc)

        # Store newline call as separate expression
        newline_ref = self._hoist(newline_call)

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
        """Translate `range(stop)` / `range(start, stop)` /
        `range(start, stop, step)`.

        A small literal `range(N)` constant-folds to
        `(list 0 1 ... N-1)`. Everything else goes through a
        per-module `_pyvelox_range` helper emitted into the program
        prologue on first use.
        """
        if not 1 <= len(args) <= 3:
            raise ValueError(
                f"range() takes 1 to 3 arguments ({len(args)} given)")

        # Constant fast path: range(N) where N is a small integer
        # literal. Avoids materializing huge inline lists.
        if (len(args) == 1
                and isinstance(args[0], ast.Constant)
                and isinstance(args[0].value, int)
                and not isinstance(args[0].value, bool)
                and 0 <= args[0].value <= self._RANGE_LITERAL_LIMIT):
            elements = [encode_integer(i) for i in range(args[0].value)]
            return create_inline_call('list', elements, self.bc)

        self._emit_range_helper()

        if len(args) == 1:
            start_b = encode_integer(0)
            stop_b = self.translate_expr_with_ref(args[0])
            step_b = encode_integer(1)
        elif len(args) == 2:
            start_b = self.translate_expr_with_ref(args[0])
            stop_b = self.translate_expr_with_ref(args[1])
            step_b = encode_integer(1)
        else:
            start_b = self.translate_expr_with_ref(args[0])
            stop_b = self.translate_expr_with_ref(args[1])
            step_b = self.translate_expr_with_ref(args[2])

        return create_inline_call(
            '_pyvelox_range', [start_b, stop_b, step_b], self.bc)

    def _emit_range_helper(self) -> None:
        """Emit a top-level `_pyvelox_range(start, stop, step)` helper
        that returns `[start, start+step, ..., stop)` as a list. Idempotent
        — only the first call adds bytecode to the prologue.

        The implementation is two top-level defines:

            (define _pyvelox_range_loop
              (lambda (i stop step acc)
                (if (or (and (> step 0) (>= i stop))
                        (and (< step 0) (<= i stop)))
                    (reverse acc)
                    (_pyvelox_range_loop (+ i step) stop step
                                         (cons i acc)))))

            (define _pyvelox_range
              (lambda (start stop step)
                (_pyvelox_range_loop start stop step (list))))

        stop and step are passed through the recursion so the inner
        lambda doesn't have to capture them — keeping us off the
        closure-materialisation path.
        """
        if '_pyvelox_range' in self._emitted_helpers:
            return
        self._emitted_helpers.add('_pyvelox_range')

        sym_i = lambda: encode_symbol('i', self.bc)
        sym_stop = lambda: encode_symbol('stop', self.bc)
        sym_step = lambda: encode_symbol('step', self.bc)
        sym_acc = lambda: encode_symbol('acc', self.bc)
        sym_start = lambda: encode_symbol('start', self.bc)

        # Termination predicate: counting up past stop, or counting
        # down past stop. step=0 falls through and recurses forever;
        # callers shouldn't pass it (Python raises ValueError there).
        pos_done = create_inline_call(
            'and',
            [create_inline_call('greater_than',
                                [sym_step(), encode_integer(0)], self.bc),
             create_inline_call('greater_than_equal',
                                [sym_i(), sym_stop()], self.bc)],
            self.bc)
        neg_done = create_inline_call(
            'and',
            [create_inline_call('less_than',
                                [sym_step(), encode_integer(0)], self.bc),
             create_inline_call('less_than_equal',
                                [sym_i(), sym_stop()], self.bc)],
            self.bc)
        done_test = create_inline_call('or', [pos_done, neg_done], self.bc)

        rev_acc = create_inline_call('reverse', [sym_acc()], self.bc)

        next_i = create_inline_call('add', [sym_i(), sym_step()], self.bc)
        next_acc = create_inline_call('cons', [sym_i(), sym_acc()], self.bc)
        recurse = create_inline_call(
            '_pyvelox_range_loop',
            [next_i, sym_stop(), sym_step(), next_acc],
            self.bc)

        loop_body = create_inline_call(
            'if', [done_test, rev_acc, recurse], self.bc)
        loop_lambda_ref = self._emit_lambda(
            ['i', 'stop', 'step', 'acc'], loop_body, is_function=True)
        define_loop = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_range_loop', self.bc),
             loop_lambda_ref],
            self.bc)

        outer_call = create_inline_call(
            '_pyvelox_range_loop',
            [sym_start(), sym_stop(), sym_step(),
             create_inline_call('list', [], self.bc)],
            self.bc)
        outer_lambda_ref = self._emit_lambda(
            ['start', 'stop', 'step'], outer_call, is_function=True)
        define_outer = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_range', self.bc), outer_lambda_ref],
            self.bc)

        self._preamble.extend(define_loop)
        self._preamble.extend(define_outer)

    def translate_int(self, args: List[ast.expr]) -> bytes:
        """Translate `int(x)`.

        Python's `int(x)` accepts ints, strings, and bools. Literal
        arguments resolve at compile time; variables get a runtime
        type-dispatch:

            (if (numberp x) x
              (if (stringp x) (string-to-number x)
                (if x 1 0)))

        The fallback branch coerces by truthiness, matching
        `int(True) == 1` / `int(False) == 0`. Calling `int()` on a
        list or None falls into the truthiness branch instead of
        raising — a documented divergence from CPython.
        """
        if len(args) != 1:
            raise ValueError("int() takes exactly 1 argument")

        arg = args[0]

        if isinstance(arg, ast.Constant):
            value = arg.value
            # bool is an int subclass — check first.
            if isinstance(value, bool):
                return encode_integer(1 if value else 0)
            if isinstance(value, int):
                return encode_integer(value)
            if isinstance(value, str):
                return create_inline_call(
                    'string_to_number',
                    [self.translate_expr_with_ref(arg)],
                    self.bc)
            # Other constants (None, floats) fall through to runtime.

        def build(arg_token: bytes) -> bytes:
            # (if x 1 0) — booleans and any truthy value, degrades
            # gracefully for unsupported types.
            bool_branch = self._hoist(create_inline_call(
                'if', [arg_token, encode_integer(1), encode_integer(0)],
                self.bc))
            # (string-to-number x)
            str_branch = self._hoist(create_inline_call(
                'string_to_number', [arg_token], self.bc))
            # (stringp x)
            str_test = self._hoist(create_inline_call(
                'stringp', [arg_token], self.bc))
            # (if (stringp x) (string-to-number x) bool_branch)
            inner_if = self._hoist(create_inline_call(
                'if', [str_test, str_branch, bool_branch], self.bc))
            # (numberp x)
            num_test = self._hoist(create_inline_call(
                'numberp', [arg_token], self.bc))
            # (if (numberp x) x inner_if)
            return create_inline_call(
                'if', [num_test, arg_token, inner_if], self.bc)

        # Evaluate `arg` exactly once before the dispatch reads it
        # five times — otherwise side effects in the argument would
        # fire repeatedly.
        return self._evaluate_once(arg, build)

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
        as `False` in PyVelox, so `str(None)` on a variable yields
        "False" — see the `None`/`bool` row of doc/python.md's status
        table.
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

        def build(arg_token: bytes) -> bytes:
            # (if x "True" "False") — booleans
            bool_branch = self._hoist(create_inline_call(
                'if',
                [arg_token,
                 encode_string("True", self.bc),
                 encode_string("False", self.bc)],
                self.bc))
            # (number-to-string x)
            num_branch = self._hoist(create_inline_call(
                'number_to_string', [arg_token], self.bc))
            # (booleanp x)
            bool_test = self._hoist(create_inline_call(
                'booleanp', [arg_token], self.bc))
            # (if (booleanp x) bool_branch num_branch)
            inner_if = self._hoist(create_inline_call(
                'if', [bool_test, bool_branch, num_branch], self.bc))
            # (stringp x)
            str_test = self._hoist(create_inline_call(
                'stringp', [arg_token], self.bc))
            # (if (stringp x) x inner_if)
            return create_inline_call(
                'if', [str_test, arg_token, inner_if], self.bc)

        # Evaluate `arg` exactly once — the dispatch reads it from
        # multiple branches and we don't want side effects to fire
        # more than once.
        return self._evaluate_once(arg, build)

    def translate_abs(self, args: List[ast.expr]) -> bytes:
        """Translate `abs(x)` to `(if (< x 0) (- 0 x) x)`.

        Evaluates `x` exactly once: the comparison, the negation, and
        the else branch all read it, so a side-effecting argument
        could otherwise fire three times.
        """
        if len(args) != 1:
            raise ValueError("abs() takes exactly 1 argument")

        zero_bytes = encode_integer(0)

        def build(arg_token: bytes) -> bytes:
            cmp_ref = self._hoist(create_inline_call(
                'less_than', [arg_token, zero_bytes], self.bc))
            neg_ref = self._hoist(create_inline_call(
                'subtract', [zero_bytes, arg_token], self.bc))
            return create_inline_call(
                'if', [cmp_ref, neg_ref, arg_token], self.bc)

        return self._evaluate_once(args[0], build)

    def _translate_fold_min_max(self, iterable: ast.expr, cmp_op: str) -> bytes:
        """Compile min(iterable)/max(iterable) as
        (reduce (lambda (a b) (if (cmp a b) a b)) iterable).
        cmp_op is 'less_than' for min, 'greater_than' for max."""
        list_bytes = self.translate_expr_with_ref(iterable)

        a_sym = encode_symbol('a', self.bc)
        b_sym = encode_symbol('b', self.bc)

        cmp_call = create_inline_call(cmp_op, [a_sym, b_sym], self.bc)
        cmp_ref = self._hoist(cmp_call)

        if_form = create_inline_call('if', [cmp_ref, a_sym, b_sym], self.bc)
        lambda_bytes = self._emit_lambda(['a', 'b'], if_form)

        return create_inline_call('reduce', [lambda_bytes, list_bytes], self.bc)

    def _translate_min_max(self, args: List[ast.expr],
                           cmp_op: str, label: str) -> bytes:
        """Common implementation for `min()` and `max()`.

        - `min(iterable)` / `max(iterable)` fold with a two-arg lambda.
        - Two or more positional args build a right-associated chain:
              cmp(a, b)        -> (if (cmp a b) a b)
              cmp(a, b, c, …)  -> (if (cmp a chain(b, c, …))
                                       a chain(b, c, …))

        `cmp_op` selects `less_than` (min) or `greater_than` (max);
        `label` is used in error messages.
        """
        if not args:
            raise ValueError(f"{label}() requires at least 1 argument")
        if len(args) == 1:
            return self._translate_fold_min_max(args[0], cmp_op)

        arg_bytes = [self.translate_expr_with_ref(arg) for arg in args]

        def build(items):
            if len(items) == 1:
                return items[0]
            first = items[0]
            rest = items[1] if len(items) == 2 else self._hoist(build(items[1:]))
            cmp_ref = self._hoist(
                create_inline_call(cmp_op, [first, rest], self.bc))
            return create_inline_call('if', [cmp_ref, first, rest], self.bc)

        return build(arg_bytes)

    def translate_min(self, args: List[ast.expr]) -> bytes:
        """Translate `min(...)` — see `_translate_min_max`."""
        return self._translate_min_max(args, 'less_than', 'min')

    def translate_max(self, args: List[ast.expr]) -> bytes:
        """Translate `max(...)` — see `_translate_min_max`."""
        return self._translate_min_max(args, 'greater_than', 'max')

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
        lambda_bytes = self._emit_lambda(['a', 'b'], and_call)

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
        lambda_bytes = self._emit_lambda(['a', 'b'], or_call)

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
                filter_lambda_bytes = self._emit_lambda([safe_var], filter_body)

                # Apply filter: (filter lambda iter)
                filtered = create_inline_call('filter', [filter_lambda_bytes, result_iter], self.bc)
                result_iter = self._hoist(filtered)

            # Create map lambda: (lambda (var) elt)
            elt_body = self.translate_expr(node.elt)
            map_lambda_bytes = self._emit_lambda([safe_var], elt_body)

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
                        filter_lambda_bytes = self._emit_lambda([safe_var], filter_body)

                        filtered = create_inline_call('filter', [filter_lambda_bytes, filtered_iter], self.bc)
                        filtered_iter = self._hoist(filtered)

                    # Create list of singleton tuples
                    singleton_body = create_inline_call('list', [encode_symbol(safe_var, self.bc)], self.bc)
                    singleton_lambda = self._emit_lambda([safe_var], singleton_body)

                    result = self._hoist(create_inline_call(
                        'map', [singleton_lambda, filtered_iter], self.bc))
                else:
                    # Outer generators: cartesian product
                    # This is getting complex - for now raise an error
                    raise NotImplementedError("Multiple generators in list comprehension not yet fully implemented")

            # Finally, map the element expression over the cartesian product
            # Extract all variables and create the final lambda
            all_vars = [self.get_safe_name(g.target.id) for g in reversed(gens)]

            # This is complex - for now just raise an error
            raise NotImplementedError("Multiple generators in list comprehension not yet fully implemented")
