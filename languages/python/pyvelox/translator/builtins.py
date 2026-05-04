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
        'bytes':     'translate_bytes',
        'bytearray': 'translate_bytearray',
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
        """Translate `len(x)`.

        The VM's `length` primitive only handles lists and strings, but
        `bytes` objects are buffer-flagged vectors — they need
        `vector-length`. Emit a runtime dispatch so `len` works for
        all four sequence shapes:

            (if (vectorp x) (vector-length x) (length x))

        `vectorp` is true for both regular and buffer-flagged vectors,
        so this single test covers `bytes` plus any vector the user
        gets from a port library.
        """
        if len(args) != 1:
            raise ValueError("len() takes exactly 1 argument")

        def build(arg_token: bytes) -> bytes:
            vec_len = create_inline_call(
                'vector_length', [arg_token], self.bc)
            list_or_str_len = create_inline_call(
                'length', [arg_token], self.bc)
            return self._emit_type_dispatch(
                arg_token,
                [('vectorp', vec_len)],
                list_or_str_len)

        return self._evaluate_once(args[0], build)

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
            # (numberp x) -> x ;; already a number
            # (stringp x) -> (string-to-number x)
            # (else)      -> (if x 1 0) ;; booleans + truthy fallback
            return self._emit_type_dispatch(
                arg_token,
                [('numberp', arg_token),
                 ('stringp', create_inline_call(
                     'string_to_number', [arg_token], self.bc))],
                create_inline_call(
                    'if',
                    [arg_token, encode_integer(1), encode_integer(0)],
                    self.bc))

        # Evaluate `arg` once before the dispatch reads it from
        # multiple branches.
        return self._evaluate_once(arg, build)

    def translate_str(self, args: List[ast.expr]) -> bytes:
        """Translate `str(x)`.

        Python `str(x)` accepts any type. Compile-time fast paths
        cover literals (str / bool / None / int); for variables we
        emit a call into the `_pyvelox_str` runtime helper, which
        dispatches on type at runtime:

        - already a string -> return as-is
        - boolean          -> "True" / "False"
        - py-exception     -> args[0] coerced to string, or "" if
                              args is empty
        - otherwise        -> number-to-string (errors at runtime
                              for lists/dicts/etc)

        The exception case is what makes `str(e)` and `f"{e}"` show
        the user's message instead of the bare tagged-vector
        representation. Multi-arg exceptions (`raise X("a", "b")`)
        return the first arg only -- a documented deviation from
        CPython's repr-of-args formatting.
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
            # Other constant types fall through to the runtime helper.

        # Variable / general-expression path: dispatch happens inside
        # the runtime helper so f-strings and other call sites share
        # the same code without inflating bytecode at every callsite.
        self._emit_str_helper()
        arg_bytes = self.translate_expr_with_ref(arg)
        return create_inline_call(
            '_pyvelox_str', [arg_bytes], self.bc)

    def _emit_str_helper(self) -> None:
        """Emit the `_pyvelox_str` and `_pyvelox_str_value` helpers
        into the program prologue. Idempotent.

        `_pyvelox_str_value` does the existing string/bool/number
        dispatch in isolation. `_pyvelox_str` adds an outer check for
        a py-exception tagged vector and routes through args[0] when
        one is found, falling back to the value dispatch for
        everything else:

            (define _pyvelox_str_value
              (lambda (x)
                (if (stringp x) x
                  (if (booleanp x) (if x "True" "False")
                    (number-to-string x)))))

            (define _pyvelox_str
              (lambda (x)
                (if (and (vectorp x)
                         (= (vector-length x) 3)
                         (eqp (vector-ref x 0)
                              (quote py-exception)))
                    (let ((args (vector-ref x 2)))
                      (if (nullp args) ""
                        (_pyvelox_str_value (car args))))
                    (_pyvelox_str_value x))))

        and short-circuits in the VM, so vector-length / vector-ref
        only fire after vectorp confirms the type.
        """
        if '_pyvelox_str' in self._emitted_helpers:
            return
        self._emitted_helpers.add('_pyvelox_str')

        sym_x = lambda: encode_symbol('x', self.bc)
        sym_args = lambda: encode_symbol('args', self.bc)

        # _pyvelox_str_value: the original 3-way dispatch.
        value_body = create_inline_call(
            'if',
            [create_inline_call('stringp', [sym_x()], self.bc),
             sym_x(),
             create_inline_call(
                 'if',
                 [create_inline_call('booleanp', [sym_x()], self.bc),
                  create_inline_call(
                      'if',
                      [sym_x(),
                       encode_string("True", self.bc),
                       encode_string("False", self.bc)],
                      self.bc),
                  create_inline_call(
                      'number_to_string', [sym_x()], self.bc)],
                 self.bc)],
            self.bc)
        value_lambda_ref = self._emit_lambda(
            ['x'], value_body, is_function=True)
        define_value = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_str_value', self.bc),
             value_lambda_ref],
            self.bc)
        self._preamble.extend(define_value)

        # _pyvelox_str: outer check for the py-exception tag.
        tag_quoted = create_inline_call(
            'quote',
            [encode_symbol('py-exception', self.bc)], self.bc)
        is_exc = create_inline_call(
            'and',
            [create_inline_call('vectorp', [sym_x()], self.bc),
             create_inline_call(
                 'equal',
                 [create_inline_call(
                      'vector_length', [sym_x()], self.bc),
                  encode_integer(3)],
                 self.bc),
             create_inline_call(
                 'eqp',
                 [create_inline_call(
                      'vector_ref',
                      [sym_x(), encode_integer(0)], self.bc),
                  tag_quoted],
                 self.bc)],
            self.bc)

        # Inner: extract args, return car or "".
        get_args = create_inline_call(
            'vector_ref',
            [sym_x(), encode_integer(2)], self.bc)
        car_args = create_inline_call('car', [sym_args()], self.bc)
        format_one = create_inline_call(
            '_pyvelox_str_value', [car_args], self.bc)
        format_args = create_inline_call(
            'if',
            [create_inline_call('nullp', [sym_args()], self.bc),
             encode_string("", self.bc),
             format_one],
            self.bc)
        # Wrap in a let so `args` is bound once.
        # let-as-immediate-lambda: ((lambda (args) <body>) <get-args>)
        format_lambda_ref = self._emit_lambda(
            ['args'], format_args, is_function=True)
        format_call = create_inline_call(
            format_lambda_ref, [get_args], self.bc)

        # Fallback: route through _pyvelox_str_value.
        fallback = create_inline_call(
            '_pyvelox_str_value', [sym_x()], self.bc)

        outer_body = create_inline_call(
            'if', [is_exc, format_call, fallback], self.bc)
        outer_lambda_ref = self._emit_lambda(
            ['x'], outer_body, is_function=True)
        define_outer = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_str', self.bc),
             outer_lambda_ref],
            self.bc)
        self._preamble.extend(define_outer)

    def translate_bytes(self, args: List[ast.expr]) -> bytes:
        """Translate `bytes(...)`.

        Lowers to a buffer-flagged vector — the same VM storage R7RS
        bytevectors use. Recognised forms:

        - `bytes()`            -> `(make-buffer 0)`
        - `bytes(int_lit)`     -> `(make-buffer N)` (zero-filled)
        - `bytes([b1, b2, …])` -> `(_pyvelox_bytes_from_list (list b1 b2 …))`
        - `bytes(b'...')`      -> the literal itself
        - `bytes(x)` (variable) -> runtime dispatch on `numberp` /
          `bufferp`, falling through to the from-list helper.

        CPython copies on `bytes(b'...')`; we return the same buffer.
        Documented divergence — bytes are notionally immutable, so
        sharing storage is observable only through `vector-set!`,
        which Python user code can't reach.
        """
        if len(args) > 1:
            raise ValueError(
                f"bytes() takes 0 or 1 arguments ({len(args)} given)")

        if not args:
            return create_inline_call(
                'make_buffer', [encode_integer(0)], self.bc)

        arg = args[0]

        # Compile-time fast paths: source already tells us the shape.
        if isinstance(arg, ast.Constant):
            value = arg.value
            if isinstance(value, bool):
                # bool is an int subclass; bytes(True/False) is 0/1
                # bytes in CPython. We refuse rather than silently
                # interpret it that way.
                raise NotImplementedError(
                    "bytes(bool) is ambiguous in pyvelox; pass an "
                    "explicit int (e.g. bytes(int(b))) or a list")
            if isinstance(value, int):
                # `-1` parses as UnaryOp, not Constant, so we never
                # see a negative literal here. The VM's make-buffer
                # rejects negatives at runtime anyway.
                return create_inline_call(
                    'make_buffer',
                    [self.translate_expr_with_ref(arg)], self.bc)
            if isinstance(value, bytes):
                # Recurses through translate_constant.
                return self.translate_expr_with_ref(arg)

        if isinstance(arg, (ast.List, ast.Tuple)):
            self._emit_bytes_helper()
            list_bytes = self.translate_expr_with_ref(arg)
            return create_inline_call(
                '_pyvelox_bytes_from_list', [list_bytes], self.bc)

        # Variable / general expression: dispatch at runtime.
        self._emit_bytes_helper()

        def build(arg_token: bytes) -> bytes:
            # (numberp x) -> (make-buffer x)
            # (bufferp x) -> x ;; already bytes; share storage
            # else        -> (_pyvelox_bytes_from_list x)
            return self._emit_type_dispatch(
                arg_token,
                [('numberp', create_inline_call(
                    'make_buffer', [arg_token], self.bc)),
                 ('bufferp', arg_token)],
                create_inline_call(
                    '_pyvelox_bytes_from_list', [arg_token], self.bc))

        return self._evaluate_once(arg, build)

    def translate_bytearray(self, args: List[ast.expr]) -> bytes:
        """`bytearray` isn't yet distinguished from `bytes` — the VM
        has only one buffer type and exposing both names without
        enforcing mutability semantics would mislead users."""
        raise NotImplementedError(
            "bytearray is not yet supported in pyvelox; use bytes()")

    def _emit_bytes_helper(self) -> None:
        """Emit the `_pyvelox_bytes_from_list` helper into the
        program prologue. Idempotent.

        The helper allocates a buffer of the list's length and writes
        each element (an int 0..255 or a character) into it via
        `vector-set!`. Representation matches R7RS bytevectors —
        buffer-flagged vectors. The loop is split out as a separate
        recursive helper so it doesn't capture any free variables, the
        same shape `_pyvelox_range` uses.

            (define _pyvelox_bytes_loop
              (lambda (lst buf i)
                (if (null? lst)
                    buf
                    (begin
                      (vector-set! buf i (car lst))
                      (_pyvelox_bytes_loop (cdr lst) buf (+ i 1))))))

            (define _pyvelox_bytes_from_list
              (lambda (lst)
                (_pyvelox_bytes_loop
                  lst (make-buffer (length lst)) 0)))
        """
        if '_pyvelox_bytes_from_list' in self._emitted_helpers:
            return
        self._emitted_helpers.add('_pyvelox_bytes_from_list')

        sym_lst = lambda: encode_symbol('lst', self.bc)
        sym_buf = lambda: encode_symbol('buf', self.bc)
        sym_i = lambda: encode_symbol('i', self.bc)

        # (vector-set! buf i (car lst))
        car_lst = create_inline_call('car', [sym_lst()], self.bc)
        set_call = create_inline_call(
            'vector_set', [sym_buf(), sym_i(), car_lst], self.bc)

        # (_pyvelox_bytes_loop (cdr lst) buf (+ i 1))
        cdr_lst = create_inline_call('cdr', [sym_lst()], self.bc)
        next_i = create_inline_call(
            'add', [sym_i(), encode_integer(1)], self.bc)
        recurse = create_inline_call(
            '_pyvelox_bytes_loop',
            [cdr_lst, sym_buf(), next_i], self.bc)

        # (begin (vector-set! ...) (recurse ...))
        step = create_inline_call(
            'begin', [set_call, recurse], self.bc)

        null_test = create_inline_call('nullp', [sym_lst()], self.bc)
        loop_body = create_inline_call(
            'if', [null_test, sym_buf(), step], self.bc)
        loop_lambda_ref = self._emit_lambda(
            ['lst', 'buf', 'i'], loop_body, is_function=True)
        define_loop = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_bytes_loop', self.bc),
             loop_lambda_ref],
            self.bc)

        # Outer constructor.
        len_call = create_inline_call('length', [sym_lst()], self.bc)
        make_buf = create_inline_call(
            'make_buffer', [len_call], self.bc)
        outer_call = create_inline_call(
            '_pyvelox_bytes_loop',
            [sym_lst(), make_buf, encode_integer(0)], self.bc)
        outer_lambda_ref = self._emit_lambda(
            ['lst'], outer_call, is_function=True)
        define_outer = create_inline_call(
            'define',
            [encode_symbol('_pyvelox_bytes_from_list', self.bc),
             outer_lambda_ref],
            self.bc)

        self._preamble.extend(define_loop)
        self._preamble.extend(define_outer)

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
