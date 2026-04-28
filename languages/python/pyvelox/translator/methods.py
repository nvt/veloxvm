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


"""Per-receiver method handlers for PythonTranslator.

Holds the dispatch table and translation logic for `obj.method(...)`
calls on Python's built-in receiver types (dict, list, str). The
class is a mixin: PythonTranslator inherits from it and uses the
inherited methods as if they were defined directly on the class.
"""

import ast
from typing import Callable, Dict, List, Optional, Set
from ..encoder import (
    encode_integer, encode_boolean, encode_string, encode_symbol,
    encode_form_ref, create_inline_call,
)


class _MethodHandlers:
    """See module docstring."""

    _METHOD_HANDLERS: Dict[str, Callable] = {
        # Dict methods
        'keys':       lambda t, o, a: t.translate_dict_keys(o),
        'values':     lambda t, o, a: t.translate_dict_values(o),
        'items':      lambda t, o, a: t.translate_dict_items(o),
        'get':        lambda t, o, a: t.translate_dict_get(o, a),
        # List methods
        'append':     lambda t, o, a: t.translate_list_append(o, a),
        'extend':     lambda t, o, a: t.translate_list_extend(o, a),
        'pop':        lambda t, o, a: t.translate_list_pop(o, a),
        'remove':     lambda t, o, a: t.translate_list_remove(o, a),
        'reverse':    lambda t, o, a: t.translate_list_reverse(o),
        'count':      lambda t, o, a: t.translate_list_count(o, a),
        'index':      lambda t, o, a: t.translate_list_index(o, a),
        'insert':     lambda t, o, a: t.translate_list_insert(o, a),
        # String methods
        'upper':      lambda t, o, a: t.translate_string_upper(o),
        'lower':      lambda t, o, a: t.translate_string_lower(o),
        'split':      lambda t, o, a: t.translate_string_split(o, a),
        'join':       lambda t, o, a: t.translate_string_join(o, a),
        'startswith': lambda t, o, a: t.translate_string_startswith(o, a),
        'endswith':   lambda t, o, a: t.translate_string_endswith(o, a),
        'strip':      lambda t, o, a: t.translate_string_strip(o),
        'replace':    lambda t, o, a: t.translate_string_replace(o, a),
    }

    def translate_dict_keys(self, dict_expr: ast.expr) -> bytes:
        """
        Translate d.keys() -> (map car d)
        Extract all keys (first elements of pairs).
        """
        dict_bytes = self.translate_expr_with_ref(dict_expr)

        # Create lambda (p) (car p) to extract keys
        car_call = create_inline_call('car', [encode_symbol('p', self.bc)], self.bc)
        lambda_bytes = self._emit_lambda(['p'], car_call)

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
        lambda_bytes = self._emit_lambda(['p'], cdr_call)

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
        assoc_ref = self._hoist(assoc_bytes)

        # (cdr pair) for the true branch
        cdr_bytes = create_inline_call('cdr', [assoc_ref], self.bc)
        cdr_ref = self._hoist(cdr_bytes)

        # (if pair (cdr pair) default)
        return create_inline_call('if', [assoc_ref, cdr_ref, default_bytes], self.bc)

    def translate_dict_subscript_assign(self, target: ast.Subscript,
                                        value_expr: ast.expr) -> bytes:
        """Translate `d['key'] = val`.

        Dicts are association lists in PyVelox, so this rebinds the
        variable to a new list with any existing pair for `key`
        filtered out and the new (key, val) pair consed on:

            (set! d (cons (cons 'key val)
                          (filter (lambda (p) (not (equal (car p) 'key))) d)))
        """
        key_bytes = self.translate_expr_with_ref(target.slice)
        value_bytes = self.translate_expr_with_ref(value_expr)

        # (cons key val) — the new pair, hoisted so the outer cons
        # can reference it as a single token.
        new_pair_ref = self._hoist(create_inline_call(
            'cons', [key_bytes, value_bytes], self.bc))

        # (lambda (p) (not (equal (car p) key))) — drops any existing
        # pair whose key matches.
        car_p_ref = self._hoist(create_inline_call(
            'car', [encode_symbol('p', self.bc)], self.bc))
        eq_ref = self._hoist(create_inline_call(
            'equalp', [car_p_ref, key_bytes], self.bc))
        not_bytes = create_inline_call('not', [eq_ref], self.bc)
        filter_lambda = self._emit_lambda(['p'], not_bytes)

        def build(dict_token: bytes) -> bytes:
            filtered_ref = self._hoist(create_inline_call(
                'filter', [filter_lambda, dict_token], self.bc))
            return create_inline_call(
                'cons', [new_pair_ref, filtered_ref], self.bc)

        return self._mutate_simple_name(
            target.value, "Dict subscript assignment", build)

    def translate_list_append(self, list_expr: ast.expr,
                              args: List[ast.expr]) -> bytes:
        """Translate `lst.append(x)` to `(set! lst (append lst (list x)))`.

        Python's `append` mutates in place; PyVelox lists are
        immutable cons cells so we emit a rebind instead.
        """
        if len(args) != 1:
            raise ValueError("list.append() takes exactly 1 argument")
        value_bytes = self.translate_expr_with_ref(args[0])
        wrapper_ref = self._hoist(create_inline_call(
            'list', [value_bytes], self.bc))
        return self._mutate_simple_name(
            list_expr, "List append",
            lambda token: create_inline_call(
                'append', [token, wrapper_ref], self.bc))

    def translate_list_extend(self, list_expr: ast.expr,
                              args: List[ast.expr]) -> bytes:
        """Translate `lst.extend(other)` to `(set! lst (append lst other))`."""
        if len(args) != 1:
            raise ValueError("list.extend() takes exactly 1 argument")
        other_bytes = self.translate_expr_with_ref(args[0])
        return self._mutate_simple_name(
            list_expr, "List extend",
            lambda token: create_inline_call(
                'append', [token, other_bytes], self.bc))

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

    def translate_list_remove(self, list_expr: ast.expr,
                              args: List[ast.expr]) -> bytes:
        """Translate `lst.remove(x)` to `(set! lst (remove x lst))`."""
        if len(args) != 1:
            raise ValueError("list.remove() takes exactly 1 argument")
        value_bytes = self.translate_expr_with_ref(args[0])
        return self._mutate_simple_name(
            list_expr, "List remove",
            lambda token: create_inline_call(
                'remove', [value_bytes, token], self.bc))

    def translate_list_reverse(self, list_expr: ast.expr) -> bytes:
        """Translate `lst.reverse()` to `(set! lst (reverse lst))`."""
        return self._mutate_simple_name(
            list_expr, "List reverse",
            lambda token: create_inline_call(
                'reverse', [token], self.bc))

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
        lambda_bytes = self._emit_lambda(['e'], equal_call)

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

    def _translate_string_case(self, str_expr: ast.expr,
                               char_op: str) -> bytes:
        """Common implementation for s.upper() / s.lower():
        `(list-to-string (map <char_op> (string-to-list s)))`.
        `char_op` is `char_upcase` or `char_downcase`.
        """
        str_bytes = self.translate_expr_with_ref(str_expr)
        to_list_ref = self._hoist(
            create_inline_call('string_to_list', [str_bytes], self.bc))
        map_ref = self._hoist(create_inline_call(
            'map', [encode_symbol(char_op, self.bc), to_list_ref], self.bc))
        return create_inline_call('list_to_string', [map_ref], self.bc)

    def translate_string_upper(self, str_expr: ast.expr) -> bytes:
        """Translate `s.upper()`."""
        return self._translate_string_case(str_expr, 'char_upcase')

    def translate_string_lower(self, str_expr: ast.expr) -> bytes:
        """Translate `s.lower()`."""
        return self._translate_string_case(str_expr, 'char_downcase')

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
        prefix_len_ref = self._hoist(prefix_len_bytes)

        # (substring s 0 (string-length prefix))
        substring_bytes = create_inline_call('substring', [
            str_bytes,
            encode_integer(0),
            prefix_len_ref
        ], self.bc)
        substring_ref = self._hoist(substring_bytes)

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
        str_len_ref = self._hoist(str_len_bytes)

        # (string-length suffix)
        suffix_len_bytes = create_inline_call('string_length', [suffix_bytes], self.bc)
        suffix_len_ref = self._hoist(suffix_len_bytes)

        # (- (string-length s) (string-length suffix))
        start_pos_bytes = create_inline_call('subtract', [str_len_ref, suffix_len_ref], self.bc)
        start_pos_ref = self._hoist(start_pos_bytes)

        # (substring s start_pos str_len)
        substring_bytes = create_inline_call('substring', [str_bytes, start_pos_ref, str_len_ref], self.bc)
        substring_ref = self._hoist(substring_bytes)

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
