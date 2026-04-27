"""
Edge case tests for the Python to VeloxVM compiler.

Tests unusual inputs, boundary conditions, and error handling.
"""

import unittest
import ast
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pyvelox.translator import PythonTranslator
from pyvelox.bytecode import Bytecode
from pyvelox.compiler import compile_string
from pyvelox.encoder import encode_integer
from pyvelox.errors import PyveloxCompileError


class TestBoundaryValues(unittest.TestCase):
    """Test boundary values for various types."""

    def test_zero(self):
        """Test encoding zero."""
        result = encode_integer(0)
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(result) > 0)

    def test_negative_zero(self):
        """Test encoding -0 (should be same as 0)."""
        result = encode_integer(-0)
        self.assertEqual(result, encode_integer(0))

    def test_large_integer(self):
        """Test encoding large integer."""
        result = encode_integer(1000000)
        self.assertIsInstance(result, bytes)

    def test_max_single_byte(self):
        """Test encoding max single-byte integer."""
        result = encode_integer(255)
        self.assertIsInstance(result, bytes)

    def test_empty_string(self):
        """Test compiling program with empty string."""
        source = 'print("")'
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)


class TestEmptyConstructs(unittest.TestCase):
    """Test empty or minimal constructs."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_empty_list(self):
        """Test empty list literal."""
        node = ast.parse("[]").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_empty_dict(self):
        """Test empty dict literal."""
        node = ast.parse("{}").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_function_no_params(self):
        """Test function with no parameters."""
        source = """
def hello():
    print("hi")
"""
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_function_no_body(self):
        """Test function with only pass."""
        source = """
def noop():
    pass
"""
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)


class TestNestedStructures(unittest.TestCase):
    """Test deeply nested structures."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_nested_lists(self):
        """Test nested list literals."""
        node = ast.parse("[[1, 2], [3, 4]]").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)
        # Should create multiple expressions for nested lists
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_deeply_nested_arithmetic(self):
        """Test deeply nested arithmetic."""
        node = ast.parse("((1 + 2) * (3 + 4)) + ((5 - 6) * (7 - 8))").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)
        # Should create many expressions
        self.assertTrue(len(self.bc.expressions) >= 4)

    def test_nested_function_calls(self):
        """Test nested function calls."""
        source = "print(len([1, 2, 3]))"
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)


class TestSpecialCharacters(unittest.TestCase):
    """Test strings with special characters."""

    def test_string_with_newline(self):
        """Test string containing newline."""
        source = 'print("hello\\nworld")'
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_string_with_quotes(self):
        """Test string containing quotes."""
        source = 'print("say \\"hi\\"")'
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_string_with_unicode(self):
        """Test string with unicode characters."""
        unicode_str = "Hello \u4e16\u754c"
        source = f'print("{unicode_str}")'
        bc = compile_string(source)
        self.assertIn(unicode_str, bc.symbol_table.strings)


class TestAugmentedAssignment(unittest.TestCase):
    """Test augmented assignment operators."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_plus_equals(self):
        """Test += operator."""
        source = """
x = 10
x += 5
"""
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_minus_equals(self):
        """Test -= operator."""
        code = "x = 10\nx -= 3"
        module = ast.parse(code)
        for stmt in module.body:
            self.translator.translate_stmt(stmt)
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_times_equals(self):
        """Test *= operator."""
        code = "x = 5\nx *= 2"
        module = ast.parse(code)
        for stmt in module.body:
            self.translator.translate_stmt(stmt)
        self.assertTrue(len(self.bc.expressions) > 0)


class TestComparisonChains(unittest.TestCase):
    """Test chained comparison operations."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_simple_chain(self):
        """Test simple chained comparison."""
        node = ast.parse("x < y < z").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)
        # Should create expressions for comparisons
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_long_chain(self):
        """Test long chained comparison."""
        node = ast.parse("a < b <= c < d <= e").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)


class TestBooleanOperations(unittest.TestCase):
    """Test boolean operation edge cases."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_multiple_and(self):
        """Test multiple AND operations."""
        node = ast.parse("a and b and c and d").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_multiple_or(self):
        """Test multiple OR operations."""
        node = ast.parse("a or b or c").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_mixed_boolean(self):
        """Test mixed boolean operations."""
        node = ast.parse("(a and b) or (c and d)").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)


class TestPrintVariations(unittest.TestCase):
    """Test various print statement forms."""

    def test_print_no_args(self):
        """Test print with no arguments."""
        source = "print()"
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_print_one_arg(self):
        """Test print with one argument."""
        source = 'print("hello")'
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_print_many_args(self):
        """Test print with many arguments."""
        source = 'print(1, 2, 3, 4, 5)'
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)

    def test_print_mixed_types(self):
        """Test print with mixed argument types."""
        source = 'print("x =", 10, "y =", 20)'
        bc = compile_string(source)
        self.assertTrue(len(bc.expressions) > 0)


class TestListOperations(unittest.TestCase):
    """Test list operation edge cases."""

    def test_single_element_list(self):
        """Test list with single element."""
        source = "[42]"
        bc = compile_string(source)
        module = ast.parse(source)
        self.assertTrue(len(module.body) == 1)

    def test_list_with_expressions(self):
        """Test list containing expressions."""
        source = "[1 + 1, 2 * 2, 3 - 1]"
        bc = compile_string(source)
        # Should create expressions for operations
        self.assertTrue(len(bc.expressions) > 0)


class TestEqualityCompilation(unittest.TestCase):
    """`==` and `!=` must route through the type-aware `equalp`
    primitive, not the numeric-only `equal`. The numeric path silently
    returns false for strings, booleans, and lists."""

    def _compile_and_collect_bytes(self, source):
        """Compile a Python program and return (concatenated bytecode,
        encoded equalp symbol, encoded equal symbol). Searching the
        concatenation lets us detect the chosen primitive regardless of
        which sub-expression contains it."""
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        all_bytes = b''.join(bc.expressions)
        return (all_bytes,
                encode_symbol('equalp', bc),
                encode_symbol('equal', bc))

    def test_string_equality_uses_equalp(self):
        all_bytes, equalp_sym, equal_sym = self._compile_and_collect_bytes(
            'x = "a" == "b"\n')
        self.assertIn(equalp_sym, all_bytes)
        self.assertNotIn(equal_sym, all_bytes)

    def test_inequality_uses_not_and_equalp(self):
        all_bytes, equalp_sym, equal_sym = self._compile_and_collect_bytes(
            'x = "a" != "b"\n')
        self.assertIn(equalp_sym, all_bytes)
        self.assertNotIn(equal_sym, all_bytes)

    def test_chained_equality_uses_equalp(self):
        all_bytes, equalp_sym, equal_sym = self._compile_and_collect_bytes(
            'x = 1 == 1 == 2\n')
        self.assertIn(equalp_sym, all_bytes)
        self.assertNotIn(equal_sym, all_bytes)

    def test_other_relops_unchanged(self):
        # `<` should still emit less_than, not equalp.
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string('x = 1 < 2\n')
        all_bytes = b''.join(bc.expressions)
        self.assertIn(encode_symbol('less_than', bc), all_bytes)
        self.assertNotIn(encode_symbol('equalp', bc), all_bytes)


class TestTryExceptCompilation(unittest.TestCase):
    """The VM's `guard` form takes exactly three arguments
    (exc-symbol, handler, body). The translator previously emitted
    four; the result was a runtime "Argument count" error on every
    try/except. `raise X` also has to quote its symbol argument or
    the VM tries to look X up as a variable."""

    def test_try_except_uses_three_arg_guard(self):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(
            "caught = False\n"
            "try:\n"
            "    raise ValueError\n"
            "except:\n"
            "    caught = True\n"
        )
        all_bytes = b''.join(bc.expressions)
        self.assertIn(encode_symbol('guard', bc), all_bytes)
        # `quote` is needed to keep the exception type from being
        # looked up as a variable.
        self.assertIn(encode_symbol('quote', bc), all_bytes)


class TestStrConversion(unittest.TestCase):
    """`str(x)` must work for non-numeric types: literals route to the
    correct fast path at compile time; variables and other expressions
    fall through to a runtime type-dispatch."""

    def _compile_collect(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        return bc, b''.join(bc.expressions), encode_symbol

    def test_literal_string_is_identity(self):
        # str("hi") should NOT emit number_to_string, stringp, or booleanp.
        bc, all_bytes, encode_symbol = self._compile_collect('x = str("hi")\n')
        self.assertNotIn(encode_symbol('number_to_string', bc), all_bytes)
        self.assertNotIn(encode_symbol('booleanp', bc), all_bytes)
        self.assertNotIn(encode_symbol('stringp', bc), all_bytes)

    def test_literal_int_uses_number_to_string(self):
        bc, all_bytes, encode_symbol = self._compile_collect('x = str(5)\n')
        self.assertIn(encode_symbol('number_to_string', bc), all_bytes)
        # No runtime dispatch needed for a literal int.
        self.assertNotIn(encode_symbol('booleanp', bc), all_bytes)

    def test_literal_bool_is_inlined(self):
        bc, all_bytes, encode_symbol = self._compile_collect('x = str(True)\n')
        # Pure compile-time: no number_to_string, no type predicates.
        self.assertNotIn(encode_symbol('number_to_string', bc), all_bytes)
        self.assertNotIn(encode_symbol('booleanp', bc), all_bytes)

    def test_variable_uses_runtime_dispatch(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'y = 1\nx = str(y)\n')
        # Runtime dispatch should emit stringp, booleanp, and
        # number_to_string for the fallback.
        self.assertIn(encode_symbol('stringp', bc), all_bytes)
        self.assertIn(encode_symbol('booleanp', bc), all_bytes)
        self.assertIn(encode_symbol('number_to_string', bc), all_bytes)


class TestCompileErrorLocation(unittest.TestCase):
    """Errors raised below translate_stmt/translate_expr should carry the
    offending node's source location, and the formatter should render it
    as `[path:]line:col: message` plus a snippet of the line."""

    def test_unsupported_expression_carries_location(self):
        source = "x = 1\ny = {n for n in [1, 2]}\n"
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(source)
        err = ctx.exception
        self.assertEqual(err.lineno, 2)
        # `{n for n in [1, 2]}` starts at column 4 (0-indexed) on line 2.
        self.assertEqual(err.col_offset, 4)
        self.assertIn("SetComp", err.raw_message)

    def test_unsupported_statement_carries_location(self):
        # `break` is rejected at translate_stmt — the location should
        # point at the `break` line, not the enclosing for loop.
        source = "for x in [1]:\n    break\n"
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(source)
        err = ctx.exception
        self.assertEqual(err.lineno, 2)
        self.assertIn("break", err.raw_message)

    def test_format_includes_path_line_col_and_snippet(self):
        source = "y = {n for n in [1, 2]}\n"
        try:
            compile_string(source)
        except PyveloxCompileError as e:
            rendered = e.format(source_path="prog.py")
        self.assertIn("prog.py:1:5:", rendered)
        self.assertIn("y = {n for n in [1, 2]}", rendered)
        self.assertIn("^", rendered)


if __name__ == '__main__':
    unittest.main()
