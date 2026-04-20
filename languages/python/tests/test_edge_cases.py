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
        source = 'print("Hello 世界")'
        bc = compile_string(source)
        self.assertIn("Hello 世界", bc.symbol_table.strings)


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


if __name__ == '__main__':
    unittest.main()
