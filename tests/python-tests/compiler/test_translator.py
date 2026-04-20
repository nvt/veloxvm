"""
Unit tests for translator.py

Tests the Python AST to bytecode translation.
"""

import unittest
import ast
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pyvelox.translator import PythonTranslator
from pyvelox.bytecode import Bytecode


class TestLiteralTranslation(unittest.TestCase):
    """Test translation of literal values."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_integer_literal(self):
        """Test translating integer literal."""
        node = ast.parse("42").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(result) > 0)

    def test_boolean_true(self):
        """Test translating True."""
        node = ast.parse("True").body[0].value
        result = self.translator.translate_expr(node)
        self.assertEqual(result, bytes([0x08]))

    def test_boolean_false(self):
        """Test translating False."""
        node = ast.parse("False").body[0].value
        result = self.translator.translate_expr(node)
        self.assertEqual(result, bytes([0x00]))

    def test_string_literal(self):
        """Test translating string literal."""
        node = ast.parse('"hello"').body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)
        self.assertIn('hello', self.bc.symbol_table.strings)

    def test_none_literal(self):
        """Test translating None (maps to False)."""
        node = ast.parse("None").body[0].value
        result = self.translator.translate_expr(node)
        self.assertEqual(result, bytes([0x00]))


class TestVariableTranslation(unittest.TestCase):
    """Test variable assignment and reference."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_variable_assignment(self):
        """Test translating variable assignment."""
        node = ast.parse("x = 10").body[0]
        result = self.translator.translate_stmt(node)

        # Should create inline call to 'define'
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(result) > 0)

    def test_variable_reference(self):
        """Test translating variable reference."""
        node = ast.parse("x").body[0].value
        result = self.translator.translate_expr(node)

        # Should encode symbol 'x'
        self.assertIsInstance(result, bytes)
        self.assertIn('x', self.bc.symbol_table.symbols)

    def test_reassignment(self):
        """Test reassigning a variable uses 'set'."""
        # First assignment
        assign1 = ast.parse("x = 10").body[0]
        self.translator.translate_stmt(assign1)

        # Reassignment
        assign2 = ast.parse("x = 20").body[0]
        result = self.translator.translate_stmt(assign2)
        self.assertIsInstance(result, bytes)


class TestArithmeticTranslation(unittest.TestCase):
    """Test arithmetic operations."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_addition(self):
        """Test translating addition."""
        node = ast.parse("10 + 20").body[0].value
        result = self.translator.translate_expr(node)

        # Should use 'add' primitive
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(result) > 0)

    def test_subtraction(self):
        """Test translating subtraction."""
        node = ast.parse("20 - 10").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_multiplication(self):
        """Test translating multiplication."""
        node = ast.parse("5 * 6").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_division(self):
        """Test translating division."""
        node = ast.parse("10 / 2").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_modulo(self):
        """Test translating modulo."""
        node = ast.parse("10 % 3").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_nested_arithmetic(self):
        """Test translating nested arithmetic."""
        node = ast.parse("(10 + 20) * 3").body[0].value
        result = self.translator.translate_expr(node)

        # Should create multiple expressions for nested operations
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)


class TestComparisonTranslation(unittest.TestCase):
    """Test comparison operations."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_equal(self):
        """Test translating equality."""
        node = ast.parse("x == y").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_not_equal(self):
        """Test translating inequality."""
        node = ast.parse("x != y").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_less_than(self):
        """Test translating less than."""
        node = ast.parse("x < y").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_chained_comparison(self):
        """Test translating chained comparison."""
        node = ast.parse("x < y < z").body[0].value
        result = self.translator.translate_expr(node)

        # Should create multiple expressions
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)


class TestListTranslation(unittest.TestCase):
    """Test list operations."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_empty_list(self):
        """Test translating empty list."""
        node = ast.parse("[]").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_list_literal(self):
        """Test translating list literal."""
        node = ast.parse("[1, 2, 3]").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_list_with_expressions(self):
        """Test translating list with complex expressions."""
        node = ast.parse("[1 + 2, 3 * 4]").body[0].value
        result = self.translator.translate_expr(node)

        # Complex expressions should be stored separately
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)


class TestFunctionTranslation(unittest.TestCase):
    """Test function definition and calls."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_function_definition(self):
        """Test translating function definition."""
        node = ast.parse("def add(a, b):\n    return a + b").body[0]
        result = self.translator.translate_stmt(node)

        # Should create expressions for function
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_function_call(self):
        """Test translating function call."""
        node = ast.parse("add(10, 20)").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_lambda(self):
        """Test translating lambda."""
        node = ast.parse("lambda x: x * 2").body[0].value
        result = self.translator.translate_expr(node)

        # Should create expressions for lambda
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)


class TestControlFlowTranslation(unittest.TestCase):
    """Test control flow statements."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_if_statement(self):
        """Test translating if statement."""
        code = "if x < 10:\n    y = 1\nelse:\n    y = 2"
        node = ast.parse(code).body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)

    def test_if_without_else(self):
        """Test translating if without else."""
        code = "if x > 0:\n    print(x)"
        node = ast.parse(code).body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)

    def test_for_loop(self):
        """Test translating for loop."""
        code = "for i in range(5):\n    print(i)"
        node = ast.parse(code).body[0]
        result = self.translator.translate_stmt(node)

        # Should create expressions for lambda
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_while_loop(self):
        """Test translating while loop."""
        code = "while x > 0:\n    x = x - 1"
        node = ast.parse(code).body[0]
        result = self.translator.translate_stmt(node)

        # Should create expressions for loop function
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)


class TestBuiltinFunctions(unittest.TestCase):
    """Test built-in function translation."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_print(self):
        """Test translating print."""
        node = ast.parse('print("hello")').body[0]
        result = self.translator.translate_stmt(node)

        # Should create expressions for print and newline
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_print_multiple_args(self):
        """Test translating print with multiple arguments."""
        node = ast.parse('print("x =", x)').body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)

    def test_len(self):
        """Test translating len."""
        node = ast.parse("len([1, 2, 3])").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)

    def test_range(self):
        """Test translating range."""
        node = ast.parse("range(5)").body[0].value
        result = self.translator.translate_expr(node)
        self.assertIsInstance(result, bytes)


class TestExceptionTranslation(unittest.TestCase):
    """Test exception handling."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_try_except(self):
        """Test translating try/except."""
        code = "try:\n    x = 1 / 0\nexcept:\n    print('error')"
        node = ast.parse(code).body[0]
        result = self.translator.translate_stmt(node)

        # Should create expressions for guard
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_raise(self):
        """Test translating raise."""
        node = ast.parse("raise ValueError").body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)


class TestReturnStatements(unittest.TestCase):
    """Test translation of return statements."""

    def setUp(self):
        """Create translator for each test."""
        self.bc = Bytecode()
        self.translator = PythonTranslator(self.bc)

    def test_simple_return(self):
        """Test simple return statement."""
        node = ast.parse("return 42").body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)
        # Should contain 'return' primitive call with value 42
        self.assertTrue(len(result) > 0)

    def test_return_none(self):
        """Test return with no value."""
        node = ast.parse("return").body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)
        # Should return False (Python's None)
        self.assertTrue(len(result) > 0)

    def test_return_variable(self):
        """Test return with variable."""
        node = ast.parse("return x").body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)

    def test_return_expression(self):
        """Test return with expression."""
        node = ast.parse("return x + 1").body[0]
        result = self.translator.translate_stmt(node)
        self.assertIsInstance(result, bytes)

    def test_function_with_early_return(self):
        """Test function with early return uses return primitive."""
        code = """
def test(x):
    if x < 0:
        return 999
    return 42
"""
        module = ast.parse(code)
        # Translate the function definition
        func_node = module.body[0]
        result = self.translator.translate_stmt(func_node)

        # Should have created expressions
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)

        # The early return should use the 'return' primitive
        # (we'd need to disassemble to verify the exact bytecode)

    def test_function_final_return_implicit(self):
        """Test that final return uses implicit semantics."""
        code = """
def test(x):
    return x + 1
"""
        module = ast.parse(code)
        func_node = module.body[0]
        result = self.translator.translate_stmt(func_node)

        # Should translate successfully
        self.assertIsInstance(result, bytes)
        # Final return should be implicit (just the value, not return primitive)

    def test_function_multiple_returns(self):
        """Test function with multiple return paths."""
        code = """
def test(x, y):
    if x < 0:
        return -1
    if y < 0:
        return -2
    return x + y
"""
        module = ast.parse(code)
        func_node = module.body[0]
        result = self.translator.translate_stmt(func_node)

        # Should create expressions for all the returns
        self.assertIsInstance(result, bytes)
        self.assertTrue(len(self.bc.expressions) > 0)

    def test_return_in_loop(self):
        """Test return inside a loop."""
        code = """
def test():
    for i in [1, 2, 3]:
        if i == 2:
            return i
    return 0
"""
        module = ast.parse(code)
        func_node = module.body[0]
        result = self.translator.translate_stmt(func_node)

        # Should translate the return inside the loop
        self.assertIsInstance(result, bytes)

    def test_nested_function_returns(self):
        """Test nested functions each with returns."""
        code = """
def outer(x):
    def inner(y):
        return y * 2
    return inner(x) + 1
"""
        module = ast.parse(code)
        func_node = module.body[0]
        result = self.translator.translate_stmt(func_node)

        # Both functions should translate correctly
        self.assertIsInstance(result, bytes)


if __name__ == '__main__':
    unittest.main()
