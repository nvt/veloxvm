"""
Unit tests for encoder.py

Tests the low-level bytecode encoding functions.
"""

import unittest
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pyvelox.encoder import (
    encode_integer, encode_boolean, encode_string, encode_character,
    encode_symbol, encode_inline_form, encode_form_ref, encode_form_lambda,
    create_inline_call, create_bind_form
)
from pyvelox.bytecode import Bytecode


class TestIntegerEncoding(unittest.TestCase):
    """Test integer encoding."""

    def test_zero(self):
        """Test encoding zero."""
        result = encode_integer(0)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], 0x09)  # Header
        self.assertEqual(result[1], 0x00)  # Value

    def test_positive_single_byte(self):
        """Test encoding small positive integers."""
        result = encode_integer(10)
        self.assertEqual(len(result), 2)
        self.assertEqual(result, bytes([0x09, 0x0a]))

    def test_positive_two_bytes(self):
        """Test encoding larger positive integers."""
        result = encode_integer(256)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[0], 0x11)  # Header with size=2
        self.assertEqual(result[1:], bytes([0x01, 0x00]))

    def test_negative_single_byte(self):
        """Test encoding negative integers."""
        result = encode_integer(-10)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], 0x49)  # Header with sign bit

    def test_large_positive(self):
        """Test encoding large positive integers."""
        result = encode_integer(65536)
        self.assertEqual(len(result), 4)
        self.assertEqual(result[0], 0x19)  # Header with size=3

    def test_large_negative(self):
        """Test encoding large negative integers."""
        result = encode_integer(-65536)
        self.assertEqual(len(result), 4)
        self.assertEqual(result[0], 0x59)  # Header with sign bit + size=3


class TestBooleanEncoding(unittest.TestCase):
    """Test boolean encoding."""

    def test_true(self):
        """Test encoding True."""
        result = encode_boolean(True)
        self.assertEqual(len(result), 1)
        self.assertEqual(result, bytes([0x08]))  # bit 3 set for True

    def test_false(self):
        """Test encoding False."""
        result = encode_boolean(False)
        self.assertEqual(len(result), 1)
        self.assertEqual(result, bytes([0x00]))  # bit 3 clear for False


class TestCharacterEncoding(unittest.TestCase):
    """Test character encoding."""

    def test_ascii_char(self):
        """Test encoding ASCII character."""
        result = encode_character('A')
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], 0x06)  # CHARACTER type
        self.assertEqual(result[1], ord('A'))

    def test_newline(self):
        """Test encoding newline."""
        result = encode_character('\n')
        self.assertEqual(result, bytes([0x06, 0x0a]))

    def test_invalid_multi_char(self):
        """Test that multi-character strings raise error."""
        with self.assertRaises(ValueError):
            encode_character('AB')

    def test_invalid_empty(self):
        """Test that empty strings raise error."""
        with self.assertRaises(ValueError):
            encode_character('')


class TestStringEncoding(unittest.TestCase):
    """Test string encoding."""

    def test_simple_string(self):
        """Test encoding a simple string."""
        bc = Bytecode()
        result = encode_string('hello', bc)

        # Should be 2 bytes: header + index
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], 0x04)  # STRING type

        # String should be added to string table
        self.assertIn('hello', bc.symbol_table.strings)

    def test_string_reuse(self):
        """Test that identical strings reuse the same ID."""
        bc = Bytecode()
        result1 = encode_string('test', bc)
        result2 = encode_string('test', bc)

        # Should produce identical encoding
        self.assertEqual(result1, result2)

        # String should appear only once in table
        self.assertEqual(bc.symbol_table.strings.count('test'), 1)

    def test_multiple_strings(self):
        """Test encoding multiple different strings."""
        bc = Bytecode()
        encode_string('first', bc)
        encode_string('second', bc)
        encode_string('third', bc)

        self.assertEqual(len(bc.symbol_table.strings), 3)
        self.assertIn('first', bc.symbol_table.strings)
        self.assertIn('second', bc.symbol_table.strings)
        self.assertIn('third', bc.symbol_table.strings)


class TestSymbolEncoding(unittest.TestCase):
    """Test symbol encoding."""

    def test_primitive_symbol(self):
        """Test encoding a primitive symbol."""
        bc = Bytecode()
        result = encode_symbol('add', bc)

        # 'add' is primitive 0, scope=0
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], 0x05)  # SYMBOL type
        self.assertEqual(result[1], 0x00)  # Scope 0, ID 0

    def test_app_symbol(self):
        """Test encoding an application symbol."""
        bc = Bytecode()
        result = encode_symbol('my_var', bc)

        # Should be app symbol (scope=1)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], 0x05)  # SYMBOL type
        self.assertEqual(result[1] & 0x80, 0x80)  # Scope bit set

        # Symbol should be in symbol table
        self.assertIn('my_var', bc.symbol_table.symbols)

    def test_symbol_reuse(self):
        """Test that symbols are reused."""
        bc = Bytecode()
        result1 = encode_symbol('var', bc)
        result2 = encode_symbol('var', bc)

        self.assertEqual(result1, result2)
        self.assertEqual(bc.symbol_table.symbols.count('var'), 1)


class TestFormEncoding(unittest.TestCase):
    """Test form encoding."""

    def test_inline_form_small(self):
        """Test encoding inline form with small argc."""
        result = encode_inline_form(3)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0], 0x83)  # 0x80 | 3

    def test_inline_form_max(self):
        """Test encoding inline form with max argc (63)."""
        result = encode_inline_form(63)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0], 0xBF)  # 0x80 | 63

    def test_inline_form_invalid(self):
        """Test that invalid argc raises error."""
        with self.assertRaises(ValueError):
            encode_inline_form(64)

    def test_form_ref_simple(self):
        """Test encoding simple form reference (ID < 16)."""
        result = encode_form_ref(5)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0] & 0xF0, 0xD0)  # FORM_REF with simple bit

    def test_form_ref_extended(self):
        """Test encoding extended form reference (ID >= 16)."""
        result = encode_form_ref(100)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0] & 0xE0, 0xC0)  # FORM_REF without simple bit

    def test_form_lambda_simple(self):
        """Test encoding simple lambda form."""
        result = encode_form_lambda(3)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0] & 0xF0, 0xB0)  # FORM_LAMBDA with simple bit

    def test_form_lambda_extended(self):
        """Test encoding extended lambda form."""
        result = encode_form_lambda(20)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0] & 0xE0, 0xA0)  # FORM_LAMBDA without simple bit


class TestInlineCall(unittest.TestCase):
    """Test inline call creation."""

    def test_simple_call(self):
        """Test creating a simple inline call."""
        bc = Bytecode()
        result = create_inline_call('add', [encode_integer(1), encode_integer(2)], bc)

        # Should have: inline(3) + add-symbol + int(1) + int(2)
        self.assertTrue(len(result) > 0)
        self.assertEqual(result[0], 0x83)  # inline(3)

    def test_call_with_no_args(self):
        """Test creating call with no arguments (just operator)."""
        bc = Bytecode()
        result = create_inline_call('exit', [], bc)

        # Should have: inline(1) + exit-symbol
        self.assertEqual(result[0], 0x81)  # inline(1)

    def test_call_with_many_args(self):
        """Test creating call with multiple arguments."""
        bc = Bytecode()
        args = [encode_integer(i) for i in range(10)]
        result = create_inline_call('list', args, bc)

        # Should have: inline(11) + list-symbol + 10 integers
        self.assertEqual(result[0], 0x8B)  # inline(11) = 0x80 | 11


class TestBindForm(unittest.TestCase):
    """Test bind form creation."""

    def test_bind_no_params(self):
        """Test creating bind with no parameters."""
        bc = Bytecode()
        body = encode_integer(42)
        result = create_bind_form([], body, bc)

        # Should have: inline(2) + bind-symbol + body
        self.assertEqual(result[0], 0x82)  # inline(2)

    def test_bind_with_params(self):
        """Test creating bind with parameters."""
        bc = Bytecode()
        body = encode_integer(0)
        result = create_bind_form(['a', 'b'], body, bc)

        # Should have: inline(4) + bind-symbol + a-symbol + b-symbol + body
        self.assertEqual(result[0], 0x84)  # inline(4)


if __name__ == '__main__':
    unittest.main()
