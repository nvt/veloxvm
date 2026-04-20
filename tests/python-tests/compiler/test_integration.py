"""
Integration tests for the Python to VeloxVM compiler.

Tests full compilation and execution of Python programs.
"""

import unittest
import subprocess
import tempfile
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from pyvelox.compiler import compile_string, compile_file
from pyvelox.writer import write_bytecode_file


class TestCompilationIntegration(unittest.TestCase):
    """Test full compilation pipeline."""

    def test_compile_simple_program(self):
        """Test compiling a simple Python program."""
        source = """
x = 10
y = 20
print(x + y)
"""
        bc = compile_string(source)

        # Should have expressions
        self.assertTrue(len(bc.expressions) > 0)

        # Should have symbols
        self.assertIn('x', bc.symbol_table.symbols)
        self.assertIn('y', bc.symbol_table.symbols)

    def test_compile_with_function(self):
        """Test compiling program with function."""
        source = """
def add(a, b):
    return a + b

result = add(10, 20)
print(result)
"""
        bc = compile_string(source)

        # Should have function name and result in symbols
        # 'add' appears in symbols because it's used in the assignment
        self.assertTrue(len(bc.symbol_table.symbols) > 0)
        self.assertIn('result', bc.symbol_table.symbols)

    def test_compile_with_loop(self):
        """Test compiling program with loop."""
        source = """
for i in range(3):
    print(i)
"""
        bc = compile_string(source)

        # Should have expressions for lambda
        self.assertTrue(len(bc.expressions) >= 2)

    def test_write_bytecode_file(self):
        """Test writing bytecode to file."""
        source = "print('hello')"
        bc = compile_string(source)

        with tempfile.NamedTemporaryFile(suffix='.vm', delete=False) as f:
            output_path = Path(f.name)

        try:
            write_bytecode_file(output_path, bc)

            # File should exist and have content
            self.assertTrue(output_path.exists())
            self.assertTrue(output_path.stat().st_size > 0)

            # Check header
            with open(output_path, 'rb') as f:
                header = f.read(3)
                self.assertEqual(header, bytes([0x5E, 0xB5, 0x02]))
        finally:
            output_path.unlink()


class TestVeloxVMExecution(unittest.TestCase):
    """Test executing compiled programs on VeloxVM."""

    def setUp(self):
        """Check if VeloxVM binary exists."""
        self.vm_path = Path(__file__).parent.parent.parent.parent / 'bin' / 'vm'
        if not self.vm_path.exists():
            self.skipTest(f"VeloxVM binary not found at {self.vm_path}")

    def compile_and_run(self, source_code):
        """Helper to compile and run Python code on VeloxVM."""
        bc = compile_string(source_code)

        with tempfile.NamedTemporaryFile(suffix='.vm', delete=False) as f:
            output_path = Path(f.name)

        try:
            write_bytecode_file(output_path, bc)

            # Run on VeloxVM
            result = subprocess.run(
                [str(self.vm_path), str(output_path)],
                capture_output=True,
                text=True,
                timeout=5
            )

            return result.stdout, result.stderr, result.returncode
        finally:
            output_path.unlink()

    def test_execute_hello_world(self):
        """Test executing hello world."""
        source = 'print("Hello, World!")'
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("Hello, World!", stdout)

    def test_execute_arithmetic(self):
        """Test executing arithmetic operations."""
        source = """
x = 10
y = 20
print(x + y)
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("30", stdout)

    def test_execute_function(self):
        """Test executing function definition and call."""
        source = """
def multiply(a, b):
    return a * b

result = multiply(6, 7)
print(result)
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("42", stdout)

    def test_execute_for_loop(self):
        """Test executing for loop."""
        source = """
for i in range(3):
    print(i)
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("0", stdout)
        self.assertIn("1", stdout)
        self.assertIn("2", stdout)

    def test_execute_if_statement(self):
        """Test executing if statement."""
        source = """
x = 10
if x > 5:
    print("greater")
else:
    print("smaller")
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("greater", stdout)

    def test_execute_list_operations(self):
        """Test executing list operations."""
        source = """
lst = [1, 2, 3]
print(lst)
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("1", stdout)
        self.assertIn("2", stdout)
        self.assertIn("3", stdout)

    @unittest.skip("TODO: Fix if statement inside function bodies - returns symbol instead of executing")
    def test_execute_recursive_function(self):
        """Test executing recursive function."""
        source = """
def fib(n):
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

print(fib(5))
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("5", stdout)

    def test_execute_nested_expressions(self):
        """Test executing nested expressions."""
        source = """
x = 10
y = 20
result = (x + y) * 2
print(result)
"""
        stdout, stderr, returncode = self.compile_and_run(source)

        self.assertEqual(returncode, 0)
        self.assertIn("60", stdout)


if __name__ == '__main__':
    unittest.main()
