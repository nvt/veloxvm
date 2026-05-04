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


class TestLoopControl(unittest.TestCase):
    """break and continue desugar to (raise '__pyvelox_break__) and
    (raise '__pyvelox_continue__) and are caught by guards installed
    around the loop body and the loop itself. break/continue outside
    a loop is a compile error."""

    def test_break_inside_loop_compiles(self):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(
            "for x in [1, 2, 3]:\n"
            "    if x == 2:\n"
            "        break\n"
        )
        all_bytes = b''.join(bc.expressions)
        # The loop body raises the break sentinel and the surrounding
        # guard catches it.
        self.assertIn(encode_symbol('__pyvelox_break__', bc), all_bytes)
        self.assertIn(encode_symbol('guard', bc), all_bytes)

    def test_continue_inside_loop_compiles(self):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(
            "for x in [1, 2, 3]:\n"
            "    if x == 2:\n"
            "        continue\n"
        )
        all_bytes = b''.join(bc.expressions)
        self.assertIn(encode_symbol('__pyvelox_continue__', bc), all_bytes)

    def test_break_outside_loop_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string("x = 1\nbreak\n")
        self.assertEqual(ctx.exception.lineno, 2)
        self.assertIn("break", ctx.exception.raw_message)

    def test_continue_outside_loop_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string("continue\n")
        self.assertEqual(ctx.exception.lineno, 1)
        self.assertIn("continue", ctx.exception.raw_message)

    def test_break_inside_function_inside_loop_is_compile_error(self):
        # The function body is a fresh scope; break/continue don't
        # cross function boundaries.
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError):
            compile_string(
                "for x in [1]:\n"
                "    def f():\n"
                "        break\n"
                "    f()\n"
            )


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
        # `quote` is needed to keep the py-exception tag from being
        # looked up as a variable.
        self.assertIn(encode_symbol('quote', bc), all_bytes)


class TestStructuredRaise(unittest.TestCase):
    """`raise X(args...)` lowers to a tagged 3-vector
    `#(py-exception "TYPE" (list args...))` so handlers can read the
    type and args via `e.type` / `e.args` (lowered to vector_ref)."""

    def _compile_collect(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol, encode_string
        bc = compile_string(source)
        all_bytes = b''.join(bc.expressions)
        return bc, all_bytes, encode_symbol, encode_string

    def test_raise_with_args_emits_vector(self):
        bc, all_bytes, encode_symbol, encode_string = self._compile_collect(
            'raise ValueError("oops")\n')
        # Tagged vector + raise + a string for the type and the
        # message must all show up.
        self.assertIn(encode_symbol('raise', bc), all_bytes)
        self.assertIn(encode_symbol('vector', bc), all_bytes)
        self.assertIn(encode_symbol('py-exception', bc), all_bytes)
        self.assertIn(encode_string('ValueError', bc), all_bytes)
        self.assertIn(encode_string('oops', bc), all_bytes)

    def test_bare_class_name_still_wraps(self):
        # `raise X` with no args lowers to the same tagged vector,
        # just with an empty list of args.
        bc, all_bytes, encode_symbol, encode_string = self._compile_collect(
            'raise KeyError\n')
        self.assertIn(encode_symbol('vector', bc), all_bytes)
        self.assertIn(encode_string('KeyError', bc), all_bytes)

    def test_reraise_of_bound_name_is_passthrough(self):
        # `raise e` inside an except-as-e handler should pass the
        # caught value through unchanged -- no fresh vector built
        # around it. The tell-tale: only ONE `vector` symbol appears
        # in the bytecode (from the inner construction); the re-raise
        # itself doesn't emit another.
        bc, all_bytes, encode_symbol, encode_string = self._compile_collect(
            'try:\n'
            '    raise ValueError("inner")\n'
            'except Exception as e:\n'
            '    raise e\n')
        self.assertIn(encode_symbol('raise', bc), all_bytes)
        # The inner construction emits exactly one vector form.
        self.assertEqual(all_bytes.count(encode_symbol('vector', bc)), 1)

    def test_except_as_binds_attribute_access(self):
        # e.args inside the handler lowers to vector_ref against the
        # bound exception object.
        bc, all_bytes, encode_symbol, encode_string = self._compile_collect(
            'try:\n'
            '    raise ValueError("oops")\n'
            'except Exception as e:\n'
            '    x = e.args\n')
        self.assertIn(encode_symbol('vector_ref', bc), all_bytes)

    def test_attribute_outside_handler_is_compile_error(self):
        # `e.args` on a name that isn't bound by an enclosing except
        # falls through to translate_attribute's refusal -- otherwise
        # any random `obj.foo` would silently emit vector_ref.
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('e = 1\nx = e.args\n')
        self.assertIn("args", ctx.exception.raw_message)

    def test_unsupported_raise_shape_is_compile_error(self):
        # `raise some_func()` doesn't have a Name as the call's func,
        # so translate_raise can't extract a type symbol.
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(
                'def make_exc():\n'
                '    return 1\n'
                'raise (make_exc())()\n')
        # The message mentions raise unsupported shape.
        self.assertIn("raise", ctx.exception.raw_message)

    def test_str_of_variable_emits_helper(self):
        # Variable-typed str() argument now routes through
        # _pyvelox_str so the runtime can branch on the py-exception
        # tag and return args[0] for an exception.
        bc, all_bytes, encode_symbol, encode_string = self._compile_collect(
            'x = 1\ny = str(x)\n')
        self.assertIn(encode_symbol('_pyvelox_str', bc), all_bytes)
        # The helper recognises the py-exception tag.
        self.assertIn(encode_symbol('py-exception', bc), all_bytes)
        # And falls back to _pyvelox_str_value for non-exceptions.
        self.assertIn(encode_symbol('_pyvelox_str_value', bc), all_bytes)

    def test_str_literal_skips_helper(self):
        # Literal arguments still constant-fold; the helper isn't
        # emitted unless something needs it. This keeps trivial
        # programs lean.
        bc, all_bytes, encode_symbol, encode_string = self._compile_collect(
            'y = str(42)\n')
        self.assertNotIn(encode_symbol('_pyvelox_str', bc), all_bytes)


class TestCallDispatchRegistry(unittest.TestCase):
    """`translate_call` dispatches builtins and methods through two
    class-level registries. The tests below pin the registry shape so
    accidental drift (e.g. a renamed handler that the registry still
    points at) shows up loudly, and confirm dispatch still works
    end-to-end through the registry lookup."""

    def test_builtin_handlers_resolve_to_real_methods(self):
        # Every entry in _BUILTIN_HANDLERS must name an existing
        # method on the translator class. A typo here would currently
        # crash at the first call site, but only if the program
        # exercises that builtin.
        for name, attr in PythonTranslator._BUILTIN_HANDLERS.items():
            self.assertTrue(hasattr(PythonTranslator, attr),
                            f"builtin {name!r} -> {attr!r} not found "
                            "on PythonTranslator")

    def test_method_handlers_are_callable(self):
        for name, handler in PythonTranslator._METHOD_HANDLERS.items():
            self.assertTrue(callable(handler),
                            f"method {name!r} handler is not callable")

    def test_builtin_registry_covers_documented_set(self):
        # If you remove or add a builtin, doc/python.md's built-ins
        # table needs to follow. Pin the names here so the change
        # is conscious. (Update both together when the set legitimately
        # changes.)
        expected = {
            'print', 'len', 'range', 'int', 'str', 'bytes', 'bytearray',
            'abs', 'min', 'max', 'sum', 'sorted', 'reversed', 'map',
            'filter', 'reduce', 'all', 'any', 'list', 'enumerate', 'zip',
        }
        self.assertEqual(set(PythonTranslator._BUILTIN_HANDLERS), expected)

    def test_method_registry_covers_documented_set(self):
        expected = {
            # Dict
            'keys', 'values', 'items', 'get',
            # List
            'append', 'extend', 'pop', 'remove', 'reverse',
            'count', 'index', 'insert',
            # String
            'upper', 'lower', 'split', 'join', 'startswith',
            'endswith', 'strip', 'replace',
        }
        self.assertEqual(set(PythonTranslator._METHOD_HANDLERS), expected)

    def test_builtin_dispatch_compiles(self):
        from pyvelox.compiler import compile_string
        # Touch a representative slice of the registry; if the
        # dispatch were broken this would raise PyveloxCompileError or
        # produce empty bytecode.
        bc = compile_string(
            'print(len([1, 2, 3]))\n'
            'x = abs(-5)\n'
            'y = max(1, 2, 3)\n'
        )
        self.assertGreater(len(bc.expressions), 0)

    def test_method_dispatch_compiles(self):
        from pyvelox.compiler import compile_string
        bc = compile_string(
            's = "Hi"\n'
            'u = s.upper()\n'
            'print(u)\n'
            'd = {"a": 1}\n'
            'k = d.keys()\n'
        )
        self.assertGreater(len(bc.expressions), 0)

    def test_user_function_call_uses_generic_path(self):
        # Names not in _BUILTIN_HANDLERS must fall through to the
        # generic call path so user-defined functions still resolve
        # through the symbol table.
        from pyvelox.compiler import compile_string
        bc = compile_string(
            'def square(x):\n'
            '    return x * x\n'
            'r = square(5)\n'
        )
        self.assertGreater(len(bc.expressions), 0)


class TestDefaultArguments(unittest.TestCase):
    """Functions with literal positional defaults work via call-site
    padding. Unsupported signature features (vararg, kwarg, kwonly,
    posonly, lambda defaults, non-literal defaults) raise compile
    errors with a source location instead of silently miscompiling."""

    def _compile(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        return bc, b''.join(bc.expressions), encode_symbol

    def test_default_call_with_arg_provided(self):
        bc, _, _ = self._compile(
            'def f(x, y=10):\n'
            '    return x + y\n'
            'r = f(1, 2)\n'
        )
        # Compiles without error; default is registered.
        from pyvelox.compiler import compile_string  # ensure import
        self.assertIn('f', '|'.join(bc.symbol_table.symbols))

    def test_default_call_with_arg_omitted_compiles(self):
        # Just check this compiles — the runtime padding is exercised
        # by the end-to-end demo programs.
        from pyvelox.compiler import compile_string
        bc = compile_string(
            'def f(x, y=10):\n'
            '    return x + y\n'
            'r = f(1)\n'
        )
        self.assertGreater(len(bc.expressions), 0)

    def test_too_many_args_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(
                'def f(x, y=10): return x + y\n'
                'r = f(1, 2, 3)\n'
            )
        self.assertEqual(ctx.exception.lineno, 2)
        self.assertIn("at most 2", ctx.exception.raw_message)

    def test_missing_required_arg_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(
                'def f(x, y, z=5): return x + y + z\n'
                'r = f(1)\n'
            )
        self.assertEqual(ctx.exception.lineno, 2)
        self.assertIn("missing required", ctx.exception.raw_message)

    def test_varargs_is_now_supported(self):
        from pyvelox.compiler import compile_string
        # `*args` used to be refused; it's now lowered to
        # bind_function_rest. Compile succeeding is the contract.
        compile_string('def f(*args):\n    return 0\n')

    def test_varargs_with_defaults_is_compile_error(self):
        from pyvelox.compiler import compile_string
        # `def f(a, b=1, *args)` is legal in CPython but refused for
        # now -- the call-site default-padding logic assumes a fixed
        # arity. Either feature works on its own.
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('def f(a, b=1, *args):\n    return a + b\n')
        self.assertIn("*args", ctx.exception.raw_message)
        self.assertIn("default", ctx.exception.raw_message)

    def test_kwargs_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('def f(**kw):\n    return 0\n')
        self.assertIn("**kwargs", ctx.exception.raw_message)

    def test_keyword_only_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('def f(x, *, y):\n    return x + y\n')
        self.assertIn("Keyword-only", ctx.exception.raw_message)

    def test_lambda_default_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('g = lambda x, y=10: x + y\n')
        self.assertIn("lambda", ctx.exception.raw_message)

    def test_non_literal_default_is_compile_error(self):
        # Mutable default `[]` is the canonical Python footgun; we
        # refuse non-literal defaults to keep call-site padding sound.
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('def f(x=[1, 2]):\n    return x\n')
        self.assertIn("literal", ctx.exception.raw_message)

    def test_forward_reference_resolves_default(self):
        # A function defined later in the module should have its
        # defaults visible at earlier call sites.
        from pyvelox.compiler import compile_string
        bc = compile_string(
            'def main():\n'
            '    return helper(3)\n'
            'def helper(x, bonus=100):\n'
            '    return x + bonus\n'
        )
        self.assertGreater(len(bc.expressions), 0)


class TestRange(unittest.TestCase):
    """range() supports 1, 2, or 3 arguments. Small literal range(N)
    constant-folds to (list 0 1 ... N-1); everything else routes
    through a `_pyvelox_range` helper emitted once into the program
    prologue."""

    def _compile(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        return bc, b''.join(bc.expressions), encode_symbol

    def test_small_constant_range_constant_folds(self):
        bc, all_bytes, encode_symbol = self._compile('x = range(5)\n')
        # Helper symbol should not appear for the fast path.
        self.assertNotIn(
            encode_symbol('_pyvelox_range', bc), all_bytes)

    def test_variable_range_uses_helper(self):
        bc, all_bytes, encode_symbol = self._compile(
            'n = 4\nx = range(n)\n')
        self.assertIn(encode_symbol('_pyvelox_range', bc), all_bytes)
        self.assertIn(encode_symbol('_pyvelox_range_loop', bc), all_bytes)

    def test_two_arg_range_uses_helper(self):
        bc, all_bytes, encode_symbol = self._compile(
            'x = range(2, 7)\n')
        self.assertIn(encode_symbol('_pyvelox_range', bc), all_bytes)

    def test_three_arg_range_uses_helper(self):
        bc, all_bytes, encode_symbol = self._compile(
            'x = range(0, 10, 2)\n')
        self.assertIn(encode_symbol('_pyvelox_range', bc), all_bytes)

    def test_large_constant_range_uses_helper(self):
        # Above the inline-form argc cap a literal list isn't even
        # representable; the runtime helper takes over.
        bc, all_bytes, encode_symbol = self._compile(
            'x = range(100)\n')
        self.assertIn(encode_symbol('_pyvelox_range', bc), all_bytes)

    def test_helper_emitted_only_once(self):
        from pyvelox.compiler import compile_string
        bc = compile_string(
            'a = range(10, 20)\n'
            'b = range(0, 50)\n'
            'c = range(5, 100, 5)\n'
        )
        # The helper definition lives in the program prologue, prepended
        # to expression 0. Two `define` calls (loop + range), regardless
        # of how many call sites exist.
        prologue = bytes(bc.expressions[0])
        # `define` core symbol id (we don't need to compute it; just
        # check that _pyvelox_range_loop appears exactly once as an
        # app symbol in the program).
        from pyvelox.encoder import encode_symbol
        loop_sym = encode_symbol('_pyvelox_range_loop', bc)
        # Each *use* of the symbol writes the same bytes; we just want
        # to confirm exactly one `define _pyvelox_range_loop` lives in
        # the prologue. Count occurrences of the define-the-loop pair:
        # symbol id followed by a lambda form-ref.
        self.assertEqual(prologue.count(loop_sym), 1)

    def test_no_args_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError):
            compile_string('x = range()\n')

    def test_too_many_args_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError):
            compile_string('x = range(1, 2, 3, 4)\n')


class TestFStrings(unittest.TestCase):
    """f-strings (`ast.JoinedStr` / `ast.FormattedValue`) lower to a
    chain of `string_append` calls, with each interpolated value
    routed through our `str()` conversion. Format specs and conversion
    flags are rejected at compile time."""

    def _compile_collect(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        return bc, b''.join(bc.expressions), encode_symbol

    def test_plain_fstring_is_just_a_string(self):
        # No interpolation — should not emit string_append.
        bc, all_bytes, encode_symbol = self._compile_collect(
            'x = f"hello"\n')
        self.assertNotIn(encode_symbol('string_append', bc), all_bytes)

    def test_fstring_with_interpolation_uses_string_append(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'name = "world"\n'
            'x = f"hello {name}"\n'
        )
        self.assertIn(encode_symbol('string_append', bc), all_bytes)

    def test_fstring_with_literal_int_constant_folds(self):
        # int literal inside the hole goes through str()'s fast path,
        # so the runtime type-dispatch shouldn't appear.
        bc, all_bytes, encode_symbol = self._compile_collect(
            'x = f"answer={42}"\n')
        # No runtime stringp/booleanp/numberp dispatch needed.
        self.assertNotIn(encode_symbol('numberp', bc), all_bytes)
        self.assertNotIn(encode_symbol('booleanp', bc), all_bytes)

    def test_format_spec_rejected_with_location(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('x = 1\nprint(f"{x:.2f}")\n')
        self.assertEqual(ctx.exception.lineno, 2)
        self.assertIn("format spec", ctx.exception.raw_message)

    def test_conversion_rejected_with_location(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('x = "a"\nprint(f"{x!r}")\n')
        self.assertEqual(ctx.exception.lineno, 2)
        self.assertIn("conversion", ctx.exception.raw_message)


class TestEvaluateOnce(unittest.TestCase):
    """int(), str(), and abs() emit a runtime type-dispatch that
    references their argument from multiple branches. For
    side-effecting argument expressions (calls, comprehensions, ...)
    the argument must evaluate exactly once — so the translator
    wraps non-trivial args in a let-binding. Single-token args
    (Name, Constant, Lambda) skip the wrap."""

    def _symbols(self, source):
        bc = compile_string(source)
        return bc.symbol_table.symbols

    def test_abs_of_call_evaluates_once(self):
        syms = self._symbols('def f():\n    return -5\nx = abs(f())\n')
        self.assertTrue(any(s.startswith('_t_') for s in syms),
                        f"expected let-bound temp in {syms!r}")

    def test_abs_of_name_does_not_wrap(self):
        syms = self._symbols('y = -3\nx = abs(y)\n')
        self.assertFalse(any(s.startswith('_t_') for s in syms),
                         f"unexpected let-bound temp in {syms!r}")

    def test_int_of_call_evaluates_once(self):
        syms = self._symbols('def f():\n    return 7\nx = int(f())\n')
        self.assertTrue(any(s.startswith('_t_') for s in syms))

    def test_str_of_call_evaluates_once(self):
        # str() now routes through the _pyvelox_str runtime helper
        # rather than emitting an inline dispatch with a let-bound
        # temp. The argument expression evaluates exactly once because
        # it's passed as the helper's lambda parameter -- no `_t_`
        # symbol involved. Pin the helper presence instead so a
        # regression that re-inlines the dispatch (and loses the
        # evaluate-once guarantee) shows up here.
        from pyvelox.compiler import compile_string
        bc = compile_string('def f():\n    return 7\nx = str(f())\n')
        self.assertIn('_pyvelox_str', bc.symbol_table.symbols)

    def test_chained_comparison_intermediate_evaluates_once(self):
        # `a < g() < b` — g()'s result is the RHS of the first
        # comparison and the LHS of the second. Without the let-bind
        # it would evaluate twice.
        syms = self._symbols(
            'def g():\n    return 5\nx = 1 < g() < 10\n')
        self.assertTrue(any(s.startswith('_t_') for s in syms))

    def test_chained_comparison_only_complex_intermediates_bound(self):
        # Endpoint calls don't appear twice in the chain, so they
        # don't need binding. All-name chains don't either.
        syms = self._symbols(
            'def f():\n    return 1\ndef h():\n    return 10\n'
            'x = f() < 5 < h()\n')
        self.assertFalse(any(s.startswith('_t_') for s in syms))
        syms = self._symbols('a = 1\nb = 2\nc = 3\nx = a < b < c\n')
        self.assertFalse(any(s.startswith('_t_') for s in syms))


class TestIntConversion(unittest.TestCase):
    """`int(x)` previously emitted only string_to_number, which raises
    a type error for ints/bools. Literals now resolve at compile time;
    variables go through a runtime numberp/stringp dispatch."""

    def _compile_collect(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        return bc, b''.join(bc.expressions), encode_symbol

    def test_literal_int_is_inlined(self):
        bc, all_bytes, encode_symbol = self._compile_collect('x = int(5)\n')
        # No runtime dispatch and no string_to_number for a plain int.
        self.assertNotIn(encode_symbol('string_to_number', bc), all_bytes)
        self.assertNotIn(encode_symbol('numberp', bc), all_bytes)
        self.assertNotIn(encode_symbol('stringp', bc), all_bytes)

    def test_literal_bool_becomes_integer(self):
        bc, all_bytes, encode_symbol = self._compile_collect('x = int(True)\n')
        self.assertNotIn(encode_symbol('string_to_number', bc), all_bytes)
        self.assertNotIn(encode_symbol('numberp', bc), all_bytes)

    def test_literal_string_uses_string_to_number(self):
        bc, all_bytes, encode_symbol = self._compile_collect('x = int("42")\n')
        self.assertIn(encode_symbol('string_to_number', bc), all_bytes)
        # Pure compile-time fast path — no runtime dispatch.
        self.assertNotIn(encode_symbol('numberp', bc), all_bytes)

    def test_variable_uses_runtime_dispatch(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'y = 1\nx = int(y)\n')
        self.assertIn(encode_symbol('numberp', bc), all_bytes)
        self.assertIn(encode_symbol('stringp', bc), all_bytes)
        self.assertIn(encode_symbol('string_to_number', bc), all_bytes)


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
        # `break` outside a loop is rejected at translate_stmt; the
        # location should point at the `break` line, not the enclosing
        # def or module.
        source = "def f():\n    break\n"
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


class TestBytesLowering(unittest.TestCase):
    """Bytes literals and `bytes(...)` constructor."""

    def _compile_bytes_source(self, source):
        bc = compile_string(source)
        from pyvelox.encoder import encode_symbol
        all_bytes = b''.join(bc.expressions)
        return bc, all_bytes, encode_symbol

    def test_empty_bytes_literal(self):
        # b'' must not pull in the from-list helper — it's just
        # `(make-buffer 0)`, no preamble work needed.
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            "x = b''\n")
        self.assertIn(encode_symbol('make_buffer', bc), all_bytes)
        self.assertNotIn(
            encode_symbol('_pyvelox_bytes_from_list', bc), all_bytes)

    def test_nonempty_bytes_literal_uses_helper(self):
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            "x = b'AB'\n")
        # The helper symbol must show up — the literal lowers to a
        # call to it.
        self.assertIn(
            encode_symbol('_pyvelox_bytes_from_list', bc), all_bytes)
        self.assertIn(encode_symbol('make_buffer', bc), all_bytes)

    def test_bytes_no_arg(self):
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            'x = bytes()\n')
        # Pure make-buffer 0 — no helper needed.
        self.assertIn(encode_symbol('make_buffer', bc), all_bytes)
        self.assertNotIn(
            encode_symbol('_pyvelox_bytes_from_list', bc), all_bytes)

    def test_bytes_int_literal(self):
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            'x = bytes(8)\n')
        self.assertIn(encode_symbol('make_buffer', bc), all_bytes)
        self.assertNotIn(
            encode_symbol('_pyvelox_bytes_from_list', bc), all_bytes)

    def test_bytes_list_literal_uses_helper(self):
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            'x = bytes([1, 2, 3])\n')
        self.assertIn(
            encode_symbol('_pyvelox_bytes_from_list', bc), all_bytes)

    def test_bytes_variable_uses_runtime_dispatch(self):
        # Variable arg should fold through numberp / bufferp dispatch.
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            'y = 4\nx = bytes(y)\n')
        self.assertIn(encode_symbol('numberp', bc), all_bytes)
        self.assertIn(encode_symbol('bufferp', bc), all_bytes)
        self.assertIn(encode_symbol('make_buffer', bc), all_bytes)
        self.assertIn(
            encode_symbol('_pyvelox_bytes_from_list', bc), all_bytes)

    def test_bytearray_is_refused(self):
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('x = bytearray()\n')
        self.assertIn("bytearray", ctx.exception.raw_message)

    def test_bytes_bool_is_refused(self):
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('x = bytes(True)\n')
        self.assertIn("bool", ctx.exception.raw_message)

    def test_len_emits_vector_dispatch(self):
        # The new len() lowers via vectorp / vector_length so it works
        # on bytes. Make sure we still mention `length` for the
        # list/string fallthrough.
        bc, all_bytes, encode_symbol = self._compile_bytes_source(
            'x = [1]\nprint(len(x))\n')
        self.assertIn(encode_symbol('vectorp', bc), all_bytes)
        self.assertIn(encode_symbol('vector_length', bc), all_bytes)
        self.assertIn(encode_symbol('length', bc), all_bytes)


class TestVarargs(unittest.TestCase):
    """`def f(*args)` lowering."""

    def _compile_collect(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        all_bytes = b''.join(bc.expressions)
        return bc, all_bytes, encode_symbol

    def test_vararg_function_emits_bind_function_rest(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'def f(*args):\n    return 0\n')
        self.assertIn(encode_symbol('bind_function_rest', bc), all_bytes)

    def test_non_vararg_function_does_not_emit_rest(self):
        # Sanity: a plain `def` should still use bind_function. If the
        # rest variant ever leaks into fixed-arity functions we'd
        # silently change the runtime arity check.
        bc, all_bytes, encode_symbol = self._compile_collect(
            'def f(a, b):\n    return a + b\n')
        self.assertIn(encode_symbol('bind_function', bc), all_bytes)
        self.assertNotIn(encode_symbol('bind_function_rest', bc), all_bytes)

    def test_vararg_lambda_emits_bind_function_rest(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'f = lambda *args: args\n')
        self.assertIn(encode_symbol('bind_function_rest', bc), all_bytes)

    def test_vararg_with_leading_positional(self):
        # `def f(label, *rest)` should still compile -- the rest
        # parameter sits after the fixed formals.
        bc, all_bytes, encode_symbol = self._compile_collect(
            'def f(label, *rest):\n    return label\n')
        self.assertIn(encode_symbol('bind_function_rest', bc), all_bytes)
        self.assertIn(encode_symbol('label', bc), all_bytes)
        self.assertIn(encode_symbol('rest', bc), all_bytes)


class TestStarredCallSite(unittest.TestCase):
    """Forwarding `f(*args)` at a call site."""

    def _compile_collect(self, source):
        from pyvelox.compiler import compile_string
        from pyvelox.encoder import encode_symbol
        bc = compile_string(source)
        all_bytes = b''.join(bc.expressions)
        return bc, all_bytes, encode_symbol

    def test_pure_starred_emits_apply(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'def f(*a):\n    return a\n'
            'def g(*a):\n    return f(*a)\n')
        # The wrapper should reach `apply`; `cons` shouldn't be needed
        # because there's no fixed prefix.
        self.assertIn(encode_symbol('apply', bc), all_bytes)

    def test_prefix_plus_starred_uses_cons(self):
        bc, all_bytes, encode_symbol = self._compile_collect(
            'def f(a, b, c):\n    return a + b + c\n'
            'xs = [2, 3]\n'
            'r = f(1, *xs)\n')
        self.assertIn(encode_symbol('apply', bc), all_bytes)
        # Fixed prefix is cons-prepended onto the starred list.
        self.assertIn(encode_symbol('cons', bc), all_bytes)

    def test_no_starred_does_not_emit_apply(self):
        # Sanity: a call without *args should keep the direct-emit
        # path -- if apply leaked in, every call would pay the
        # list-construction overhead.
        bc, all_bytes, encode_symbol = self._compile_collect(
            'def f(a, b):\n    return a + b\n'
            'r = f(1, 2)\n')
        self.assertNotIn(encode_symbol('apply', bc), all_bytes)

    def test_multiple_starred_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(
                'def f(*a):\n    return a\n'
                'r = f(*[1], *[2])\n')
        self.assertIn("Multiple", ctx.exception.raw_message)

    def test_starred_not_last_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string(
                'def f(*a):\n    return a\n'
                'r = f(*[1], 2)\n')
        self.assertIn("after", ctx.exception.raw_message.lower())

    def test_starred_into_builtin_is_compile_error(self):
        # print(*args) etc. would need each builtin handler to
        # cooperate; refuse for now so the error is friendly rather
        # than coming out of translate_expr deep below.
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('xs = [1, 2]\nprint(*xs)\n')
        self.assertIn("print", ctx.exception.raw_message)
        self.assertIn("Built-in", ctx.exception.raw_message)

    def test_starred_into_method_is_compile_error(self):
        from pyvelox.compiler import compile_string
        with self.assertRaises(PyveloxCompileError) as ctx:
            compile_string('xs = []\nys = [1]\nxs.extend(*ys)\n')
        self.assertIn("Method", ctx.exception.raw_message)


if __name__ == '__main__':
    unittest.main()
