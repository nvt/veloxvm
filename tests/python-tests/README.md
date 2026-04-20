# PyVelox Test Suite

Unit tests for the PyVelox Python-to-VeloxVM compiler.

## Overview

These tests verify that Python source code compiles correctly to VeloxVM bytecode and executes as expected. Unlike the Scheme unit tests which use a test framework, these are Python programs that compile to bytecode and run directly on the VM.

## Test Files

**test_basic_features.py** - Core language features test
Tests all major Python features supported by PyVelox:
- Arithmetic operations (+, -, *, //, %)
- Numeric comparisons (==, !=, <, <=, >, >=)
- Lists (indexing, slicing, concatenation)
- Dictionaries (lookup via association lists)
- Variables and augmented assignment (+=, -=, *=)
- Tuple unpacking
- Functions (def, lambda, return)
- Control flow (if/elif/else, ternary expressions)
- Loops (for, while)
- Built-in functions (len, int, str, abs, min, max, sum)
- Higher-order functions (map, filter)
- Bitwise operators (&, |, ^, <<, >>)
- Enumerate and zip
- Complex expressions

**test_pyvelox_suite.py** - Comprehensive test (has some issues)
Attempts to test all features with PASS/FAIL assertions. Currently has issues with:
- Boolean/string comparisons with `==` (compiler limitation)
- List mutation (not fully supported)

## Running Tests

### Run All Tests (Recommended)

```bash
# Run all Python tests with the test runner
./tests/python-tests/run-tests.sh
```

This will compile and run all `test_*.py` files, showing a summary of results.

### Run Individual Tests

```bash
# Compile a test
./languages/python/pyvelox-compile tests/python-tests/test_basic_features.py

# Run the compiled test
bin/vm tests/python-tests/test_basic_features.vm
```

## Known Limitations

Based on testing, the following features have issues in the current PyVelox implementation:

1. **Negative indexing**: `lst[-1]` not supported
2. **Negative slice indices**: `lst[-2:]` not supported
3. **Boolean comparisons**: `True == True` causes type error (use boolean values directly)
4. **String comparisons**: `"a" == "a"` causes type error (compiler uses `=` instead of `equal?`)
5. **List mutation**: Updating list elements `lst[0] = x` may have issues
6. **sorted/reversed**: These functions may cause infinite loops (needs investigation)

## Expected Output

The basic features test should produce output like:

```
==========================================================
PyVelox Basic Features Test
==========================================================

===Arithmetic Operations===
  Integer assignment: x =42
  Addition 10 + 5 =15
  ...
  OK: All arithmetic operations work

===Numeric Comparisons===
  5 == 5:#t
  ...
  OK: Numeric comparisons work

...

==========================================================
PyVelox Basic Features Test Complete
==========================================================
All tested features compiled and executed successfully!
==========================================================
```

## Adding New Tests

When creating new Python tests:

1. **Focus on demonstrative output**: Print results showing feature works
2. **Avoid problematic features**: Skip features listed in Known Limitations
3. **Document issues**: Add comments when skipping tests due to compiler bugs
4. **Keep tests simple**: One feature per section
5. **Use descriptive output**: Make it easy to see what's being tested

## See Also

- `doc/python.md` - PyVelox compiler documentation
- `languages/python/pyvelox/` - Compiler implementation
- `tests/unit-tests/` - Scheme unit tests (use test framework)
