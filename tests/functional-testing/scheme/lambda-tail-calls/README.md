# Lambda Return Value Corruption Bug Tests

This directory contains tests for the lambda return value corruption bug fixed in commit 143d9847.

## The Bug

Lambda frames were overwriting `argv[0]` with their return values, corrupting the lambda form. When lambdas returned symbols, the VM would attempt to look up those data symbols as procedure names, causing crashes with `VM_ERROR_BYTECODE`. Other return types caused silent corruption.

## Test Files

**test-lambda-returns.scm**
- Tests lambdas returning different types: integers, strings, booleans, quoted numbers, and symbols
- Validates that non-recursive lambda return values don't corrupt the frame
- All tests should pass without crashes

**test-inline-vs-named.scm**
- Tests both inline lambda calls `((lambda () 'symbol))` and named lambda calls
- Validates that both code paths correctly handle lambda return values
- Both should successfully return and display symbols without crashing

**test-mutual-recursion.scm**
- Tests mutual recursion with tail call optimization
- Includes is-even?/is-odd? functions calling each other
- Tests with small numbers for correctness and large numbers (1000) to verify TCO
- Also tests mutual recursion with accumulator parameters

## Running the Tests

From the repository root:

```bash
bin/vm tests/functional-testing/scheme/lambda-tail-calls/test-lambda-returns.vm
bin/vm tests/functional-testing/scheme/lambda-tail-calls/test-inline-vs-named.vm
bin/vm tests/functional-testing/scheme/lambda-tail-calls/test-mutual-recursion.vm
```

All tests should complete successfully without errors.
