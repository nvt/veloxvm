#!/usr/bin/env python3
"""
PyVelox Closure Tests

Phase 1 coverage: read-only captures (no mutation of captured names).
Phase 2 will add the box-rewrite path for mutated captures.
"""

failures = [0]  # list so we can mutate without `global`

def check(actual, expected, description):
    if actual == expected:
        print("  PASS:", description)
    else:
        failures[0] = failures[0] + 1
        print("  FAIL:", description, "expected", expected, "got", actual)

# Lambda capturing one outer parameter (the canonical "make_adder").
def make_adder(n):
    return lambda x: x + n

add5 = make_adder(5)
add10 = make_adder(10)

check(add5(3), 8, "make_adder(5)(3)")
check(add5(7), 12, "make_adder(5)(7)")
check(add10(3), 13, "make_adder(10)(3)")
check(add5(3), 8, "add5 still works after add10 calls")

# Lambda capturing multiple outer parameters.
def make_mixer(a, b):
    return lambda x: a * x + b

line = make_mixer(2, 3)
check(line(2), 7, "2*2 + 3 = 7")
check(line(5), 13, "2*5 + 3 = 13")

# Nested def captures outer local.
def outer_with_def():
    n = 100
    def inner(x):
        return x + n
    return inner

g = outer_with_def()
check(g(5), 105, "def-in-def captures outer local")
check(g(0), 100, "def-in-def: zero arg")

# Triple nesting (curried style). Each level captures from all enclosing scopes.
def curry_add(a):
    def part1(b):
        def part2(c):
            return a + b + c
        return part2
    return part1

check(curry_add(1)(2)(3), 6, "curried 3-level capture: 1+2+3")
check(curry_add(10)(20)(30), 60, "curried 3-level capture: 10+20+30")

# A lambda inside a lambda captures from the outermost.
def make_pair_op(op):
    return lambda x: lambda y: op(x, y)

def add(x, y):
    return x + y

adder = make_pair_op(add)
check(adder(3)(4), 7, "lambda-in-lambda over function-valued capture")

if failures[0] > 0:
    print("FAILURES:", failures[0])
    raise Exception("test_closures: assertions failed")

print("test_closures: all pass")
