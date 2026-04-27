#!/usr/bin/env python3
"""
PyVelox Closure Tests

Phase 1 coverage: read-only captures (no mutation of captured names).
Phase 2 will add the box-rewrite path for mutated captures.
"""

failures = 0

def check(actual, expected, description):
    global failures
    if actual == expected:
        print("  PASS:", description)
    else:
        failures = failures + 1
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

# --- Phase 2: mutable captures (the box rewrite) ---

# A counter that owns a captured-and-mutated local. Each call sees the
# updated value; independent counters do not interfere.
def make_counter():
    count = 0
    def inc():
        nonlocal count
        count = count + 1
        return count
    return inc

c1 = make_counter()
c2 = make_counter()
check(c1(), 1, "counter c1 first call")
check(c1(), 2, "counter c1 second call")
check(c1(), 3, "counter c1 third call")
check(c2(), 1, "counter c2 starts fresh")
check(c1(), 4, "c1 unaffected by c2")

# Two closures sharing the same captured-and-mutated binding. Writes through
# one must be visible through the other -- the captures must reference a
# shared box, not snapshot the value.
def make_pair():
    shared = 0
    def writer():
        nonlocal shared
        shared = shared + 1
        return shared
    def reader():
        return shared
    return [writer, reader]

pair = make_pair()
write_fn = pair[0]
read_fn = pair[1]
check(write_fn(), 1, "shared writer first")
check(write_fn(), 2, "shared writer second")
check(read_fn(), 2, "shared reader sees writer's value")
check(write_fn(), 3, "shared writer third")
check(read_fn(), 3, "shared reader sees latest")

# Deep nesting: innermost function mutates a name two scopes out.
def outer_mut():
    x = 100
    def middle():
        def inner():
            nonlocal x
            x = x + 1
            return x
        return inner
    return middle

deep = outer_mut()()
check(deep(), 101, "deep mutation: 101")
check(deep(), 102, "deep mutation: 102")
check(deep(), 103, "deep mutation: 103")

# Mixed read-only + mutable captures in the same lambda body. The constant
# `base` is captured by value; `n` is captured-and-mutated so it lives in
# a box.
def make_offset_counter(base):
    n = 0
    def step():
        nonlocal n
        n = n + 1
        return base + n
    return step

s = make_offset_counter(1000)
check(s(), 1001, "offset counter step 1")
check(s(), 1002, "offset counter step 2")
check(s(), 1003, "offset counter step 3")

# --- Phase 3: global declaration ---

# `global` makes a name resolve to the program-wide binding, regardless of
# whether it would otherwise be a local of the function.
module_counter = 0

def bump_global():
    global module_counter
    module_counter = module_counter + 1
    return module_counter

check(bump_global(), 1, "global counter first bump")
check(bump_global(), 2, "global counter second bump")
check(module_counter, 2, "global counter visible at module level")

# `global` mixed with a captured local: base is captured by value, total is
# global.
total_acc = 0

def make_global_adder(base):
    def step(x):
        global total_acc
        total_acc = total_acc + base + x
        return total_acc
    return step

ga = make_global_adder(100)
check(ga(1), 101, "global+capture: 0 + 100 + 1")
check(ga(2), 203, "global+capture: 101 + 100 + 2")
check(total_acc, 203, "global+capture: module total visible")

if failures > 0:
    print("FAILURES:", failures)
    raise Exception("test_closures: assertions failed")

print("test_closures: all pass")
