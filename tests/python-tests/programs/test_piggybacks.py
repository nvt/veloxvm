#!/usr/bin/env python3
"""
PyVelox cheap-piggyback feature test.

`str.casefold()` and `math.isqrt()` -- both backed by primitives /
patterns that already exist in the VM (char_downcase via the
str.lower path; Newton's method via a runtime helper modelled on
the Scheme exact-integer-sqrt). `import math` is a compiler-level
no-op so the idiomatic `import math; math.isqrt(...)` shape works
without needing the VM to define a math library.
"""

import math

failures = 0

def expect(label, actual, want):
    global failures
    if actual != want:
        print("FAIL:", label, "got", actual, "want", want)
        failures += 1
    else:
        print("ok:", label, "=", actual)


# ============================================================
# str.casefold (ASCII -- identical to lower)
# ============================================================

expect("casefold ALL CAPS", "HELLO".casefold(), "hello")
expect("casefold MixedCase", "MixedCase".casefold(), "mixedcase")
expect("casefold already lower", "lower".casefold(), "lower")
expect("casefold empty", "".casefold(), "")
expect("casefold digits unchanged", "abc123!".casefold(), "abc123!")
# Variable receiver too.
s = "WORLD"
expect("casefold(variable)", s.casefold(), "world")


# ============================================================
# math.isqrt (Newton's method)
# ============================================================

expect("isqrt(0)", math.isqrt(0), 0)
expect("isqrt(1)", math.isqrt(1), 1)
expect("isqrt(2)", math.isqrt(2), 1)
expect("isqrt(3)", math.isqrt(3), 1)
expect("isqrt(4)", math.isqrt(4), 2)
expect("isqrt(8)", math.isqrt(8), 2)
expect("isqrt(9)", math.isqrt(9), 3)
expect("isqrt(15)", math.isqrt(15), 3)
expect("isqrt(16)", math.isqrt(16), 4)
expect("isqrt(99)", math.isqrt(99), 9)
expect("isqrt(100)", math.isqrt(100), 10)
expect("isqrt(10000)", math.isqrt(10000), 100)
# Random larger values.
expect("isqrt(123456)", math.isqrt(123456), 351)
expect("isqrt(1000000)", math.isqrt(1000000), 1000)

# Variable input.
n = 49
expect("isqrt(variable)", math.isqrt(n), 7)

# Negative argument raises ValueError.
try:
    math.isqrt(-1)
    expect("isqrt(-1) raised", False, True)
except Exception as e:
    expect("isqrt(-1) type", e.type, "ValueError")
    expect("isqrt(-1) str non-empty", len(str(e)) > 0, True)


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all piggyback tests passed")
