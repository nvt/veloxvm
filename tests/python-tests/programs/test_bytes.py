#!/usr/bin/env python3
"""
PyVelox bytes feature test.

Smoke-tests `b'...'` literals, the `bytes(...)` constructor, length,
indexing, and a couple of cross-shape interactions. The runner only
checks exit status; we exit non-zero on any unexpected value to make
regressions visible.

Bytes are backed by R7RS bytevector storage (buffer-flagged
vectors). See doc/python.md for the supported subset.
"""

failures = 0

def expect(label, actual, want):
    global failures
    if actual != want:
        print("FAIL:", label, "got", actual, "want", want)
        failures += 1
    else:
        print("ok:", label, "=", actual)

# Empty literal.
expect("len(b'')", len(b''), 0)

# Non-empty literal length.
expect("len(b'ABC')", len(b'ABC'), 3)

# Boundary bytes survive round-trip via literal indexing.
b = b'\x00\x01\xfe\xff'
expect("len(b'\\x00\\x01\\xfe\\xff')", len(b), 4)
expect("b[0]", b[0], 0)
expect("b[1]", b[1], 1)
expect("b[2]", b[2], 254)
expect("b[3]", b[3], 255)

# bytes() empty constructor.
e = bytes()
expect("len(bytes())", len(e), 0)

# bytes(N) -> N zero bytes.
zeros = bytes(5)
expect("len(bytes(5))", len(zeros), 5)
expect("zeros[0]", zeros[0], 0)
expect("zeros[4]", zeros[4], 0)

# bytes(list) -> filled buffer.
hi = bytes([72, 105, 33])
expect("len(bytes([72,105,33]))", len(hi), 3)
expect("hi[0]", hi[0], 72)
expect("hi[1]", hi[1], 105)
expect("hi[2]", hi[2], 33)

# bytes(b'...') shares storage; len matches.
shared = bytes(hi)
expect("len(bytes(hi))", len(shared), 3)
expect("bytes(hi)[1]", shared[1], 105)

# Variable-arg dispatch: integer path.
n = 4
expect("len(bytes(n))", len(bytes(n)), 4)

# Variable-arg dispatch: list path.
lst = [1, 2, 3, 4, 5]
expect("len(bytes(lst))", len(bytes(lst)), 5)
expect("bytes(lst)[0]", bytes(lst)[0], 1)
expect("bytes(lst)[4]", bytes(lst)[4], 5)

# len() still works on lists, strings, dicts.
expect("len([1,2,3])", len([1, 2, 3]), 3)
expect("len('hi')", len('hi'), 2)
expect("len({'a':1,'b':2})", len({'a': 1, 'b': 2}), 2)

# Subscript on lists still works.
xs = [10, 20, 30]
expect("xs[1]", xs[1], 20)

if failures > 0:
    print("FAILURES:", failures)
    # The e2e runner treats non-zero exit as failure.
    raise SystemExit(1)

print("all bytes tests passed")
