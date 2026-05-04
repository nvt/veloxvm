#!/usr/bin/env python3
"""
PyVelox *args feature test.

Exercises `def f(*args)` and `lambda *args: ...`. The runner only
checks exit status; we exit non-zero on any unexpected value.

`bind_function_rest` collects trailing actuals into a list bound to
the rest parameter. Inside the function, `args` behaves like any
list — len(), iteration, indexing, list methods.

CPython exposes args as a tuple; pyvelox represents it as a list,
which is the same storage shape. The immutability distinction isn't
visible from user code anyway.
"""

failures = 0

def expect(label, actual, want):
    global failures
    if actual != want:
        print("FAIL:", label, "got", actual, "want", want)
        failures += 1
    else:
        print("ok:", label, "=", actual)


def collect(*args):
    return list(args)


def total(*args):
    s = 0
    for x in args:
        s = s + x
    return s


def with_label(label, *rest):
    return [label, len(rest)]


# Empty *args.
expect("collect()", collect(), [])
expect("len(collect())", len(collect()), 0)
expect("total()", total(), 0)

# Single arg.
expect("collect(7)", collect(7), [7])
expect("total(7)", total(7), 7)

# Many args.
expect("collect(1,2,3)", collect(1, 2, 3), [1, 2, 3])
expect("total(1,2,3,4,5)", total(1, 2, 3, 4, 5), 15)

# Mixed types in *args.
expect("collect('a','b')", collect('a', 'b'), ['a', 'b'])

# Leading positional + *args.
expect("with_label('hi')", with_label('hi'), ['hi', 0])
expect("with_label('hi',1,2,3)", with_label('hi', 1, 2, 3), ['hi', 3])

# args is indexable like any list.
def first(*args):
    if len(args) > 0:
        return args[0]
    return None


expect("first()", first(), None)
expect("first(42)", first(42), 42)
expect("first(10,20)", first(10, 20), 10)

# lambda with *args.
nopass = lambda *args: len(args)
expect("(lambda *a: len(a))()", nopass(), 0)
expect("(lambda *a: len(a))(1,2,3)", nopass(1, 2, 3), 3)

# *args inside a closure: the inner def captures the outer's local.
def make_appender(prefix):
    def go(*args):
        return [prefix, len(args)]
    return go


app = make_appender("p")
expect("closure-args(): []", app(), ['p', 0])
expect("closure-args(1,2)", app(1, 2), ['p', 2])

if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all *args tests passed")
