#!/usr/bin/env python3
"""
PyVelox class feature test: class definitions, __init__, instance
attribute access, method dispatch (and inheritance + super +
isinstance, added in subsequent sections below).

Wire format: classes are tagged vectors `#(pyclass "Name" ...)`;
instances are `#(pyinstance class slot-alist)`. Attribute access
lowers through `_pyvelox_get_attr` / `_pyvelox_set_attr`; method
calls go through `_pyvelox_lookup_method`.
"""

failures = 0

def expect(label, actual, want):
    global failures
    if actual != want:
        print("FAIL:", label, "got", actual, "want", want)
        failures += 1
    else:
        print("ok:", label, "=", actual)


# ============================================================
# Empty class with explicit pass.
# ============================================================

class Empty:
    pass


_empty = Empty()
# Pyvelox doesn't support `is`, so just probe that an attribute set
# afterwards round-trips -- proves we got back a real instance.
_empty.tag = "ok"
expect("Empty() compiles", _empty.tag, "ok")


# ============================================================
# Constructor + simple slot.
# ============================================================

class Box:
    def __init__(self, v):
        self.v = v


a = Box(10)
b = Box(20)
expect("a.v after init", a.v, 10)
expect("b.v after init", b.v, 20)

# Independence of instances.
a.v = 99
expect("a.v after mutate", a.v, 99)
expect("b.v unchanged", b.v, 20)

# Adding a slot post-init (CPython allows; pyvelox grows the alist).
b.extra = "hi"
expect("b.extra after dynamic add", b.extra, "hi")


# ============================================================
# Methods that read self.
# ============================================================

class Counter:
    def __init__(self, start):
        self.n = start

    def value(self):
        return self.n

    def step(self, by):
        self.n = self.n + by
        return self.n


c = Counter(5)
expect("Counter.value initial", c.value(), 5)
expect("Counter.step(3)", c.step(3), 8)
expect("Counter.step(2)", c.step(2), 10)
expect("Counter.value final", c.value(), 10)


# ============================================================
# Method calling another method on self.
# ============================================================

class Adder:
    def __init__(self, n):
        self.n = n

    def doubled(self):
        return self.add(self.n)

    def add(self, x):
        return self.n + x


ad = Adder(7)
expect("Adder.add", ad.add(3), 10)
expect("Adder.doubled", ad.doubled(), 14)


# ============================================================
# Method that returns a fresh instance of the same class.
# Exercises the "class registered before method-body compilation"
# ordering -- without that, recursive constructor calls would fall
# through to regular function dispatch and error at runtime.
# ============================================================

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def magnitude_sq(self):
        return self.x * self.x + self.y * self.y

    def add(self, other):
        return Point(self.x + other.x, self.y + other.y)


p = Point(3, 4)
q = Point(1, 2)
r = p.add(q)
expect("Point sum x", r.x, 4)
expect("Point sum y", r.y, 6)
expect("Point.magnitude_sq", r.magnitude_sq(), 52)
expect("p.magnitude_sq unchanged", p.magnitude_sq(), 25)


# ============================================================
# Method with *args.
# ============================================================

class Logger:
    def __init__(self):
        self.entries = []

    def log(self, *items):
        self.entries = self.entries + [items]
        return len(items)


lg = Logger()
expect("Logger.log returns count 0", lg.log(), 0)
expect("Logger.log returns count 3", lg.log("a", "b", "c"), 3)
expect("Logger.entries length", len(lg.entries), 2)


# ============================================================
# Class with no __init__ -- bare instantiation.
# ============================================================

class Tag:
    def kind(self):
        return "tag"


t = Tag()
expect("Tag.kind", t.kind(), "tag")
t.label = "x"
expect("Tag.label after set", t.label, "x")


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all class tests passed")
