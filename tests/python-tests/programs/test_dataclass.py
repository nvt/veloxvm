#!/usr/bin/env python3
"""
PyVelox @dataclass feature test.

@dataclass on a class with `name: type` annotated fields
synthesises an __init__(self, f1, f2, ...) that stores each
parameter into the matching slot. Methods can coexist with
fields. A user-supplied __init__ takes precedence over the
synthesised one.

Field defaults are validated as literal constants and assigned by
the synthesised init when their position isn't supplied at the
call site (see the field-defaults section below).
"""

failures = 0

def expect(label, actual, want):
    global failures
    if actual != want:
        print("FAIL:", label, "got", actual, "want", want)
        failures += 1
    else:
        print("ok:", label, "=", actual)


# Plain dataclass with two scalar fields.
@dataclass
class Point:
    x: int
    y: int


p = Point(3, 4)
expect("Point.x", p.x, 3)
expect("Point.y", p.y, 4)
# Mutability: the slot-alist is mutable like any other instance state.
p.x = 99
expect("Point.x after mutate", p.x, 99)


# Dataclass with three fields of mixed types.
@dataclass
class Record:
    name: str
    code: int
    active: bool


r = Record("widget", 42, True)
expect("Record.name", r.name, "widget")
expect("Record.code", r.code, 42)
expect("Record.active", r.active, True)


# Methods coexist with fields. Synthesised __init__ runs the
# field assignments; user-defined methods are unchanged.
@dataclass
class Vec:
    a: int
    b: int

    def magnitude_sq(self):
        return self.a * self.a + self.b * self.b

    def scaled(self, k):
        return Vec(self.a * k, self.b * k)


v = Vec(3, 4)
expect("Vec.a", v.a, 3)
expect("Vec.b", v.b, 4)
expect("Vec.magnitude_sq", v.magnitude_sq(), 25)
v2 = v.scaled(2)
expect("Vec.scaled.a", v2.a, 6)
expect("Vec.scaled.b", v2.b, 8)
expect("original v.a unchanged", v.a, 3)


# User-provided __init__ skips synthesis. The annotation list
# becomes a no-op for __init__-construction purposes; only the
# user's __init__ runs.
@dataclass
class Doubled:
    x: int

    def __init__(self, x):
        self.x = x * 2


d = Doubled(5)
expect("Doubled.x via custom init", d.x, 10)


# Single-field dataclass.
@dataclass
class Wrapper:
    inner: int


w = Wrapper(7)
expect("Wrapper.inner", w.inner, 7)


# isinstance still works on dataclass instances.
expect("isinstance(p, Point)", isinstance(p, Point), True)
expect("isinstance(p, Vec)", isinstance(p, Vec), False)


# Inheritance: a non-dataclass subclass of a dataclass inherits
# the synthesised __init__, so constructing the subclass with the
# parent's signature works.
class TaggedPoint(Point):
    def label(self):
        return f"({self.x}, {self.y})"


tp = TaggedPoint(10, 20)
expect("TaggedPoint.x (inherited init)", tp.x, 10)
expect("TaggedPoint.y (inherited init)", tp.y, 20)
expect("TaggedPoint.label", tp.label(), "(10, 20)")
expect("isinstance(tp, Point)", isinstance(tp, Point), True)


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all dataclass tests passed")
