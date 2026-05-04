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


# ============================================================
# Field defaults: trailing fields can have literal default values.
# Synthesised __init__ is variadic and uses argc dispatch to pick
# arg vs default per field.
# ============================================================

@dataclass
class Config:
    host: str
    port: int = 8080
    debug: bool = False
    label: str = "default"


# All defaults consumed.
c1 = Config("api")
expect("Config.host (only required)", c1.host, "api")
expect("Config.port default", c1.port, 8080)
expect("Config.debug default", c1.debug, False)
expect("Config.label default", c1.label, "default")

# Override the first defaulted field.
c2 = Config("api", 9000)
expect("Config.port override", c2.port, 9000)
expect("Config.debug still default", c2.debug, False)
expect("Config.label still default", c2.label, "default")

# Override several.
c3 = Config("api", 7000, True)
expect("Config.debug override", c3.debug, True)
expect("Config.label still default", c3.label, "default")

# Override all.
c4 = Config("api", 7000, True, "custom")
expect("Config.label override", c4.label, "custom")


# All-default fields: zero positional args is enough.
@dataclass
class AllDefaults:
    a: int = 1
    b: int = 2
    c: int = 3


ad = AllDefaults()
expect("AllDefaults.a", ad.a, 1)
expect("AllDefaults.b", ad.b, 2)
expect("AllDefaults.c", ad.c, 3)

ad2 = AllDefaults(10)
expect("AllDefaults a override", ad2.a, 10)
expect("AllDefaults b still default", ad2.b, 2)


# Subclass of a defaulted dataclass inherits the variadic __init__.
class ExtendedConfig(Config):
    def description(self):
        return f"{self.host}:{self.port}"


ec = ExtendedConfig("primary", 4000)
expect("ExtendedConfig host", ec.host, "primary")
expect("ExtendedConfig port", ec.port, 4000)
expect("ExtendedConfig debug default inherited", ec.debug, False)
expect("ExtendedConfig.description", ec.description(), "primary:4000")


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all dataclass tests passed")
