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


# ============================================================
# Inheritance + super() + isinstance.
# ============================================================

class Animal:
    def __init__(self, name):
        self.name = name

    def describe(self):
        return self.name

    def shout(self):
        return "..."


class Dog(Animal):
    def __init__(self, name, breed):
        super().__init__(name)
        self.breed = breed

    def shout(self):
        return "woof"


class Puppy(Dog):
    def shout(self):
        return f"{super().shout()}-tiny"


# Inherited method (parent's describe), no override.
animal = Animal("generic")
expect("Animal.describe", animal.describe(), "generic")
expect("Animal.shout default", animal.shout(), "...")

# Subclass: super().__init__ runs parent constructor.
dog = Dog("rex", "lab")
expect("Dog.name (inherited init)", dog.name, "rex")
expect("Dog.breed (own init)", dog.breed, "lab")
expect("Dog.describe (inherited)", dog.describe(), "rex")
expect("Dog.shout (override)", dog.shout(), "woof")

# Grandchild: super().shout() reaches Dog's override, not Animal's.
puppy = Puppy("milo", "corgi")
expect("Puppy.name", puppy.name, "milo")
expect("Puppy.breed", puppy.breed, "corgi")
expect("Puppy.shout (super chain)", puppy.shout(), "woof-tiny")

# isinstance walks the chain.
expect("isinstance(puppy, Puppy)", isinstance(puppy, Puppy), True)
expect("isinstance(puppy, Dog)", isinstance(puppy, Dog), True)
expect("isinstance(puppy, Animal)", isinstance(puppy, Animal), True)
expect("isinstance(dog, Puppy)", isinstance(dog, Puppy), False)
expect("isinstance(animal, Dog)", isinstance(animal, Dog), False)
expect("isinstance(\"x\", Animal)", isinstance("x", Animal), False)
expect("isinstance(42, Animal)", isinstance(42, Animal), False)

# Class with no __init__ in the chain still constructs.
class Bare:
    def kind(self):
        return "bare"


bare = Bare()
expect("Bare.kind", bare.kind(), "bare")

# Subclass adds __init__ where the parent has none.
class WithInit(Bare):
    def __init__(self, x):
        self.x = x


w = WithInit(7)
expect("WithInit.x", w.x, 7)
expect("WithInit.kind (inherited)", w.kind(), "bare")
expect("isinstance(w, Bare)", isinstance(w, Bare), True)

# Diamond-style chain: A -> B -> C, all calling super().__init__.
class A:
    def __init__(self):
        self.a = 1


class B(A):
    def __init__(self):
        super().__init__()
        self.b = 2


class C(B):
    def __init__(self):
        super().__init__()
        self.c = 3


c = C()
expect("C.a (from A's init)", c.a, 1)
expect("C.b (from B's init)", c.b, 2)
expect("C.c (own init)", c.c, 3)


# ============================================================
# Construction paths the compiler resolves statically.
# ============================================================

# No __init__ anywhere in the chain: construction lowers to a bare
# allocation, with no init call to make. Methods and attribute
# attachment must still work on the result.
class NoInit:
    def label(self):
        return "no-init"


class NoInitChild(NoInit):
    pass


plain = NoInitChild()
expect("no-init subclass method", plain.label(), "no-init")
plain.tag = 7
expect("no-init subclass attribute", plain.tag, 7)


# Constructing a class from inside its own method body. The class
# isn't statically resolvable while its own methods are still being
# compiled, so this keeps taking the runtime-lookup path -- it has to
# build a working instance all the same.
class Node:
    def __init__(self, v):
        self.v = v
        self.link = None

    def prepend(self, v):
        head = Node(v)
        head.link = self
        return head


tail = Node(1)
head = tail.prepend(2)
expect("self-constructing method", head.v, 2)
expect("self-constructed link", head.link.v, 1)


# A user-written __init__ with defaults. The closure is fixed-arity,
# so the construction site is what fills the missing tail in.
class Defaulted:
    def __init__(self, a, b=7, c="z"):
        self.a = a
        self.b = b
        self.c = c


d1 = Defaulted(1)
expect("user init required arg", d1.a, 1)
expect("user init default b", d1.b, 7)
expect("user init default c", d1.c, "z")

d2 = Defaulted(1, 2)
expect("user init overridden b", d2.b, 2)
expect("user init default c kept", d2.c, "z")

d3 = Defaulted(1, 2, "y")
expect("user init all supplied", d3.c, "y")


# The same, inherited: the subclass adds no __init__, so both the
# closure and its defaults come from the base.
class DefaultedChild(Defaulted):
    def total(self):
        return self.a + self.b


dc = DefaultedChild(5)
expect("inherited defaulted init", dc.total(), 12)


# ============================================================
# Defaults on methods, which are reached through the runtime
# lookup and so carry their own padding rather than relying on
# the call site.
# ============================================================

class Calc:
    def __init__(self, base):
        self.base = base

    def add(self, a, b=10, c=100):
        return self.base + a + b + c


calc = Calc(1000)
expect("method all args", calc.add(1, 2, 3), 1006)
expect("method one default", calc.add(1, 2), 1103)
expect("method two defaults", calc.add(1), 1111)


# Inherited, and reached through super() from an override.
class CalcChild(Calc):
    def add(self, a, b=20, c=200):
        return super().add(a) + 1


cc = CalcChild(0)
expect("override defaults", cc.add(1), 112)


# A module function may share a name with a method without
# inheriting its signature.
def add(a, b=5):
    return a + b


expect("module function keeps own defaults", add(1), 6)
expect("module function explicit", add(1, 2), 3)


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all class tests passed")
