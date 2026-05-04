#!/usr/bin/env python3
"""
PyVelox @classmethod / @staticmethod feature test.

Both decorators wrap the underlying method at class-def time so all
three method kinds (instance, class, static) share the universal
`(recv args...)` calling convention. The wrappers adjust what the
real method sees as its first arg:

- instance: receives the actual object.
- class: receives the class object (resolved via _pyvelox_class_of
  so `Class.cm()` and `obj.cm()` both work).
- static: receives no implicit first arg.

`cls(args)` inside a classmethod body routes through _pyvelox_invoke
which dispatches at runtime: a class object goes through
make_instance, anything else through apply. This makes the standard
"alternative constructor" pattern work.
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
# classmethod as alternative constructor.
# ============================================================

class Counter:
    def __init__(self, n):
        self.n = n

    @classmethod
    def zero(cls):
        return cls(0)

    @classmethod
    def doubled(cls, n):
        return cls(n * 2)

    @staticmethod
    def add(x, y):
        return x + y

    def value(self):
        return self.n


# Through the class.
c1 = Counter.zero()
expect("Counter.zero().value", c1.value(), 0)

c2 = Counter.doubled(7)
expect("Counter.doubled(7).value", c2.value(), 14)

# Through an instance: same dispatch.
c3 = c1.zero()
expect("c1.zero().value", c3.value(), 0)

c4 = c2.doubled(5)
expect("c2.doubled(5).value", c4.value(), 10)

# isinstance still works on classmethod-constructed instances.
expect("isinstance(c1, Counter)", isinstance(c1, Counter), True)


# ============================================================
# staticmethod ignores the receiver.
# ============================================================

# Through the class.
expect("Counter.add(2, 3)", Counter.add(2, 3), 5)

# Through an instance.
expect("c1.add(10, 20)", c1.add(10, 20), 30)


# ============================================================
# Subclass inherits cm/sm. cls in a classmethod resolves to the
# actual subclass, so cls(0) returns a subclass instance.
# ============================================================

class StepCounter(Counter):
    def step(self):
        self.n = self.n + 1
        return self.n


# Subclass calling the inherited classmethod.
s1 = StepCounter.zero()
expect("StepCounter.zero() type",
       isinstance(s1, StepCounter), True)
expect("StepCounter.zero() also Counter",
       isinstance(s1, Counter), True)
expect("StepCounter.zero() has subclass method",
       s1.step(), 1)
expect("StepCounter.value after step", s1.value(), 1)

# doubled also constructs subclass instance.
s2 = StepCounter.doubled(3)
expect("StepCounter.doubled(3) type",
       isinstance(s2, StepCounter), True)
expect("StepCounter.doubled(3).value", s2.value(), 6)

# staticmethod inherits unchanged.
expect("StepCounter.add", StepCounter.add(1, 2), 3)


# ============================================================
# Multiple classmethods coexisting; one calling another via cls.
# ============================================================

class Greeter:
    def __init__(self, msg):
        self.msg = msg

    @classmethod
    def hello(cls):
        return cls("hello")

    @classmethod
    def world(cls):
        return cls("world")

    @classmethod
    def greet(cls, name):
        # Calls another classmethod through cls -- exercises
        # _pyvelox_class_of via the method-dispatch path.
        h = cls.hello()
        return f"{h.msg} {name}"


expect("Greeter.hello().msg", Greeter.hello().msg, "hello")
expect("Greeter.world().msg", Greeter.world().msg, "world")
expect("Greeter.greet via cls", Greeter.greet("there"), "hello there")


# ============================================================
# super() in a classmethod body uses cls as the first arg.
# ============================================================

class Base:
    @classmethod
    def label(cls):
        return "base"


class Mid(Base):
    @classmethod
    def label(cls):
        return f"mid-on-{super().label()}"


expect("Mid.label", Mid.label(), "mid-on-base")
expect("Base.label", Base.label(), "base")


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all classmethod/staticmethod tests passed")
