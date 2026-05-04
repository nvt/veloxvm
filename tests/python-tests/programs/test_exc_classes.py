#!/usr/bin/env python3
"""
PyVelox custom exception classes feature test.

Classes that subclass the auto-injected `Exception` base. raise
X(args...) on a class name routes through instance construction
instead of the legacy py-exception tagged-vector shape. Both
shapes coexist so legacy `raise UnboundName("msg")` still works.

Wire format. Custom exception instances are pyinstance vectors
whose slot-alist holds `args` and `type`, populated by Exception's
auto-injected __init__. Handler-side `e.args` / `e.type` /
`str(e)` / `f"{e}"` go through the unified `_pyvelox_get_attr` and
`_pyvelox_str` runtime helpers, which recognise both shapes.
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
# Custom exception class with no body of its own.
# ============================================================

class NotFound(Exception):
    pass


try:
    raise NotFound("user 42")
except Exception as e:
    expect("NotFound type", e.type, "NotFound")
    expect("NotFound args", e.args, ["user 42"])
    expect("NotFound first arg", e.args[0], "user 42")
    expect("str(NotFound)", str(e), "user 42")
    expect("f-string NotFound", f"err: {e}", "err: user 42")
    expect("isinstance NotFound", isinstance(e, NotFound), True)
    expect("isinstance Exception", isinstance(e, Exception), True)


# ============================================================
# Multiple custom exception classes; isinstance discriminates.
# ============================================================

class BadInput(Exception):
    pass


class Timeout(Exception):
    pass


def caught_kind(exc):
    return [exc.type,
            isinstance(exc, BadInput),
            isinstance(exc, Timeout)]


try:
    raise BadInput("malformed")
except Exception as e:
    expect("BadInput discriminate",
           caught_kind(e), ["BadInput", True, False])

try:
    raise Timeout("slow", 30)
except Exception as e:
    expect("Timeout discriminate",
           caught_kind(e), ["Timeout", False, True])
    expect("Timeout multi args", e.args, ["slow", 30])
    expect("Timeout str (first arg)", str(e), "slow")


# ============================================================
# Custom subclass that overrides __init__ but calls super().
# Adds its own slot beyond what Exception sets. The `code` slot
# is purely an illustrative custom field -- nothing inside the VM
# associates this class with any specific subsystem; it's just a
# Python-level exception with extra state.
# ============================================================

class TaggedError(Exception):
    def __init__(self, code, msg):
        super().__init__(msg)
        self.code = code


try:
    raise TaggedError(404, "not found")
except Exception as e:
    expect("TaggedError type", e.type, "TaggedError")
    expect("TaggedError code (custom slot)", e.code, 404)
    expect("TaggedError args (from super)", e.args, ["not found"])
    expect("TaggedError str", str(e), "not found")


# ============================================================
# Bare `raise X` (no constructor call) on a defined class --
# instance with empty args.
# ============================================================

try:
    raise NotFound
except Exception as e:
    expect("Bare-class type", e.type, "NotFound")
    expect("Bare-class args", e.args, [])
    expect("Bare-class str (empty)", str(e), "")


# ============================================================
# Re-raise of bound name passes the same instance through.
# ============================================================

try:
    try:
        raise NotFound("inner")
    except Exception as e:
        # Confirm the attributes are visible on the bound name.
        expect("re-raise inner type", e.type, "NotFound")
        raise e
except Exception as outer:
    expect("re-raise outer type", outer.type, "NotFound")
    expect("re-raise outer args", outer.args, ["inner"])


# ============================================================
# Wrap-and-rethrow: catch one custom exception, raise a different
# class with a synthesised message.
# ============================================================

try:
    try:
        raise BadInput("low-level")
    except Exception as e:
        raise TaggedError(400, f"bad: {e.args[0]}")
except Exception as outer:
    expect("wrap type", outer.type, "TaggedError")
    expect("wrap code", outer.code, 400)
    expect("wrap args", outer.args, ["bad: low-level"])


# ============================================================
# Legacy: raise of an undefined name still produces a py-exception
# vector. Handler-side access works through the same paths.
# ============================================================

try:
    raise SomethingNotDefined("legacy")
except Exception as e:
    expect("legacy type", e.type, "SomethingNotDefined")
    expect("legacy args", e.args, ["legacy"])
    expect("legacy str", str(e), "legacy")


# ============================================================
# Built-in Exception() works directly (auto-injected).
# ============================================================

try:
    raise Exception("vanilla")
except Exception as e:
    expect("Exception type", e.type, "Exception")
    expect("Exception args", e.args, ["vanilla"])
    expect("Exception str", str(e), "vanilla")
    expect("Exception isinstance self", isinstance(e, Exception), True)


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all custom exception class tests passed")
