#!/usr/bin/env python3
"""
PyVelox structured-raise feature test.

Exercises that `raise X(args...)` now carries the message and
positional arguments through to the handler, accessible as
`e.args` and the type symbol via `e.type`.

Wire format is a 3-slot tagged vector
`#(py-exception type-sym args-list)`; the predicate-style access is
positional vector_ref under the hood.
"""

failures = 0

def expect(label, actual, want):
    global failures
    if actual != want:
        print("FAIL:", label, "got", actual, "want", want)
        failures += 1
    else:
        print("ok:", label, "=", actual)


# Single-arg raise: e.args[0] survives.
try:
    raise ValueError("bad input")
except Exception as e:
    expect("single args", e.args, ["bad input"])
    expect("single args[0]", e.args[0], "bad input")
    expect("single len(args)", len(e.args), 1)
    expect("single type", e.type, 'ValueError')

# Zero-arg raise: e.args is empty.
try:
    raise RuntimeError()
except Exception as e:
    expect("zero args", e.args, [])
    expect("zero len(args)", len(e.args), 0)
    expect("zero type", e.type, 'RuntimeError')

# Multi-arg raise: every arg lands in args list.
try:
    raise KeyError("step", "broke", 42)
except Exception as e:
    expect("multi args", e.args, ["step", "broke", 42])
    expect("multi len(args)", len(e.args), 3)
    expect("multi type", e.type, 'KeyError')

# Bare class name (no constructor call): args is empty, type carried.
try:
    raise IndexError
except Exception as e:
    expect("bare type", e.type, 'IndexError')
    expect("bare args", e.args, [])

# Bare `raise` falls back to a generic Exception.
try:
    raise
except Exception as e:
    expect("bare raise type", e.type, 'Exception')
    expect("bare raise args", e.args, [])

# Computed argument: not just literals. (f-string concatenation,
# avoiding `"prefix " + str(x)` which is a known pyvelox gap.)
def msg_for(code):
    return f"code-{code}"


try:
    raise ValueError(msg_for(5))
except Exception as e:
    expect("computed arg", e.args[0], "code-5")

# Re-raise via the bound name passes the original object through
# untouched; outer handler sees the same type and args.
try:
    try:
        raise TypeError("first floor")
    except Exception as e:
        # Sanity: the inner handler also sees the message.
        expect("inner re-raise inner", e.args[0], "first floor")
        raise e
except Exception as outer:
    expect("inner re-raise outer", outer.args[0], "first floor")
    expect("inner re-raise type", outer.type, 'TypeError')

# Re-raise as a fresh exception (different type, transformed message).
try:
    try:
        raise ValueError("low-level")
    except Exception as e:
        raise RuntimeError(f"wrapper: {e.args[0]}")
except Exception as outer:
    expect("wrap type", outer.type, 'RuntimeError')
    expect("wrap msg", outer.args[0], "wrapper: low-level")

# Loop-control sentinels still work: a user `except:` inside a loop
# must not absorb break/continue.
hits = 0
for i in [1, 2, 3]:
    try:
        if i == 2:
            break
        hits = hits + 1
    except Exception as e:
        # Should never run; break must not be caught here.
        hits = hits + 100


expect("break still propagates", hits, 1)

# A loop body that raises, gets caught, and continues the loop.
collected = []
for i in [1, 2, 3]:
    try:
        if i == 2:
            raise ValueError("two")
        collected = collected + [i]
    except Exception as e:
        collected = collected + [f"caught:{e.args[0]}"]


expect("loop catches and continues", collected, [1, "caught:two", 3])

if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all raise tests passed")
