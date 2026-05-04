#!/usr/bin/env python3
"""
PyVelox typed exception handlers feature test.

Multiple `except` clauses with type filters, dispatched via
runtime _pyvelox_isinstance. `except Exception:` is treated as a
catch-all alias so the legacy `raise UnboundName(...)` shape and
custom exception classes are both absorbed by it. `except:`
without a type is the same catch-all.
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
# Multi-clause dispatch on custom exception classes.
# ============================================================

class NotFound(Exception):
    pass


class BadInput(Exception):
    pass


class Conflict(Exception):
    pass


def classify(kind):
    try:
        if kind == "missing":
            raise NotFound("no such thing")
        if kind == "bad":
            raise BadInput("malformed")
        if kind == "race":
            raise Conflict("stale")
        if kind == "legacy":
            raise UnboundError("legacy raise")
        if kind == "ok":
            return "ok"
        return "unrecognised"
    except NotFound as e:
        return f"not-found: {e.args[0]}"
    except BadInput as e:
        return f"bad-input: {e.args[0]}"
    except Conflict as e:
        return f"conflict: {e.args[0]}"
    except Exception as e:
        return f"fallback ({e.type}): {e.args[0]}"


expect("classify(ok)", classify("ok"), "ok")
expect("classify(missing)", classify("missing"),
       "not-found: no such thing")
expect("classify(bad)", classify("bad"), "bad-input: malformed")
expect("classify(race)", classify("race"), "conflict: stale")
expect("classify(legacy)", classify("legacy"),
       "fallback (UnboundError): legacy raise")


# ============================================================
# Subclass dispatch: a clause for a base catches subclass instances.
# ============================================================

class Network(Exception):
    pass


class Timeout(Network):
    pass


class DnsFailure(Network):
    pass


def explain(reason):
    try:
        if reason == "timeout":
            raise Timeout("slow")
        if reason == "dns":
            raise DnsFailure("nxdomain")
        if reason == "other":
            raise BadInput("not network")
    except Network as e:
        return f"network: {e.type}"
    except Exception as e:
        return f"other: {e.type}"


expect("explain(timeout)", explain("timeout"), "network: Timeout")
expect("explain(dns)", explain("dns"), "network: DnsFailure")
expect("explain(other)", explain("other"), "other: BadInput")


# ============================================================
# Uncaught typed handler propagates outwards.
# ============================================================

class Inner(Exception):
    pass


class Outer(Exception):
    pass


def filtered():
    try:
        raise Outer("escapes")
    except Inner as e:
        return "wrong-branch"


try:
    filtered()
    expect("uncaught", "ran", "should-have-raised")
except Exception as e:
    expect("uncaught propagates", e.type, "Outer")


# ============================================================
# isinstance still works inside a typed handler -- the bound
# value is a normal exception object.
# ============================================================

def deep_check():
    try:
        raise Timeout("hang")
    except Network as e:
        return [
            isinstance(e, Timeout),
            isinstance(e, Network),
            isinstance(e, Exception),
            isinstance(e, NotFound),
        ]


expect("isinstance inside typed clause",
       deep_check(), [True, True, True, False])


# ============================================================
# break/continue sentinels are still propagated through typed
# handlers -- the loop-sentinel filter wraps the whole dispatch.
# ============================================================

def loop_break():
    hits = 0
    for i in [1, 2, 3]:
        try:
            if i == 2:
                break
            hits = hits + 1
        except NotFound as e:
            hits = hits + 100
        except Exception as e:
            hits = hits + 1000
    return hits


expect("break propagates through typed handlers", loop_break(), 1)


def loop_continue_with_typed_catch():
    collected = []
    for i in [1, 2, 3]:
        try:
            if i == 2:
                raise NotFound("two")
            collected = collected + [i]
        except NotFound as e:
            collected = collected + [f"caught:{e.args[0]}"]
    return collected


expect("typed catch inside loop",
       loop_continue_with_typed_catch(), [1, "caught:two", 3])


# ============================================================
# Re-raise of bound name within a typed handler still works.
# ============================================================

def wrap():
    try:
        try:
            raise BadInput("inner")
        except BadInput as e:
            raise e
    except Exception as outer:
        return outer.args[0]


expect("re-raise in typed handler", wrap(), "inner")


if failures > 0:
    print("FAILURES:", failures)
    raise SystemExit(1)

print("all typed-except tests passed")
