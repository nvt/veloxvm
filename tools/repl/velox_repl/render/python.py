"""Render a decoded VObj as Python surface syntax.

VObj kinds map to their closest Python equivalents (list -> [],
vector -> [], symbol -> 'name as identifier-string'). Kinds that
don't have a direct Python counterpart (pair, char, none) get a
best-effort representation.
"""

from __future__ import annotations

from velox_repl.render.decode import VObj


def render(obj: VObj) -> str:
    return _fmt(obj)


def _fmt(obj: VObj) -> str:
    k = obj.kind
    if k == "none":
        return "None"
    if k == "bool":
        return "True" if obj.value else "False"
    if k == "int":
        return str(obj.value)
    if k == "real":
        return repr(obj.value)
    if k == "char":
        return repr(obj.value)
    if k == "string":
        return repr(obj.value)
    if k == "symbol":
        # No first-class symbol type in Python; show as a quoted identifier
        return repr(obj.value)
    if k == "list":
        return "[" + ", ".join(_fmt(item) for item in obj.value) + "]"
    if k == "vector":
        return "[" + ", ".join(_fmt(item) for item in obj.value) + "]"
    if k == "pair":
        car, cdr = obj.value
        return "(" + _fmt(car) + ", " + _fmt(cdr) + ")"
    if k == "opaque":
        return f"<{obj.value}>"
    return f"<unknown:{k}>"
