"""Render a decoded VObj as Scheme surface syntax."""

from __future__ import annotations

from velox_repl.render.decode import VObj


def render(obj: VObj) -> str:
    return _fmt(obj)


def _fmt(obj: VObj) -> str:
    k = obj.kind
    if k == "none":
        return "#<unspecified>"
    if k == "bool":
        return "#t" if obj.value else "#f"
    if k == "int":
        return str(obj.value)
    if k == "real":
        return _fmt_real(obj.value)
    if k == "rational":
        num, den = obj.value
        # Defensive: a denominator of 0 should never reach the wire,
        # but render as a fallback rather than raising on display.
        if den == 0:
            return f"#<rational {num}/0>"
        return f"{num}/{den}"
    if k == "char":
        return _fmt_char(obj.value)
    if k == "string":
        return _quote(obj.value)
    if k == "symbol":
        return obj.value
    if k == "list":
        if not obj.value:
            return "()"
        return "(" + " ".join(_fmt(item) for item in obj.value) + ")"
    if k == "vector":
        return "#(" + " ".join(_fmt(item) for item in obj.value) + ")"
    if k == "pair":
        car, cdr = obj.value
        return "(" + _fmt(car) + " . " + _fmt(cdr) + ")"
    if k == "opaque":
        return f"#<{obj.value}>"
    return f"#<unknown:{k}>"


def _fmt_real(v: float) -> str:
    if v != v:  # NaN
        return "+nan.0"
    if v == float("inf"):
        return "+inf.0"
    if v == float("-inf"):
        return "-inf.0"
    if v == int(v):
        return f"{v:.1f}"
    return repr(v)


def _fmt_char(c: str) -> str:
    named = {
        " ": "#\\space",
        "\n": "#\\newline",
        "\t": "#\\tab",
        "\r": "#\\return",
    }
    if c in named:
        return named[c]
    if c.isprintable():
        return f"#\\{c}"
    return f"#\\x{ord(c):x}"


def _quote(s: str) -> str:
    out = ['"']
    for ch in s:
        if ch == "\\":
            out.append("\\\\")
        elif ch == '"':
            out.append('\\"')
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\t":
            out.append("\\t")
        elif ch == "\r":
            out.append("\\r")
        else:
            out.append(ch)
    out.append('"')
    return "".join(out)
