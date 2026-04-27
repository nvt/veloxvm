#!/usr/bin/env python3
"""Verify that every primitive-ID definition in the tree matches the
canonical table in doc/primitives.md.

The table is the single source of truth for primitive order. The
following files are checked:

  * core/vm-procedures.c - VM_OPERATOR entries (C identifier column)
  * core/vm-symbols.c    - symbol_map SYM(...) entries (Scheme column)
  * languages/scheme-racket/primitives.rkt (Scheme column)
  * languages/python/pyvelox/primitives.py (C identifier column)

Any disagreement with the canonical table - in order, count, or name -
is reported and the script exits non-zero.
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]


def read(path: str) -> str:
    with open(REPO / path, encoding="utf-8") as f:
        return f.read()


def parse_canonical() -> tuple[list[str], list[str]]:
    """Return (c_names, scheme_names) from doc/primitives.md."""
    txt = read("doc/primitives.md")
    c_names, scheme_names = [], []
    row = re.compile(r"^\|\s*(\d+)\s*\|\s*`([^`]+)`\s*\|\s*`([^`]+)`\s*\|\s*$")
    for line in txt.splitlines():
        m = row.match(line)
        if not m:
            continue
        idx = int(m.group(1))
        if idx != len(c_names):
            raise SystemExit(
                f"doc/primitives.md: expected index {len(c_names)}, got {idx}"
            )
        c_names.append(m.group(2))
        scheme_names.append(m.group(3))
    if not c_names:
        raise SystemExit("doc/primitives.md: no primitive rows found")
    return c_names, scheme_names


def parse_vm_procedures() -> list[str]:
    body = re.search(
        r"operators\[\]\s*=\s*\{(.*?)\n\};", read("core/vm-procedures.c"), re.S
    )
    if not body:
        raise SystemExit("could not locate operators[] in core/vm-procedures.c")
    return re.findall(r"VM_OPERATOR\(\s*(\w+)", body.group(1))


def parse_vm_symbols() -> list[str]:
    body = re.search(
        r"symbol_map\[\]\s*=\s*\{(.*?)^\};",
        read("core/vm-symbols.c"),
        re.S | re.M,
    )
    if not body:
        raise SystemExit("could not locate symbol_map[] in core/vm-symbols.c")
    return re.findall(r'SYM\("([^"]*)"\)', body.group(1))


def parse_racket() -> list[str]:
    body = re.search(
        r"\(define vm-primitives\s+'\((.*?)\)\)",
        read("languages/scheme-racket/primitives.rkt"),
        re.S,
    )
    if not body:
        raise SystemExit(
            "could not locate vm-primitives in languages/scheme-racket/primitives.rkt"
        )
    # Strip comments and whitespace.
    tokens = re.sub(r";.*", "", body.group(1)).split()
    return tokens


def parse_python() -> list[str]:
    # Avoid importing the module (it has runtime dependencies).
    body = re.search(
        r"VM_PRIMITIVES\s*=\s*\[(.*?)\n\]",
        read("languages/python/pyvelox/primitives.py"),
        re.S,
    )
    if not body:
        raise SystemExit("could not locate VM_PRIMITIVES in primitives.py")
    return re.findall(r"'([^']+)'", body.group(1))


def compare(source_name: str, source_list: list[str], expected: list[str]) -> int:
    """Return number of errors reported for this source."""
    errors = 0
    if len(source_list) != len(expected):
        print(
            f"  {source_name}: has {len(source_list)} entries, "
            f"expected {len(expected)}"
        )
        errors += 1
    for i, (got, want) in enumerate(zip(source_list, expected)):
        if got != want:
            print(f"  {source_name}: index {i}: {got!r} != expected {want!r}")
            errors += 1
            if errors >= 10:
                print(f"  {source_name}: ... (further divergences suppressed)")
                return errors
    return errors


def main() -> int:
    c_names, scheme_names = parse_canonical()
    print(f"Canonical table: {len(c_names)} primitives")

    total_errors = 0
    sources = [
        ("core/vm-procedures.c", parse_vm_procedures(), c_names),
        ("core/vm-symbols.c", parse_vm_symbols(), scheme_names),
        ("languages/scheme-racket/primitives.rkt", parse_racket(), scheme_names),
        ("languages/python/pyvelox/primitives.py", parse_python(), c_names),
    ]
    for name, got, want in sources:
        errors = compare(name, got, want)
        if errors == 0:
            print(f"  {name}: OK")
        total_errors += errors

    if total_errors == 0:
        print("All sources agree with doc/primitives.md.")
        return 0
    print(f"\n{total_errors} divergence(s) found.")
    return 1


if __name__ == "__main__":
    sys.exit(main())
