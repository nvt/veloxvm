#!/bin/sh
# Verify that primitive-ID definitions across the VM and the compilers
# match the canonical table in doc/primitives.md.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
exec python3 "$SCRIPT_DIR/check.py"
