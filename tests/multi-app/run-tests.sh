#!/usr/bin/env bash
# Thin wrapper that runs the multi-app concurrency test suite.
exec python3 "$(dirname "$0")/run-tests.py" "$@"
