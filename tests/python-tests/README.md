# PyVelox Test Suite

Tests for the PyVelox Python-to-VeloxVM compiler. There are two layers:

- **`compiler/`** — Python `unittest` tests of the compiler itself
  (encoder, translator, edge cases, located errors).
- **`programs/`** — end-to-end tests that compile a `.py` file with
  `pyvelox-compile` and run the resulting bytecode on `bin/vm`.

## Running

```sh
./tests/python-tests/run-tests.sh
```

That builds, compiles, and runs both layers and prints a summary. The
end-to-end runner only checks the VM's exit code, so a runtime error
in a test program is currently invisible — runtime assertions in
`programs/test_pyvelox_suite.py` print explicit `PASS`/`FAIL` lines
that you can scan visually.

To run an individual program test:

```sh
./languages/python/pyvelox-compile tests/python-tests/programs/test_basic_features.py
./bin/vm tests/python-tests/programs/test_basic_features.vm
```

To run the compiler unit tests directly:

```sh
PYTHONPATH=languages/python python3 -m unittest discover -s tests/python-tests/compiler -v
```

## Programs

| File | What it covers |
|------|----------------|
| `test_basic_features.py` | Demonstrative tour of the supported feature set (arithmetic, lists, dicts, control flow, built-ins). |
| `test_closures.py` | Lexical scope, free-variable capture, mutable captures via box rewrite. |
| `test_pyvelox_suite.py` | Broader suite with `PASS`/`FAIL` print-out. Skips the documented footguns (see `doc/python.md`'s status table). |

## Adding tests

- Prefer reusing `test_pyvelox_suite.py`'s `test(condition, label)`
  helper for new cases; new files only when the surface area calls
  for it (e.g. a major language feature).
- Avoid features marked `Buggy` in `doc/python.md`'s language status
  table — those compile but produce wrong runtime behaviour.
- Print enough output to make the failure mode obvious; the runner
  doesn't surface runtime errors otherwise.

## See also

- `doc/python.md` — supported-feature matrix and runtime caveats.
- `languages/python/pyvelox/` — compiler implementation.
- `tests/unit-tests/` — Scheme/R5RS test framework (separate runner).
