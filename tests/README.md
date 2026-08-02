# Testing

Test suites are self-contained and expose a `run-tests.sh` entry point:

```sh
tests/unit-tests/run-tests.sh
tests/primitives/run-tests.sh
tests/python-tests/run-tests.sh
tests/multi-app/run-tests.sh
```

See [`python-tests/README.md`](python-tests/README.md) for Python frontend test
details. The OTA suite has its own host harness in `tests/ota/run-tests.sh`.

## Required software

### For functional testing

- Python 3.x
- Pexpect

### For fuzzing

- American Fuzzy Lop (`afl-fuzz`)
