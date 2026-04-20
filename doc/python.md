# PyVelox - Python to VeloxVM bytecode compiler

PyVelox compiles a subset of Python source to VeloxVM bytecode that runs
on the same VM as the Scheme and Cyclus front-ends. It targets IoT
scripting, not CPython compatibility.

## Usage

```sh
./pyvelox-compile hello.py             # writes hello.vm
./pyvelox-compile -o program.vm src.py
./pyvelox-compile --dump fibonacci.py  # inspect bytecode
```

From the repository root, `./compile.sh basic/hello` and
`./run.sh basic/hello` drive the whole build and execute the result.

Programmatic access:

```python
from pyvelox import compile_file, compile_string
compile_file("program.py", "output.vm")
bytecode = compile_string('print("Hello, World!")')
```

## Supported

- **Types**: int, bool, str, list, dict (association list), None.
- **Control flow**: if/elif/else, for, while, ternary, break, continue.
- **Functions**: def, lambda, return, tuple unpacking (assignments
  and for loops).
- **Operators**: arithmetic, comparison, logical, bitwise, unary.
- **Slicing** (no step): lists, strings, and VM vectors; negative
  indices supported.
- **Exception handling**: `try`/`except`/`except Exception as e`/
  `raise`.
- **Built-ins**: `print`, `len`, `range(const)`, `int`, `str`,
  `enumerate`, `zip`, `map`, `filter`, `reversed`, `any`, `all`, `sum`,
  `min`, `max`, `abs`.

## Not supported

Where a missing feature would otherwise produce silently wrong output,
the compiler raises `NotImplementedError` at compile time. These
currently include `sorted()`, typed `except T:` clauses, multiple
`except` clauses, and slice step (`[::2]`).

Other Python constructs are not parsed or translated and will fail
loudly: identity operators (`is`, `is not`), default arguments,
`*args`/`**kwargs`, `nonlocal`/`global`, classes, comprehensions,
`with` statements, generators (`yield`), decorators, and f-strings.

## Runtime caveats

- Dictionaries are association lists with O(n) lookup.
- Lists are cons cells; mutating operations produce new lists.
- Floats require a VM built with `VM_ENABLE_REALS`; in the default
  build they may be truncated to integers.
- `range()` accepts only a constant argument.
- Embedded builds cap per-expression bytecode at 255 bytes; keep
  individual statements modest.
