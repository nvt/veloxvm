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

When something is rejected at compile time you get a
`PyveloxCompileError` with `path:line:col` location and the offending
source line; the CLI prints the same and exits non-zero.

## Language status

| Construct | Status | Notes |
|-----------|--------|-------|
| `int`, `bool`, `str`, `None` | Yes | `None` is encoded as `False`; they're indistinguishable at runtime. |
| `float` | Partial | Truncated to int unless the VM is built with `VM_ENABLE_REALS`. |
| `list` | Yes | Cons cells; mutating methods rebind the variable rather than updating in place. |
| `dict` | Yes | Association lists, O(n) lookup. |
| `tuple` | Yes | Stored as a list; the immutable distinction isn't enforced. |
| `set`, `frozenset` | No | No set type. |
| `if`/`elif`/`else`, ternary `x if cond else y` | Yes | |
| `for`, `while` | Yes | |
| `break`, `continue` | Yes | Implemented via VM exception sentinels. |
| `def`, `lambda`, `return` | Yes | Closures and free-variable capture supported. |
| Default arguments `def f(x=10)` | Partial | Defaults must be literal constants (int, bool, str, None). Resolved by call-site padding, so passing `f` as a value (e.g. `map(f, xs)`) doesn't carry the defaults. Lambdas with defaults are refused. |
| `*args` / `**kwargs`, keyword-only, positional-only | No | Refused at compile time. |
| `nonlocal`, `global` | Yes | Recognised by the scope analyser. |
| `try` / `except` / `raise` | Partial | Single bare `except:` or `except Exception:`; typed handlers and multiple `except` clauses refused. `raise X(msg)` keeps the type, drops the message. |
| `with` (context managers) | No | |
| `class` | No | |
| Comparison ops (`<`, `<=`, `>`, `>=`) | Yes | Numeric only. |
| `==`, `!=` | Yes | Type-aware deep equality (`equalp`). `True == 1` is `False` because the VM keeps booleans and ints as distinct types. |
| `is`, `is not` | No | Refused at compile time. |
| Arithmetic / bitwise / boolean ops | Yes | `/` produces a rational, not a float. |
| Augmented assignment (`+=`, `-=`, ...) | Yes | Simple variable targets only. |
| Multiple targets (`a = b = 5`) | No | |
| Tuple unpacking (`a, b = pair`, `for x, y in ...`) | Yes | Simple names only. |
| Slicing `seq[start:stop]` | Yes | Lists, strings, and VM vectors. Negative indices supported. |
| Slicing with step `[::2]` | No | |
| Plain negative indexing `lst[-1]` | Buggy | Compiles but crashes at runtime. Use `lst[len(lst)-1]` for now. |
| Subscript assignment `lst[0] = v` | Buggy | Emits dict-style code; produces wrong list contents. Rebuild the list instead. |
| Subscript assignment `d['k'] = v` | Yes | Simple variable targets only. |
| List comprehension `[expr for x in xs]` | Partial | Single generator plus filters. Multiple generators refused. |
| Dict / set comprehensions | No | |
| Generators (`yield`) | No | |
| f-strings `f"x={x}"` | Yes | Lowers to `string-append` over `str()`. Format specs (`:.2f`) and conversions (`!r`, `!s`, `!a`) are refused. |
| Decorators | No | |
| `import lib` | Yes | Loads a port-specific VM library by canonical name. |
| `import x as y`, `from x import y` | No | |

Status values: `Yes` supported, `Partial` supported with documented
gaps, `No` refused at compile time, `Buggy` currently silently
miscompiles (will be tightened to a refusal).

## Built-ins

| Function | Status | Notes |
|----------|--------|-------|
| `print` | Yes | Adds a trailing newline; `sep`/`end` kwargs ignored. |
| `len` | Yes | |
| `range` | Yes | 1–3 arguments, variable bounds. Small literal `range(N)` constant-folds; everything else uses a `_pyvelox_range` helper emitted into the program prologue. Step `0` would loop forever (Python raises). |
| `int`, `str`, `abs`, `min`, `max`, `sum` | Yes | `int` and `str` insert a small runtime type-dispatch for non-literal arguments. |
| `enumerate`, `zip`, `reversed`, `list` | Yes | |
| `map`, `filter`, `reduce`, `any`, `all` | Yes | Eager — return lists, not iterators. |
| `sorted` | No | Refused; sort manually until a VM-level sort is added. |

Method calls (only on simple variable receivers; expressions like
`[1, 2].append(3)` are refused):

| Receiver | Methods |
|----------|---------|
| `str` | `upper`, `lower`, `split`, `join`, `startswith`, `endswith`, `strip`, `replace` |
| `list` | `append`, `extend`, `pop` (no-arg), `remove`, `reverse`, `count`, `index`, `insert` |
| `dict` | `keys`, `values`, `items`, `get` |

## Runtime caveats

- Lists are cons cells; method calls that look mutating actually
  rebind the variable to a new list. Aliases of the old list keep
  seeing the old contents.
- Dictionaries are association lists; lookup is linear. Fine for small
  configurations, not for hot paths.
- The `/` operator produces a rational, not a float. Use `//` for
  integer division.
- Embedded builds cap per-expression bytecode at 255 bytes; keep
  individual statements modest.
- `int(x)`, `str(x)`, and f-string interpolation insert a runtime
  type-dispatch when `x` isn't a literal. Cheap, but worth pulling
  out of hot loops.

## See also

- `tests/python-tests/` — runnable test suite (compiler unit tests
  plus end-to-end programs).
- `languages/python/pyvelox/translator.py` — the AST-to-bytecode
  walker if you need to understand exact emission.
