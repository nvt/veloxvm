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
| `bytes` (`b'...'`, `bytes(...)`) | Yes | Backed by R7RS bytevector storage (buffer-flagged vector). `bytes(b'...')` shares storage instead of copying — observable only via `vector-set!`, which user Python can't reach. `bytes(True)` / `bytes(False)` is refused as ambiguous. |
| `bytearray` | No | The VM has no separate mutable-buffer type; refused at compile time. |
| `if`/`elif`/`else`, ternary `x if cond else y` | Yes | |
| `for`, `while` | Yes | |
| `break`, `continue` | Yes | Implemented via VM exception sentinels. |
| `def`, `lambda`, `return` | Yes | Closures and free-variable capture supported. |
| Default arguments `def f(x=10)` | Partial | Defaults must be literal constants (int, bool, str, None). Resolved by call-site padding, so passing `f` as a value (e.g. `map(f, xs)`) doesn't carry the defaults. Lambdas with defaults are refused. |
| `*args` (receive and forward) | Yes | `def f(*args)` and `lambda *args:` lower to `bind_function_rest`; trailing actuals arrive as a list bound to `args`. `f(*xs)` and `f(prefix, *xs)` at call sites lower to `(apply f arg-list)`. Combining `*args` with default arguments is refused, as are: multiple `*` arguments at one call site, positionals after `*`, and `*` arguments to built-ins or method calls. |
| `**kwargs`, keyword-only, positional-only | No | Refused at compile time. |
| `nonlocal`, `global` | Yes | Recognised by the scope analyser. |
| `try` / `except` / `raise` | Yes | Multiple `except` clauses with type filters: `except SomeClass as e:` matches if `_pyvelox_isinstance(e, SomeClass)`. `except:` and `except Exception:` are catch-all aliases that absorb both the pyinstance and the legacy `#(py-exception ...)` shape. The class named in a typed clause must be defined earlier in the module; tuple filters (`except (A, B):`) aren't yet supported, and a catch-all clause that shadows later typed clauses is refused so unreachable code doesn't sneak in. `raise X(args...)` lowers as instance construction when `X` is a defined class (or `Exception` itself, which is auto-injected); otherwise as a tagged 3-vector. Handlers read `e.type`, `e.args`, `str(e)`, `f"{e}"` the same way regardless of which shape was raised. `raise e` on a bound name re-raises the caught object unchanged. |
| `with` (context managers) | No | |
| `class` | Partial | `class Foo: def __init__/methods` and `class Bar(Foo): ...` lower to tagged class vectors. Instance construction `Foo(args)` runs `__init__`; classes without `__init__` (in their own body or any ancestor) still construct. `self.x = v` / `self.x` / `obj.method(args)` work. `super().method(args)` walks from the enclosing class's parent. `isinstance(obj, Cls)` walks the class chain. Refused: multiple inheritance, forward-reference base classes, `@classmethod` / `@staticmethod` / `@property` / decorators, class-level attributes, nested classes, dunder operator overloading (`__add__` etc.), `__getattr__` / `__setattr__` / descriptors, metaclasses, bare `super()` standalone, `super(Class, self)` 2-arg form, `isinstance` with a tuple second arg. Method names that collide with `_METHOD_HANDLERS` (`get`, `append`, `upper`, etc.) are shadowed by the builtin handler. |
| Comparison ops (`<`, `<=`, `>`, `>=`) | Yes | Numeric only. |
| `==`, `!=` | Yes | Type-aware deep equality (`equalp`). `True == 1` is `False` because the VM keeps booleans and ints as distinct types. |
| `is`, `is not` | No | Refused at compile time. |
| Arithmetic / bitwise / boolean ops | Yes | `/` produces a rational, not a float. |
| Augmented assignment (`+=`, `-=`, ...) | Yes | Simple variable targets only. |
| Multiple targets (`a = b = 5`) | No | |
| Tuple unpacking (`a, b = pair`, `for x, y in ...`) | Yes | Simple names only. |
| Slicing `seq[start:stop]` | Yes | Lists, strings, regular vectors, and bytes. Negative indices supported. Bytes slicing returns a fresh bytes object. |
| Slicing with step `[::2]` | No | |
| Plain negative indexing `lst[-1]` | Buggy | Compiles but crashes at runtime. Use `lst[len(lst)-1]` for now. |
| Subscript assignment `lst[0] = v` | Buggy | Emits dict-style code; produces wrong list contents. Rebuild the list instead. |
| Subscript assignment `d['k'] = v` | Yes | Simple variable targets only. |
| List comprehension `[expr for x in xs]` | Partial | Single generator plus filters. Multiple generators refused. |
| Dict / set comprehensions | No | |
| Generators (`yield`) | No | |
| f-strings `f"x={x}"` | Yes | Lowers to `string-append` over `str()`. Format specs (`:.2f`) and conversions (`!r`, `!s`, `!a`) are refused. |
| Decorators | Partial | Bare `@dataclass` on a class (see Custom exception classes / dataclass section). All other class- and function-level decorators are still refused. |
| `import lib` | Yes | Loads a port-specific VM library by canonical name. `import math` is a compiler-level no-op so `math.isqrt(...)` works without needing the VM to define a math library. |
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
| `bytes` | Yes | `bytes()`, `bytes(N)` (N zero-filled), `bytes([b1, b2, …])`, `bytes(b'...')`. Variable arg routes through a `numberp`/`bufferp` runtime dispatch. |
| `bytearray` | No | Refused at compile time. |
| `isinstance` | Yes | Single class only -- `isinstance(x, (A, B))` with a tuple is refused. Walks the class chain via `eqp` against the target. |
| `enumerate`, `zip`, `reversed`, `list` | Yes | |
| `map`, `filter`, `reduce`, `any`, `all` | Yes | Eager — return lists, not iterators. |
| `sorted` | No | Refused; sort manually until a VM-level sort is added. |

Method calls (only on simple variable receivers; expressions like
`[1, 2].append(3)` are refused):

| Receiver | Methods |
|----------|---------|
| `str` | `upper`, `lower`, `casefold`, `split`, `join`, `startswith`, `endswith`, `strip`, `replace` |
| `list` | `append`, `extend`, `pop` (no-arg), `remove`, `reverse`, `count`, `index`, `insert` |
| `dict` | `keys`, `values`, `items`, `get` |

## Class object representation

Classes are tagged 4-vectors:

    #(pyclass "Name" parent method-alist)

where `parent` is another class object or `#f` for a root class,
and `method-alist` is `((symbol-name . closure) ...)`. Instances
are tagged 3-vectors:

    #(pyinstance class-ref slot-alist)

`slot-alist` starts empty and grows by cons-prepending new pairs as
`self.x = v` writes go through; existing slots are mutated via
`set-cdr!`. Both alists are looked up via `assoc`, so attribute
access is O(N) in the slot/method count -- fine for typical class
sizes.

`_pyvelox_lookup_method` walks the class chain through the parent
slot, so an inherited method is resolved without copying it into
each subclass's method-alist. `super().method(args)` reaches the
helper with the enclosing class's parent slot directly, so the walk
starts above the current class. `isinstance(obj, Cls)` is backed by
a similar walk against the parent chain.

The supporting runtime helpers (`_pyvelox_make_instance`,
`_pyvelox_lookup_method`, `_pyvelox_get_attr`, `_pyvelox_set_attr`,
`_pyvelox_isinstance`, `_pyvelox_class_extends`) are emitted lazily
into the program prologue when the first class definition or
attribute access is compiled.

## @dataclass

`@dataclass` on a class with `name: type` annotated fields
synthesises an `__init__(self, f1, f2, ...)` that stores each
parameter into the matching slot via `_pyvelox_set_attr`.
Methods can coexist with fields. If the user provides their own
`__init__`, synthesis is skipped (matching CPython).

```python
@dataclass
class Point:
    x: int
    y: int

    def magnitude_sq(self):
        return self.x * self.x + self.y * self.y
```

`Point(3, 4)` constructs an instance whose `x` and `y` slots are
populated from the constructor args; `Vec.magnitude_sq()`,
`isinstance(p, Point)`, subclassing `Point`, etc. all behave like
they would on any class.

What's not supported: parameterised forms (`@dataclass(eq=False,
frozen=True, ...)`), per-field defaults (`x: int = 0`), the
`field()` config helper, keyword-only fields. Type annotations
themselves are stored in the AST but otherwise ignored at
runtime, matching CPython.

A class body that has annotations but no `@dataclass` decorator
is rejected (CPython would silently treat them as class-level
attributes); the error message points at the missing decorator.

## Custom exception classes

`class MyError(Exception): pass` (and arbitrarily nested
subclasses) work. The compiler auto-injects an `Exception` base
class into the program prologue the first time a class extends it
or `raise Exception(...)` is used. Exception's synthesised
`__init__(self, *args)` stores `self.args = args` and reads the
actual class name from the instance's class slot into `self.type`,
so subclasses without their own `__init__` get the standard shape
for free, and subclasses that override `__init__` can call
`super().__init__(msg)` to retain it.

`raise X(args)` on a defined class routes through
`_pyvelox_make_instance`; the resulting `pyinstance` is what the
handler sees. Handler-side `e.args`, `e.type`, `str(e)`, and
`f"{e}"` work the same way they do for the legacy
`py-exception` tagged vector -- both shapes share the
`_pyvelox_get_attr` and `_pyvelox_str` runtime helpers.

`isinstance(e, MyError)` walks the class chain, so the usual
"check the kind" pattern works inside an except handler. Typed
exception handlers (`except MyError as e:`) are also supported
directly: each clause's type filter lowers to an
`_pyvelox_isinstance` check, and clauses chain via nested ifs that
fall through to a re-raise when nothing matches. Tuple filters
(`except (A, B):`) aren't yet supported; expand them into separate
clauses or use an `except Exception:` plus `if isinstance(e, ...)`
inside the body.

What's deferred to later steps: multiple inheritance + MRO,
`@classmethod` / `@staticmethod` / `@property`, dunder operator
overloading (`__add__` etc.), `__getattr__`/`__setattr__` and the
descriptor protocol, metaclasses.

## Runtime caveats

- Exceptions carry their constructor args. `raise ValueError("oops")`
  is observable inside the handler as `e.args == ["oops"]`,
  `e.type == "ValueError"`, `str(e) == "oops"`. Multi-arg exceptions
  return `args[0]` from `str(e)` -- a deviation from CPython, which
  formats the args tuple's repr. `print(e)` still prints the bare
  vector representation; wrap it as `print(str(e))` or `print(e.args[0])`
  to get the message. `e.message` doesn't exist (matching CPython 3,
  which removed it).
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

## Pseudo-modules

Some module-style names are recognised by the compiler directly
rather than going through the VM's library loader, so they work
without needing a port library. The matching `import` statement
compiles to a no-op.

| Module | Members |
|--------|---------|
| `math` | `isqrt(n)` -- integer square root via Newton's method. Negative `n` raises `ValueError`. |

A local variable named `math` (or any other pseudo-module name)
shadows the syntactic dispatch -- if you write `math = SomeObj()`,
`math.isqrt(n)` falls back to regular instance-method dispatch on
that local. Other `math.*` names than the ones listed above are
refused at compile time.

## See also

- `tests/python-tests/` — runnable test suite (compiler unit tests
  plus end-to-end programs).
- `languages/python/pyvelox/translator.py` — the AST-to-bytecode
  walker if you need to understand exact emission.
