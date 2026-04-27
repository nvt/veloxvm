#!/usr/bin/env python3
"""Multi-app concurrency tests for VeloxVM.

Each scenario loads two or more compiled .vm programs in a single VM
invocation (bin/vm prog1.vm prog2.vm ...) and asserts properties of
the combined stdout. The VM's cooperative scheduler runs all programs
in one process, so these tests exercise:

  * scheduler interleaving via slice preemption (non-yielding apps)
  * scheduler interleaving via thread-sleep! yields
  * scheduler interleaving across language frontends (Scheme + Python)
  * scheduler interleaving under heavy CPU load
  * coexistence of more than two programs in one VM run
  * symbol-table isolation (each vm_program_t has its own bindings)
  * fault isolation (an erroring program does not stop the others)

Slice-preemption tests rely on bin/vm's cooperative scheduler running
each thread for at most VM_EXEC_INSTR_PER_INVOCATION (1000 on posix)
bytecode instructions before moving to the next runnable thread; the
busy_*/compute_* fixtures are sized to span several slice boundaries.
"""

from __future__ import annotations

import os
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
APPS_DIR = Path(__file__).resolve().parent / "apps"
BIN_DIR = APPS_DIR / "bin"
VM = REPO / "bin" / "vm"
RACKET_COMPILER = REPO / "languages" / "scheme-racket" / "main.rkt"
PYVELOX_COMPILE = REPO / "languages" / "python" / "pyvelox-compile"

SCHEME_SOURCES = [
    "busy_a", "busy_b", "tag_c",
    "compute_a", "compute_b",
    "globals_x", "globals_y",
    "yield_a", "yield_b",
    "crash", "safe",
]
PYTHON_SOURCES = ["busy_py"]

# Cap each VM run so a hang in one scenario does not block the suite.
RUN_TIMEOUT_S = 15


def needs_compile(src: Path, dst: Path) -> bool:
    if not dst.exists():
        return True
    if src.stat().st_mtime > dst.stat().st_mtime:
        return True
    if VM.exists() and VM.stat().st_mtime > dst.stat().st_mtime:
        return True
    return False


def compile_scheme(name: str) -> Path:
    src = APPS_DIR / f"{name}.scm"
    dst = BIN_DIR / f"{name}.vm"
    if not needs_compile(src, dst):
        return dst
    BIN_DIR.mkdir(exist_ok=True)
    subprocess.run(
        ["racket", str(RACKET_COMPILER), "-o", str(dst), str(src)],
        check=True, cwd=REPO, stdout=subprocess.DEVNULL,
    )
    return dst


def compile_python(name: str) -> Path:
    src = APPS_DIR / f"{name}.py"
    dst = BIN_DIR / f"{name}.vm"
    if not needs_compile(src, dst):
        return dst
    BIN_DIR.mkdir(exist_ok=True)
    subprocess.run(
        [str(PYVELOX_COMPILE), "-o", str(dst), str(src)],
        check=True, cwd=REPO, stdout=subprocess.DEVNULL,
    )
    return dst


def compile_all() -> None:
    for n in SCHEME_SOURCES:
        compile_scheme(n)
    for n in PYTHON_SOURCES:
        compile_python(n)


def run_vm(*program_names: str) -> str:
    """Run bin/vm with the given compiled programs and return stdout."""
    args = [str(VM)] + [str(BIN_DIR / f"{n}.vm") for n in program_names]
    proc = subprocess.run(
        args, cwd=REPO, capture_output=True, text=True, timeout=RUN_TIMEOUT_S,
    )
    return proc.stdout


def assert_mutual_interleaving(out: str, a_first: str, a_done: str,
                               b_first: str, b_done: str
                               ) -> tuple[bool, str]:
    """Each app's first line must appear before the other's done marker."""
    for marker in (a_first, a_done, b_first, b_done):
        if marker not in out:
            return False, f"missing marker {marker!r}\nstdout:\n{out}"
    a_first_pos = out.index(a_first)
    a_done_pos = out.index(a_done)
    b_first_pos = out.index(b_first)
    b_done_pos = out.index(b_done)
    if not (a_first_pos < b_done_pos and b_first_pos < a_done_pos):
        return False, ("no interleaving observed: "
                       f"{a_first}@{a_first_pos} {a_done}@{a_done_pos} "
                       f"{b_first}@{b_first_pos} {b_done}@{b_done_pos}\n"
                       f"stdout:\n{out}")
    return True, "both apps' work was interleaved by the scheduler"


# Test cases: each returns (ok, message).

def test_two_scheme_apps_preempted() -> tuple[bool, str]:
    """Two non-yielding Scheme programs interleave via slice preemption."""
    out = run_vm("busy_a", "busy_b")
    return assert_mutual_interleaving(
        out, a_first="A0\n", a_done="A-done", b_first="B0\n", b_done="B-done",
    )


def test_three_apps_coexist() -> tuple[bool, str]:
    """More than two programs can be loaded and all complete."""
    out = run_vm("busy_a", "busy_b", "tag_c")
    expected = ["A-done", "B-done", "TAG_C_HELLO"]
    missing = [t for t in expected if t not in out]
    if missing:
        return False, f"missing tokens: {missing}\nstdout:\n{out}"
    return True, "all three programs reached completion"


def test_mixed_languages_preempted() -> tuple[bool, str]:
    """A Scheme program and a Python program interleave under preemption."""
    out = run_vm("busy_a", "busy_py")
    return assert_mutual_interleaving(
        out, a_first="A0\n", a_done="A-done",
        b_first="PY0\n", b_done="PY-done",
    )


def test_independent_globals() -> tuple[bool, str]:
    """Each program has its own symbol table; same name -> different values."""
    out = run_vm("globals_x", "globals_y")
    if "X-counter=100" not in out:
        return False, f"X-counter=100 missing\nstdout:\n{out}"
    if "Y-counter=200" not in out:
        return False, f"Y-counter=200 missing\nstdout:\n{out}"
    return True, "both globals kept their per-program values"


def test_interleaved_yielding() -> tuple[bool, str]:
    """thread-sleep! yields produce fine-grained interleaving."""
    out = run_vm("yield_a", "yield_b")
    return assert_mutual_interleaving(
        out, a_first="A0\n", a_done="A-done", b_first="B0\n", b_done="B-done",
    )


def test_fault_isolation() -> tuple[bool, str]:
    """One program errors; the other still completes successfully."""
    out = run_vm("crash", "safe")
    if "CRASH-START" not in out:
        return False, f"crashing program never started\nstdout:\n{out}"
    if "CRASH-NEVER" in out:
        return False, f"code after error was reached\nstdout:\n{out}"
    if "SAFE-DONE" not in out:
        return False, f"safe program did not finish\nstdout:\n{out}"
    return True, "safe program finished despite peer error"


def test_compute_heavy_interleaving() -> tuple[bool, str]:
    """Two CPU-bound apps without yields interleave via slice preemption."""
    out = run_vm("compute_a", "compute_b")
    return assert_mutual_interleaving(
        out, a_first="CA0=", a_done="CA-done",
        b_first="CB0=", b_done="CB-done",
    )


TESTS = [
    ("two_scheme_apps_preempted",   test_two_scheme_apps_preempted),
    ("three_apps_coexist",          test_three_apps_coexist),
    ("mixed_languages_preempted",   test_mixed_languages_preempted),
    ("independent_globals",         test_independent_globals),
    ("interleaved_yielding",        test_interleaved_yielding),
    ("fault_isolation",             test_fault_isolation),
    ("compute_heavy_interleaving",  test_compute_heavy_interleaving),
]


def colored(s: str, code: str) -> str:
    if not sys.stdout.isatty():
        return s
    return f"\033[{code}m{s}\033[0m"


def main() -> int:
    if not VM.exists():
        print(f"VM binary not found at {VM} - run 'make' first.", file=sys.stderr)
        return 1
    if not RACKET_COMPILER.exists():
        print(f"Racket compiler not found at {RACKET_COMPILER}.", file=sys.stderr)
        return 1
    if not os.access(PYVELOX_COMPILE, os.X_OK):
        print(f"pyvelox-compile not executable at {PYVELOX_COMPILE}.", file=sys.stderr)
        return 1

    print("Compiling test apps...")
    compile_all()

    passed = 0
    failed = 0
    for name, fn in TESTS:
        try:
            ok, msg = fn()
        except subprocess.TimeoutExpired:
            ok, msg = False, f"timed out after {RUN_TIMEOUT_S}s"
        except Exception as e:
            ok, msg = False, f"exception: {e!r}"
        if ok:
            print(f"  {colored('PASS', '32')} {name}: {msg}")
            passed += 1
        else:
            print(f"  {colored('FAIL', '31')} {name}: {msg}")
            failed += 1

    print()
    print(f"Summary: {passed} passed, {failed} failed")
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
