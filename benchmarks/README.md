# VeloxVM Benchmarks

Performance measurement and stress testing programs for VeloxVM.

## Overview

These benchmarks are designed to measure VM performance characteristics and validate implementation correctness under stress conditions. Unlike unit tests (which verify functional correctness), benchmarks measure timing, throughput, and resource usage.

## Available Benchmarks

### Garbage Collection

**memory-stress.scm** - Multi-root GC mark
- Builds 10 separate lists of 200 cells each, all live as top-level
  bindings simultaneously (2000 cells across 10 distinct roots)
- Forces a GC cycle and verifies every root list survives intact
- Unique coverage: GC root-set traversal across multiple top-level
  bindings (tree-walk and alloc-churn each have only one live root)

**deep-structures.scm** - Deep linear-spine GC mark
- Builds a single 200-cell linear list (no branching), forces GC,
  verifies the spine is intact
- Earlier recursive mark implementation overflowed the C stack
  around depth 40; depth 200 proves iterative mark works
- Complementary to tree-walk.scm, which exercises a branched shape

### Compute

**fib-recursive.scm** - Naive recursive Fibonacci, repeated
- (fib 12) computed 1000 times, ~465k function calls total
- No tail-call optimisation applies; pure call/return dispatch probe
- Sized for VM_CONTEXT_STACK_SIZE = 64 (POSIX): n = 12 inside an outer
  tail-recursive loop leaves comfortable stack headroom
- Cross-frontend candidate: same source structure compiles under all
  three frontends, so timings give a frontend-quality comparison

**prime-sieve.scm** - Sieve of Eratosthenes up to 10000
- Stresses vector-ref / vector-set! at scale, integer arithmetic in
  tight loops, and nested tail recursion (mark-multiples inside
  mark-composites)
- Vector ops go through a separate code path from cons cells

### Memory

**tree-walk.scm** - Binary tree build + sum, repeated
- Builds a depth-9 tree (1022 cons cells) using proper-list nodes,
  walks it summing leaves, repeats 50 times (~51k allocations total)
- Different from deep-structures.scm: branched live structure during
  walk, not a single long spine; allocation-then-discard cycles
- Note: nodes are encoded as (list left right) rather than
  (cons left right) to avoid a VM bug in cdr on improper lists
  of length >= 2

**alloc-churn.scm** - Sustained map/filter churn over a fixed list
- 1000 iterations of (map + filter) over a 200-cell constant list,
  ~300k cons allocations against a constant 200-cell live root
- Final (reduce + data) verifies the top-level list survived GC

### Closures

**closures.scm** - Closure creation + invocation throughput
- Builds 100 independent counter closures, then increments each 1000
  times (100k total calls)
- Exercises closure dispatch, captures binding, and the box rewrite
  path that gives mutable captures their shared heap storage
- Every call goes through bind_function -> capture binding ->
  box-ref / box-set! on the counter's state, so this is a regression
  probe for the closure runtime and box primitive throughput
- Sanity checks the first counter's final value at the end

## Running Benchmarks

Run all benchmarks at once with the bundled runner:

```bash
./benchmarks/run-benchmarks.sh
```

It compiles any out-of-date sources, executes each `.vm`, and prints
PASS/FAIL plus elapsed time per benchmark with a final summary.

To compile and run a single benchmark manually (artefacts land in
`benchmarks/bin/<name>.vm`):

```bash
./compile.sh benchmarks/closures.scm
bin/vm benchmarks/bin/closures.vm

# Or measure end-to-end wall clock
time bin/vm benchmarks/bin/closures.vm
```

## Interpreting Results

### Memory Stress (multi-root)

- **Pass criteria**: all 10 root lists report `(car list-X) = 200`
  after a forced GC cycle
- **Significance**: validates the GC's root-set traversal across
  multiple top-level bindings

### Deep Structures (linear spine)

- **Pass criteria**: a depth-200 list reports `list-depth = 200`
  after a forced GC cycle
- **Significance**: validates iterative mark on long single-spine
  structures (the earlier recursive mark would overflow at ~40)

### Closures

- **Pass criteria**: First counter's final value matches `(+ n-increments 1)`
  (one extra read at the end via `read-counter`)
- **Significance**: Validates that closures created in a tight loop hold
  independent state, that mutable captures persist across calls, and
  that the box-rewrite path is functioning under load

### Compute and Memory benchmarks

The four benchmarks added in the compute and memory categories share
a common output format: each prints a result line, an `elapsed: <N>
ms` line, and terminates with `Status: PASS` or `Status: FAIL`.

- **Fibonacci recursive**: pass when `(fib 12) = 144`. Elapsed time
  is dominated by ~465k function calls; expect tens to hundreds of
  ms on POSIX.
- **Prime sieve**: pass when 1229 primes are found up to 10000.
  Should run in milliseconds; a multi-second result indicates a
  vector-op regression.
- **Tree walk**: pass when the depth-9 leaf sum is 512. Builds and
  walks ~51k cons allocations across 50 iterations.
- **Allocation churn**: pass when the post-churn `(reduce + data)`
  returns 20100. A drift from this value indicates the GC corrupted
  the top-level data binding.

## Adding New Benchmarks

When creating new benchmarks:

1. **Focus on measurement**: Use timing primitives and report quantitative results
2. **Self-documenting**: Include clear output explaining what's being measured
3. **Repeatable**: Results should be consistent across runs
4. **Representative**: Test realistic workloads, not just edge cases
5. **Minimal dependencies**: Keep benchmarks simple and focused

## See Also

- `tests/unit-tests/` - Functional correctness tests
- `doc/instruction-set.md` - VM instruction documentation
- `core/vm-memory.c` - GC implementation
