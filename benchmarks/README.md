# VeloxVM Benchmarks

Performance measurement and stress testing programs for VeloxVM.

## Overview

These benchmarks are designed to measure VM performance characteristics and validate implementation correctness under stress conditions. Unlike unit tests (which verify functional correctness), benchmarks measure timing, throughput, and resource usage.

## Available Benchmarks

### Garbage Collection

**memory-stress.scm** - Memory allocation stress test
- Tests GC with varying structure sizes (100, 500, 1000 objects)
- Creates 10 lists of 200 objects (2000 total) simultaneously
- Includes 10 cycles of allocation/deallocation to trigger GC
- Validates automatic GC handles memory pressure correctly
- Confirms iterative marking algorithm prevents stack overflow

**deep-structures.scm** - Deep structure GC validation
- Creates deeply nested lists (depth 60, 100, 200) using tail recursion
- Verifies structures survive automatic garbage collection
- Previous recursive GC implementation would overflow C stack at ~40 depth
- Validates iterative marking algorithm handles arbitrary depths
- Creates additional structures to trigger GC and verify integrity

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

Compile and run benchmarks like any VeloxVM program:

```bash
# Compile benchmark
./compile-racket.sh benchmarks/closures.scm

# Run benchmark
bin/vm benchmarks/closures.vm

# Or measure end-to-end wall clock
time bin/vm benchmarks/closures.vm
```

## Interpreting Results

### Memory Stress Test

- **Pass criteria**: All allocations complete without errors
- **Automatic GC**: VM triggers garbage collection as needed
- **Expected output**: "MEMORY STRESS TEST COMPLETE"

### Deep Structure Tests

- **Pass criteria**: Lists at depths 60, 100, 200 constructed and verified
- **GC validation**: Structures survive automatic garbage collection
- **Significance**: Validates iterative marking handles deep structures without C stack overflow
- **Expected output**: "All deep structures survived GC" PASS

### Closures

- **Pass criteria**: First counter's final value matches `(+ n-increments 1)`
  (one extra read at the end via `read-counter`)
- **Significance**: Validates that closures created in a tight loop hold
  independent state, that mutable captures persist across calls, and
  that the box-rewrite path is functioning under load

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
