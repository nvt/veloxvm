# Algorithm examples

Classic algorithms implemented in more than one language so that you can
compare how each front-end expresses the same program.

## Sieve of Eratosthenes

| File | Language | Style |
|------|----------|-------|
| `sieve.scm`  | Scheme  | Functional, vector-based, tail-recursive helpers |
| `sieve2.cyl` | Cyclus  | Imperative, C-like loops, direct array manipulation |
| `sieve.py`   | Python  | List-based, `for`/`while` loops |

All three compute the same primes up to a common bound. Build any of them
with `./compile.sh algorithms/<file>` from the repository root and run
with `./run.sh algorithms/sieve`.

## Parallel mergesort

| File | Language | Style |
|------|----------|-------|
| `parallel-sort.scm` | Scheme | Fork-join via `thread-create!` + `thread-join!` |

Splits a deterministic pseudo-random integer list into N chunks, spawns
one worker thread per chunk to mergesort it, then joins each worker to
retrieve its sorted sublist and merges the K results on the main
thread. The same input is also sorted sequentially; the two outputs
are compared element-by-element so the run is self-validating.

VeloxVM's scheduler is preemptive but single-core, so the parallel
path doesn't beat the sequential one on wall-clock — it pays thread
setup and join overhead with no real parallelism. The point of the
demo is to exercise `thread-create!` and `thread-join!`'s
return-value plumbing with a workload that fails loudly if the
synchronization is wrong.
