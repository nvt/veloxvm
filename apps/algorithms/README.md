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

## Bounded-buffer producer/consumer

| File | Language | Style |
|------|----------|-------|
| `producer-consumer.scm` | Scheme | Mutex + two CVs, SRFI-18 wait/signal |

Headline demo for the SRFI-18 condition-variable surface: one
producer pushes integers 1..N into a fixed-capacity FIFO; two
consumers pop and accumulate into a shared sum protected by a
mutex. Two condition variables coordinate the queue — `not-full`
parks the producer when the buffer is at capacity, `not-empty`
parks consumers when the queue is empty. The producer
`condition-variable-broadcast!`s `not-empty` after setting a
shutdown flag so both consumers can wake from an empty queue and
exit.

The demo verifies that `(mutex-unlock! m cv)` is genuinely atomic
with respect to the predicate retest: a missed wakeup would
produce a sum smaller than the expected arithmetic-series total.
The `thread-yield!` between consumed items hands the rest of the
scheduling slice to the other consumer; without it the first
consumer to grab the mutex tends to drain the queue alone.
