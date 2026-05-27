# Threading in VeloxVM apps

VeloxVM's threading model is **concurrency, not parallelism**. The VM
itself runs as a single OS thread; the "threads" an app creates are
green threads multiplexed by the VM's own scheduler onto that one OS
thread. They cannot use multiple CPU cores and cannot speed up
CPU-bound work — a parallel mergesort on N threads runs at the same
wall-clock as a single-threaded one (sometimes slightly slower because
of scheduling overhead).

The right mental model is **fibers / green threads / Erlang processes**,
not POSIX threads.

## Where threading pays off

1. **Concurrency for waiting.** A sensor poll, a network read, a
   periodic timer, and a UI tick can all live in one `.vm` program and
   the scheduler interleaves them. Threads parked in `thread-sleep!`,
   `mutex-lock!`, or `mutex-unlock! m cv` do not burn CPU — the VM's
   `poll()` loop wakes them on timer expiry or fd readiness. This is
   the embedded sweet spot: a 32 kB Contiki-NG node cannot afford
   multiple OS threads anyway, but it can afford a handful of green
   threads inside the VM.

2. **Multi-program coexistence.** Each loaded `.vm` runs as its own
   thread(s) under per-program policies (CPU, memory, network, power,
   thread count — see `core/policies/`). The scheduler enforces those
   limits and isolates faults: one app crashing or busy-looping does
   not take the others down. This is what `tests/multi-app/` exercises.

3. **Structuring code that has multiple things to wait on.** Even
   when an app could be written as a single-threaded event loop,
   factoring it into a "radio listener", a "sensor sampler", and a
   "main loop" is often clearer.

## Where threading does not help

- **CPU-bound work.** Splitting a tight loop across N threads makes it
  slower, not faster.
- **Replacing OS threads.** Anything that wants real parallelism
  belongs outside the VM (host-side preprocessing, a tighter
  algorithm, a different target).
- **Blocking syscalls.** The VM does I/O through its non-blocking poll
  loop; an app that pretends to block in C would stall every other
  thread in the VM. Stick to the supplied I/O primitives.

## The primitives

Surface follows [SRFI 18](https://srfi.schemers.org/srfi-18/srfi-18.html)
with the divergences noted below. Full ID table is `doc/primitives.md`.

### Threads
- `(thread-create! thunk)` — spawn and start a new thread running
  `thunk`; returns a thread object. (Combines SRFI 18's `make-thread`
  + `thread-start!`.)
- `(thread? obj)`, `(current-thread)`
- `(thread-join! t [timeout-ms [timeout-val]])` — block until `t`
  finishes; returns its thunk's result. With `timeout-ms` (integer
  ms, or `#f` for no timeout), returns `timeout-val` (default `#f`)
  on expiry. SRFI 18 specifies that an omitted `timeout-val` should
  raise `join-timeout-exception`; we return `#f` until that exception
  type lands.
- `(thread-terminate! t)` — kill `t`; returns `#t` if it was alive.
- `(thread-yield!)` — give up the rest of this slice.
- `(thread-sleep! ms)` — suspend for `ms` milliseconds. **Argument is
  a relative ms integer, not a SRFI-18 absolute time object.**
  `(thread-sleep! 0)` is a yield.
- `(thread-specific t)`, `(thread-specific-set! t v)`
- `(thread-id)`, `(thread-stats [t])` — non-SRFI: integer id and
  per-thread counters.

### Mutexes
- `(make-mutex [name])` — name is optional (defaults to empty string).
- `(mutex? obj)`, `(mutex-name m)`
- `(mutex-specific m)`, `(mutex-specific-set! m v)`
- `(mutex-lock! m [timeout-ms])` — acquire, optionally with a ms
  timeout. Returns `#t` on acquisition, `#f` on timeout.
- `(mutex-unlock! m [cv [timeout-ms]])` — release, optionally
  atomically park on `cv`. With cv: returns `#t` on signal, `#f` on
  timeout. Without cv: returns unspecified.
- `(mutex-state m)` — **only the locked/owned case returns a useful
  value today**; the other three SRFI states are not implemented.

### Condition variables
- `(make-condition-variable [name])`
- `(condition-variable? obj)`, `(condition-variable-name cv)`
- `(condition-variable-specific cv)`, `(condition-variable-specific-set! cv v)`
- `(condition-variable-signal! cv)` — wake one parked waiter.
- `(condition-variable-broadcast! cv)` — wake all.

### What is missing relative to SRFI 18

- `make-thread` / `thread-start!` (combined into `thread-create!`)
- `thread-name`
- Absolute-time `thread-sleep!` and the `time` object family
  (`current-time`, `time?`, `time->seconds`, `seconds->time`)
- Named exception types (`join-timeout-exception?`,
  `abandoned-mutex-exception?`, `terminated-thread-exception?`,
  `uncaught-exception?`, `uncaught-exception-reason`). Without
  `join-timeout-exception?`, `thread-join!` on timeout falls back
  to returning `#f` when no `timeout-val` is supplied.
- `current-exception-handler` / `with-exception-handler` (we have
  R6RS-style `guard` and `raise` instead)
- Three of four `mutex-state` return symbols; only the locked/owned
  case returns a useful value today (the thread object).

## The canonical wait/signal idiom

```scheme
(define m  (make-mutex "queue"))
(define cv (make-condition-variable "not-empty"))

;; consumer
(mutex-lock! m)
(let loop ()
  (if (queue-empty?)
      (begin (mutex-unlock! m cv)   ; atomic release-and-wait
             (mutex-lock! m)
             (loop))
      (process (dequeue!))))
(mutex-unlock! m)

;; producer
(mutex-lock! m)
(enqueue! item)
(condition-variable-signal! cv)
(mutex-unlock! m)
```

`(mutex-unlock! m cv)` is atomic with respect to the predicate retest
above — that atomicity is what prevents the classic missed-wakeup race
between releasing the mutex and parking on the cv. Loop around the
wait; never assume the predicate holds just because you were signaled.

## Example apps

- `apps/algorithms/parallel-sort.scm` — fork-join mergesort,
  demonstrates `thread-create!` + `thread-join!` returning a value.
  Self-validates against a sequential reference.
- `apps/algorithms/producer-consumer.scm` — bounded-buffer
  producer/consumer over `not-full` / `not-empty` CVs, exercises
  `condition-variable-signal!` and `-broadcast!` end to end.
- `apps/embedded/thread.iscm`, `apps/embedded/mutex.iscm` — older
  demos using just `thread-create!` + `thread-sleep!` + simple
  mutexes.

## Implementation notes for the curious

- The scheduler is in `core/vm-sched.c`. It iterates threads by index
  each tick and runs each runnable one for a per-program
  instruction budget (`perf_attr.exec_instr_per_invocation`).
- Threads are represented by `vm_thread_t` (`include/vm.h`). Each
  carries its own expression-frame stack, so the C call stack is not
  used for VM-level continuations.
- A parked thread carries a `wait_cancel` callback so timeouts can
  pull it off its mutex/cv wait list and surface `#f` to the caller.
- Timer wakeups go through `vm_native_sleep` in the port layer
  (POSIX uses a `setitimer`/`SIGALRM` queue; Contiki-NG uses
  `ctimer`).
- The interaction with garbage collection: mutexes and CVs are
  external objects with `mark` callbacks; their wait lists are part
  of GC-traced state, so a mutex stashed in a global binding stays
  alive across collections even while parked threads reference it.
