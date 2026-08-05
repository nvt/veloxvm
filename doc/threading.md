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
  `thunk`; returns a thread object. Convenience for the common
  `make-thread` + `thread-start!` two-step.
- `(make-thread thunk [name])` — create a thread in a
  not-yet-runnable state; `thread-start!` flips it to runnable.
- `(thread-start! t)` — start a `make-thread`-created thread;
  returns the thread.
- `(thread-name t)` — return the name supplied to `make-thread`,
  or `#f` if none.
- `(thread? obj)`, `(current-thread)`
- `(thread-join! t [timeout-ms [timeout-val]])` — block until `t`
  finishes; returns its thunk's result. With `timeout-ms` (integer
  ms, a SRFI-18 time object, or `#f` for no timeout): returns
  `timeout-val` on expiry, or raises `join-timeout-exception` if
  `timeout-val` is omitted. Joining a thread killed via
  `thread-terminate!` raises `terminated-thread-exception`; joining
  a thread that died with an uncaught exception raises
  `uncaught-exception` wrapping the original.
- `(thread-terminate! t)` — kill `t`; returns `#t` if it was alive.
- `(thread-yield!)` — give up the rest of this slice.
- `(thread-sleep! timeout)` — suspend until the timeout deadline.
  Accepts an integer (ms-relative, VeloxVM convention) or a SRFI-18
  time object (absolute deadline; sleep until that point in time).
  `(thread-sleep! 0)` and `(thread-sleep! (current-time))` both
  yield without blocking.
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

### Time objects (SRFI 18)

- `(current-time)` — samples the host clock; returns an absolute
  time object.
- `(time? obj)`
- `(time->seconds t)` — returns an integer when the time is a whole
  number of seconds, otherwise a real (a real-less embedded target
  truncates to integer seconds).
- `(seconds->time x)` — accepts integer, rational, or real seconds
  since epoch.

Time objects can be used wherever the surface accepts a timeout
(`thread-sleep!`, `thread-join!`, `mutex-lock!`, `mutex-unlock! m cv
timeout`). They are interpreted as absolute deadlines and converted
to ms-relative internally. Integer timeouts continue to mean
ms-relative (the VeloxVM convention, divergent from SRFI 18 which
reads bare numbers as absolute seconds since epoch).

### Typed exceptions (SRFI 18)

The runtime raises typed conditions for four failure modes; user
code dispatches inside `guard`:

- `(join-timeout-exception? obj)` — raised by `thread-join!` when
  its timeout expires and no `timeout-val` was supplied.
- `(abandoned-mutex-exception? obj)` — raised by `mutex-lock!` when
  the mutex was abandoned by a terminated owner.
- `(terminated-thread-exception? obj)` — raised by `thread-join!`
  on a joinee that was killed via `thread-terminate!`.
- `(uncaught-exception? obj)` — raised by `thread-join!` on a
  joinee that died with an unhandled exception. The wrapped reason
  is recovered via `(uncaught-exception-reason exc)`.

```scheme
(guard (exc ((join-timeout-exception? exc)         'timed-out)
            ((terminated-thread-exception? exc)    'killed)
            ((uncaught-exception? exc)
             (list 'died (uncaught-exception-reason exc)))
            (else (raise exc)))
  (thread-join! worker 1000))
```

### What is missing relative to SRFI 18

- `current-exception-handler` / `with-exception-handler` — we have
  R6RS-style `guard` and `raise` instead, plus the four SRFI 18
  typed exceptions above. A proper SRFI-18 handler chain would
  require restructuring the existing raise/guard machinery and is
  redundant for the use cases `guard` already covers.
- Three of four `mutex-state` return symbols; only the locked/owned
  case returns a useful value today (the thread object). Wiring
  the other three requires exposing core-scope symbols to
  programs, a piece of broader R7RS work that is out of scope.
- Integer-vs-time-object timeout semantics: VeloxVM keeps integers
  as ms-relative for back-compat; SRFI 18 reads them as absolute
  seconds since epoch. Use time objects for portable absolute
  deadlines.

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
- `apps/algorithms/threading-demo.py` — Python example using named,
  deferred-start threads, mutex-protected shared state, thread-specific
  values, yielding, and join results.
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
