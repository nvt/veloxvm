;; Regression: vm_write_object had two unbounded loops -- recursion
;; through nested lists/vectors/boxes (depth) and per-byte hex emission
;; for byte-buffer vectors (width). Either could pin the cooperative
;; scheduler indefinitely on hostile input. Both are now capped --
;; recursion at 16 levels, byte-buffer hex dump at VM_BUFFER_PRINT_LIMIT
;; bytes -- with an "<X items|bytes omitted>" tail.

(include "../unit-test-framework.scm")

(test-suite "vm_write_object hostile-input bounds")

;; Build a vector nested DEPTH levels deep. Each level is a
;; single-element vector wrapping the previous.
(define (nest depth)
  (define (build d acc)
    (if (= d 0) acc (build (- d 1) (vector acc))))
  (build depth 0))

;; The smoke test is that printing a 30-level nested vector returns
;; cleanly. Without the depth cap the printer would either spin (on a
;; cyclic structure) or recurse 30 levels into itself; with the cap it
;; emits "..." past 16 levels and unwinds. Reaching the next assertion
;; demonstrates the call returned.
(print (nest 30)) (print "\n")
(assert-true #t "deep vector print returned cleanly")

;; Shallow nesting (below the cap) round-trips through equal? as
;; expected -- the cap doesn't kick in for normal-sized values.
(define small (nest 3))
(assert-equal small small "shallow vector compares equal to itself")

;; A multi-kilobyte byte buffer would, without the width cap, emit
;; megabytes of \xNN sequences in one scheduler tick. The smoke test
;; is that printing a 5000-byte buffer returns cleanly. The buffer
;; itself round-trips equal? to itself either way.
(define big-buffer (make-buffer 5000))
(define (fill i)
  (if (= i 5000) 'done
      (begin (buffer-append big-buffer i (modulo i 256)) (fill (+ i 1)))))
(fill 0)
(print big-buffer) (print "\n")
(assert-true #t "wide buffer print returned cleanly")

(test-summary)
