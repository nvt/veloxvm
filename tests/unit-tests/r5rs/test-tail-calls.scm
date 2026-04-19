;; VeloxVM Unit Tests - R5RS §3.5 proper tail recursion
;;
;; Each test recurses TCO-DEPTH times in a particular tail position. The
;; VeloxVM POSIX port has VM_CONTEXT_STACK_SIZE = 64 frames, so a value
;; well above that (50000) blows the stack decisively if tail calls in
;; that position are not eliminated.
;;
;; These tests cover the tail positions that currently work in VeloxVM.
;; Two R5RS tail positions are not exercised here because a separate bug
;; in closure capture (inner lambdas fail to look up outer lexical
;; variables, see scheme-r5rs-compliance.md) makes them hang or error
;; before TCO is even reached:
;;   - `case` clauses (rewritten via `let` + `cond` with a temp var)
;;   - `letrec` body when the body references an outer lexical variable

(include "../unit-test-framework.scm")

(define TCO-DEPTH 50000)

(test-suite "Tail Recursion - Primitive Forms")

;; `if` alternate (the baseline).
(define (loop-if n)
  (if (= n 0) 'ok (loop-if (- n 1))))
(assert-equal 'ok (loop-if TCO-DEPTH) "if alternate: 50k tail calls")

;; `if` consequent.
(define (loop-if-then n)
  (if (> n 0) (loop-if-then (- n 1)) 'ok))
(assert-equal 'ok (loop-if-then TCO-DEPTH) "if consequent: 50k tail calls")

(test-suite "Tail Recursion - Derived Conditionals (R5RS 4.2.1)")

(define (loop-cond n)
  (cond ((= n 0) 'ok)
        (else (loop-cond (- n 1)))))
(assert-equal 'ok (loop-cond TCO-DEPTH) "cond else clause: 50k tail calls")

(define (loop-and n)
  (and (>= n 0)
       (if (= n 0) 'ok (loop-and (- n 1)))))
(assert-equal 'ok (loop-and TCO-DEPTH) "and last expression: 50k tail calls")

(define (loop-or n)
  (or (and (= n 0) 'ok)
      (loop-or (- n 1))))
(assert-equal 'ok (loop-or TCO-DEPTH) "or last expression: 50k tail calls")

(define (loop-when n)
  (if (= n 0) 'ok (when #t (loop-when (- n 1)))))
(assert-equal 'ok (loop-when TCO-DEPTH) "when body: 50k tail calls")

(test-suite "Tail Recursion - Binding Constructs (R5RS 4.2.2)")

(define (loop-let n)
  (if (= n 0)
      'ok
      (let ((m (- n 1)))
        (loop-let m))))
(assert-equal 'ok (loop-let TCO-DEPTH) "let body: 50k tail calls")

(define (loop-let* n)
  (if (= n 0)
      'ok
      (let* ((m (- n 1)))
        (loop-let* m))))
(assert-equal 'ok (loop-let* TCO-DEPTH) "let* body: 50k tail calls")

;; Named let is the canonical iteration form.
(define (loop-named n)
  (let go ((k n))
    (if (= k 0) 'ok (go (- k 1)))))
(assert-equal 'ok (loop-named TCO-DEPTH) "named let: 50k tail calls")

(test-suite "Tail Recursion - Sequencing (R5RS 4.2.3)")

(define (loop-begin n)
  (begin
    (if (= n 0) 'ok (loop-begin (- n 1)))))
(assert-equal 'ok (loop-begin TCO-DEPTH) "begin last expression: 50k tail calls")

(test-suite "Tail Recursion - Indirect Calls")

;; Mutual recursion exercises cross-procedure tail calls.
(define (ping n) (if (= n 0) 'ok (pong (- n 1))))
(define (pong n) (if (= n 0) 'ok (ping (- n 1))))
(assert-equal 'ok (ping TCO-DEPTH) "mutual recursion: 50k tail calls")

;; `apply` in tail position (R5RS 6.4 requires apply to make a tail call).
(define (loop-apply n)
  (if (= n 0)
      'ok
      (apply loop-apply (list (- n 1)))))
(assert-equal 'ok (loop-apply TCO-DEPTH) "apply in tail position: 50k tail calls")

(test-summary)
