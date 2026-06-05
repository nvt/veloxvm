;;; VeloxVM Unit Tests - Tail calls across nested loops (R5RS 3.5 regression)
;;;
;;; Regression for the tail-call folding bug fixed in core/vm-sched.c
;;; (see doc/scheme-tco-nested-loop-bug.md). A bottom-up merge sort packs
;;; the patterns that the old call-site (expr_id) matching failed to fold:
;;;
;;;   - nested named-let loops: pass -> run -> the merge `loop`;
;;;   - a loop (the merge loop) re-entered from MULTIPLE recursive call
;;;     sites (the cond branches), so consecutive iterations have different
;;;     call-site ids;
;;;   - reached through a wrapper (`list-sort`) that calls `vector-sort!` in
;;;     non-tail position;
;;;   - with a forwarded comparator closure (`less?`) invoked inside the
;;;     innermost loop.
;;;
;;; Before the fix this overflowed the context stack (folding never fired,
;;; the stack grew per iteration). The input below is built programmatically
;;; and is large enough that, without proper tail-call folding, the nested
;;; loops exceed VM_CONTEXT_STACK_SIZE. A pass proves the folding works.

(include "../unit-test-framework.scm")

;; Stably merge src[lo,mid) and src[mid,hi) into dst[lo,hi). The `loop`
;; recurses from three different cond branches.
(define (sort:merge-run less? src dst lo mid hi)
  (let loop ((i lo) (j mid) (k lo))
    (cond ((and (< i mid) (< j hi))
           (if (less? (vector-ref src j) (vector-ref src i))
               (begin (vector-set! dst k (vector-ref src j))
                      (loop i (+ j 1) (+ k 1)))
               (begin (vector-set! dst k (vector-ref src i))
                      (loop (+ i 1) j (+ k 1)))))
          ((< i mid)
           (vector-set! dst k (vector-ref src i))
           (loop (+ i 1) j (+ k 1)))
          ((< j hi)
           (vector-set! dst k (vector-ref src j))
           (loop i (+ j 1) (+ k 1)))
          (else #t))))

(define (vector-sort! less? vec)
  (let ((n (vector-length vec)))
    (when (> n 1)
      (let ((scratch (make-vector n 0)))
        (let pass ((width 1) (src vec) (dst scratch))
          (if (>= width n)
              (when (not (eq? src vec))
                (let copy ((i 0))
                  (when (< i n)
                    (vector-set! vec i (vector-ref src i))
                    (copy (+ i 1)))))
              (begin
                (let run ((i 0))
                  (when (< i n)
                    (let* ((mr (+ i width)) (hr (+ i (* 2 width)))
                           (mid (if (< mr n) mr n)) (hi (if (< hr n) hr n)))
                      (sort:merge-run less? src dst i mid hi))
                    (run (+ i (* 2 width)))))
                (pass (* width 2) dst src))))))))

(define (list-sort less? lst)
  (let ((v (list->vector lst)))
    (vector-sort! less? v)
    (vector->list v)))

;; Build (n n-1 ... 1), reversed: the worst case for the merge passes.
(define (countdown n)
  (let loop ((i 0) (acc '()))
    (if (>= i n) acc (loop (+ i 1) (cons (+ i 1) acc)))))

(define (sorted? lst)
  (cond ((null? lst) #t)
        ((null? (cdr lst)) #t)
        ((< (car (cdr lst)) (car lst)) #f)
        (else (sorted? (cdr lst)))))

(test-suite "R5RS 3.5: tail calls across nested loops (merge sort)")

;; 64 reversed elements: deep enough that un-folded nested loops overflow.
(define result (list-sort < (countdown 64)))
(assert-true  (sorted? result)             "merge sort fully ordered (no overflow)")
(assert-equal 1  (car result)              "min element first")
(assert-equal 64 (car (reverse result))    "max element last")
(assert-equal 64 (length result)           "no elements lost")

(test-summary)
