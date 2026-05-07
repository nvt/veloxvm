;; Regression: vm_get_object in core/vm-bytecode.c set obj->type to
;; STRING/RATIONAL before calling vm_alloc_at to fill obj->value.
;; If that allocation triggered a GC, mark_object walked the obj
;; with the new type and a stale value pointer from a prior use of
;; the slot, segfaulting inside the STRING / RATIONAL case of
;; mark_object. The fix brackets the alloc with vm_gc_disable /
;; vm_gc_enable.
;;
;; Repro requires sustained string-cons activity to land in the GC
;; window with a slot that previously held a string -- the original
;; deep-structures benchmark hit it around 1500-2000 iterations of a
;; (cons "literal" acc) loop. We run 2500 here for a comfortable
;; margin.

(include "../unit-test-framework.scm")

(test-suite "GC of string-bearing cons cells under sustained pressure")

(define (build-string-list depth)
  (define (iter d acc)
    (if (= d 0) acc (iter (- d 1) (cons "extra" acc))))
  (iter depth '()))

(define (pressure k)
  (if (= k 0)
      'done
      (begin (build-string-list 100) (pressure (- k 1)))))

;; Before the fix this segfaults around iter 1500-5000 with a SIGSEGV
;; inside mark_object. The benchmark just needs to complete; the
;; assertion is implicit (test program survives).
(pressure 2500)
(assert-equal 'done (pressure 100) "string-cons pressure does not crash GC")

(test-summary)
