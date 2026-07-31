;; Equivalence test for the GC mark phase, which marks from an explicit
;; work list (core/vm-memory.c) and walks cdr spines iteratively rather
;; than recursing through car and cdr. It drives the marker across every
;; shape the work list handles separately -- long spines, wide fanout of
;; compound elements, improper tails, shared referents, cycles, vectors,
;; closures -- holding each structure live across repeated collections,
;; and checks that the data comes through intact.
;;
;; The sizes stay small enough to run at every heap tier, down to the
;; 32 kB / 16 kB configuration. That also means the suite cannot tell an
;; iterative marker from a recursive one on a host with a large native
;; stack: benchmarks/crash-spine.scm is the probe for that, and it needs
;; a spine long enough both to cross VM_GC_MIN_ALLOCATED while live and
;; to outlast the tail-call folding a compiler applies to cdr recursion.
;; Building with a small -DVM_MARK_STACK_SIZE makes this suite exercise
;; the expand-in-place path taken when the work list saturates.

(include "../unit-test-framework.scm")

(test-suite "GC marking of deep, wide, shared, and cyclic structures")

(define spine-depth 300)
(define fanout-width 60)

;; Right-nested spine of scalars: the deep shape, and one work-list
;; slot when the cdr chain is walked iteratively.
(define spine
  (let loop ((i spine-depth) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons i acc)))))

;; Every element is itself a pair, so each one becomes a work item and
;; the peak tracks the width rather than the depth.
(define wide
  (let loop ((i fanout-width) (acc '()))
    (if (= i 0) acc (loop (- i 1) (cons (cons i (- i)) acc)))))

;; Improper tail: the cdr walk has to hand a non-pair tail to the marker
;; rather than following it as another link.
(define improper (cons 1 (cons 2 "tail")))

;; One referent reached many times over. Duplicates are filtered when
;; they are popped, not when they are pushed.
(define shared-cell (cons 'shared 'cell))
(define shared-vec (make-vector 20 shared-cell))

;; Self-referential pair. Termination depends on the mark bit.
(define cyc (cons 'head '()))
(set-cdr! cyc cyc)

;; A closure capturing a live structure, reached through its captures.
(define capturing (lambda () (car spine)))

;; Sustained allocation with all of the above still reachable, so the
;; marker walks them repeatedly rather than once. There is no primitive
;; that forces a collection, so the loop has to allocate past
;; VM_GC_MIN_ALLOCATED (half the heap) to get a sweep at all -- 1800
;; rounds of 200 cells is ~11 MB, comfortably over the POSIX default.
;; Under a smaller heap the sweeps just start earlier.
(define (pressure k)
  (if (= k 0)
      'done
      (begin (let loop ((i 200) (acc '()))
               (if (= i 0) acc (loop (- i 1) (cons i acc))))
             (pressure (- k 1)))))

(assert-equal 'done (pressure 1800) "allocation pressure completes")

(assert-equal 1 (car spine) "deep spine head survives marking")
(assert-equal spine-depth (length spine) "deep spine keeps its length")

(assert-equal 1 (car (car wide)) "wide fanout car survives marking")
(assert-equal -1 (cdr (car wide)) "wide fanout cdr survives marking")
(assert-equal fanout-width (length wide) "wide fanout keeps its length")

(assert-equal 2 (car (cdr improper)) "improper list body survives")
(assert-equal "tail" (cdr (cdr improper)) "improper tail survives")

(assert-equal 'shared (car (vector-ref shared-vec 0)) "shared referent survives")
(assert-equal 'shared (car (vector-ref shared-vec 19)) "last alias survives")

(assert-equal 'head (car cyc) "cyclic pair survives marking")
(assert-equal 'head (car (cdr cyc)) "cycle still closes on itself")

(assert-equal 1 (capturing) "closure capture survives marking")

(test-summary)
