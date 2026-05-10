;; VeloxVM Unit Tests - Non-destructive list primitives
;;
;; Regression coverage for a pre-existing bug in append, remove, and
;; vm_list_copy where the operations mutated their input arguments in
;; place. R5RS specifies that append must allocate a new list (sharing
;; structure only with its last argument), and SRFI-1 / common
;; convention require remove to leave its input unchanged. Sharing of
;; list literals across use sites in compiled code makes the bug
;; observable even when callers don't hold their own alias.

(include "../unit-test-framework.scm")

(test-suite "Non-destructive list primitives")

;; ============================================================================
;; append leaves both inputs untouched
;; ============================================================================

(define a-orig '(1 2 3))
(define b-orig '(4 5 6))
(define ab (append a-orig b-orig))

(assert-equal '(1 2 3 4 5 6) ab "append produces the concatenation")
(assert-equal '(1 2 3) a-orig  "first argument to append is unmodified")
(assert-equal '(4 5 6) b-orig  "second argument to append is unmodified")
(assert-false (eq? a-orig ab)  "append result is a new list (not eq? to first arg)")

;; Repeated calls on the same literal each return a fresh result with
;; the literal preserved.
(define repeat-a '(1 2))
(define r1 (append repeat-a '(3)))
(define r2 (append repeat-a '(4)))
(assert-equal '(1 2 3) r1 "first append result correct")
(assert-equal '(1 2 4) r2 "second append result correct")
(assert-equal '(1 2)   repeat-a "literal stays intact across multiple appends")

;; ============================================================================
;; remove leaves the input untouched
;; ============================================================================

(define rm-orig '(1 2 3 2 4))
(define rm-result (remove 2 rm-orig))

(assert-equal '(1 3 4)     rm-result "remove drops every match")
(assert-equal '(1 2 3 2 4) rm-orig   "remove leaves the input list unchanged")
(assert-false (eq? rm-orig rm-result) "remove result is a new list")

;; Removing a non-existent element returns a list equal to the input
;; but still freshly allocated.
(define rm-noop (remove 99 rm-orig))
(assert-equal '(1 2 3 2 4) rm-noop "remove of absent element preserves elements")
(assert-equal '(1 2 3 2 4) rm-orig "input still unchanged after no-op remove")

;; ============================================================================
;; cons doesn't share structure that callers can mutate
;; (cons internally calls vm_list_copy on its tail; a shallow copy
;;  would let push/insert through the cons result corrupt the tail)
;; ============================================================================

(define cons-tail '(2 3))
(define cons-result (cons 1 cons-tail))
(assert-equal '(1 2 3) cons-result "cons constructs the expected list")
(assert-equal '(2 3)   cons-tail   "cons leaves its tail argument intact")

;; ============================================================================
;; Multiple operations chained on the same literal
;; ============================================================================

(define base '(10 20 30))
(define after-append (append base '(40)))
(define after-remove (remove 20 base))
(define after-cons   (cons 0 base))

(assert-equal '(10 20 30 40) after-append "chained append")
(assert-equal '(10 30)       after-remove "chained remove")
(assert-equal '(0 10 20 30)  after-cons   "chained cons")
(assert-equal '(10 20 30)    base         "literal preserved through three operations")

(test-summary)
