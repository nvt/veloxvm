;;; VeloxVM Unit Tests - slice
;;; Tests for the `slice` primitive across all four supported sequence
;;; types: string, list, regular vector, and buffer-flagged vector
;;; (R7RS bytevector storage).
;;;
;;; History: slicing a buffer-flagged vector previously segfaulted —
;;; the result was created with VM_VECTOR_FLAG_REGULAR and the copy
;;; loop read input->elements, which is NULL for a buffer. Fixed in
;;; core/expr-list.c by branching on the input's flag.

(include "../unit-test-framework.scm")

(test-suite "slice across sequence types")

;; ---------------------------------------------------------------
;; Strings
;; ---------------------------------------------------------------

(assert-equal "ell"   (slice "hello" 1 4) "string: middle")
(assert-equal ""      (slice "hello" 2 2) "string: empty (start=end)")
(assert-equal "hello" (slice "hello" 0 5) "string: full")
(assert-equal "lo"    (slice "hello" -2 5) "string: negative start")
(assert-equal "hel"   (slice "hello" 0 -2) "string: negative end")
(assert-equal "hello" (slice "hello" 0 100) "string: end clamped")
(assert-equal ""      (slice "hello" 4 2) "string: start>end becomes empty")

;; ---------------------------------------------------------------
;; Lists
;; ---------------------------------------------------------------

(assert-equal '(2 3 4)     (slice '(1 2 3 4 5) 1 4) "list: middle")
(assert-equal '()          (slice '(1 2 3 4 5) 2 2) "list: empty")
(assert-equal '(1 2 3 4 5) (slice '(1 2 3 4 5) 0 5) "list: full")
(assert-equal '(4 5)       (slice '(1 2 3 4 5) -2 5) "list: negative start")
(assert-equal '(1 2 3)     (slice '(1 2 3 4 5) 0 -2) "list: negative end")
(assert-equal '(1 2 3 4 5) (slice '(1 2 3 4 5) 0 100) "list: end clamped")

;; ---------------------------------------------------------------
;; Regular vectors
;; ---------------------------------------------------------------

(define v (vector 10 20 30 40 50))
(define vs (slice v 1 4))
(assert-equal #t (vector? vs) "vector slice: still a vector")
(assert-equal 3 (vector-length vs) "vector slice: length")
(assert-equal 20 (vector-ref vs 0) "vector slice: element 0")
(assert-equal 30 (vector-ref vs 1) "vector slice: element 1")
(assert-equal 40 (vector-ref vs 2) "vector slice: element 2")

;; ---------------------------------------------------------------
;; Buffer-flagged vectors (R7RS bytevectors)
;; ---------------------------------------------------------------

(define b (make-buffer 5))
(vector-set! b 0 65)
(vector-set! b 1 66)
(vector-set! b 2 67)
(vector-set! b 3 68)
(vector-set! b 4 69)

;; Middle slice keeps the buffer flag (the regression: result used to
;; be a regular vector, when it didn't outright segfault).
(define bs (slice b 1 4))
(assert-equal #t (buffer? bs) "buffer slice: still a buffer")
(assert-equal #t (vector? bs) "buffer slice: still a vector (vector? is true for buffers)")
(assert-equal 3 (vector-length bs) "buffer slice: length")
(assert-equal 66 (char->integer (vector-ref bs 0)) "buffer slice: byte 0")
(assert-equal 67 (char->integer (vector-ref bs 1)) "buffer slice: byte 1")
(assert-equal 68 (char->integer (vector-ref bs 2)) "buffer slice: byte 2")

;; Source unchanged.
(assert-equal 65 (char->integer (vector-ref b 0)) "buffer slice: source byte 0 unchanged")
(assert-equal 69 (char->integer (vector-ref b 4)) "buffer slice: source byte 4 unchanged")

;; Empty buffer slice keeps the flag and zero length.
(define be (slice b 2 2))
(assert-equal #t (buffer? be) "buffer slice: empty stays a buffer")
(assert-equal 0 (vector-length be) "buffer slice: empty length")

;; Full buffer slice.
(define bf (slice b 0 5))
(assert-equal #t (buffer? bf) "buffer slice: full stays a buffer")
(assert-equal 5 (vector-length bf) "buffer slice: full length")
(assert-equal 65 (char->integer (vector-ref bf 0)) "buffer slice: full byte 0")
(assert-equal 69 (char->integer (vector-ref bf 4)) "buffer slice: full byte 4")

;; Negative-start slice on a buffer.
(define bn (slice b -2 5))
(assert-equal #t (buffer? bn) "buffer slice: negative start stays a buffer")
(assert-equal 2 (vector-length bn) "buffer slice: negative start length")
(assert-equal 68 (char->integer (vector-ref bn 0)) "buffer slice: negative start byte 0")
(assert-equal 69 (char->integer (vector-ref bn 1)) "buffer slice: negative start byte 1")

;; Out-of-bounds end clamps without erroring.
(define bc (slice b 0 100))
(assert-equal #t (buffer? bc) "buffer slice: clamp stays a buffer")
(assert-equal 5 (vector-length bc) "buffer slice: clamp length")

;; start > end yields an empty buffer.
(define bx (slice b 4 2))
(assert-equal #t (buffer? bx) "buffer slice: start>end stays a buffer")
(assert-equal 0 (vector-length bx) "buffer slice: start>end length 0")

;; The result is an independent copy: mutating it leaves the source
;; alone and vice versa.
(define bcopy (slice b 0 5))
(vector-set! bcopy 0 200)
(assert-equal 200 (char->integer (vector-ref bcopy 0)) "buffer slice: copy mutated")
(assert-equal 65  (char->integer (vector-ref b 0))     "buffer slice: source untouched after copy mutate")
(vector-set! b 1 201)
(assert-equal 66 (char->integer (vector-ref bcopy 1)) "buffer slice: copy untouched after source mutate")

(test-summary)
