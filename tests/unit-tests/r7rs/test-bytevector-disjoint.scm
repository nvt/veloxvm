;;; VeloxVM Unit Tests - R7RS bytevector / vector disjointness
;;; R7RS §3.2 requires the eleven base type predicates to be pairwise
;;; disjoint -- in particular, (vector? bv) must be #f for any
;;; bytevector bv. VeloxVM backs bytevectors with buffer-flagged
;;; vm_vector_t storage shared with make-buffer, so the disjointness
;;; lives in the predicate definitions rather than in distinct type
;;; tags: vector? clears on the BUFFER flag, bytevector? matches it.

(include "../unit-test-framework.scm")

(test-suite "Bytevector / vector disjointness")

(define v (vector 1 2 3))
(define bv (make-bytevector 4))
(define buf (make-buffer 4))

;; vector? sees only element-vectors, never byte-storage
(assert-equal #t (vector? v)   "vector? matches a regular vector")
(assert-equal #f (vector? bv)  "vector? rejects a bytevector (R7RS disjointness)")
(assert-equal #f (vector? buf) "vector? rejects a raw buffer")
(assert-equal #f (vector? "abc") "vector? rejects strings as before")
(assert-equal #f (vector? '(1 2 3)) "vector? rejects lists as before")

;; bytevector? is the dual: matches byte-storage only
(assert-equal #f (bytevector? v)  "bytevector? rejects a regular vector")
(assert-equal #t (bytevector? bv) "bytevector? matches a make-bytevector result")
(assert-equal #t (bytevector? buf) "bytevector? matches a make-buffer result")

;; The two predicates must never both be #t on the same value.
(define (vector-or-bytevector? x)
  (cond
    ((vector? x) 'vector)
    ((bytevector? x) 'bytevector)
    (else 'neither)))

(assert-equal 'vector     (vector-or-bytevector? v)   "v classified as vector only")
(assert-equal 'bytevector (vector-or-bytevector? bv)  "bv classified as bytevector only")
(assert-equal 'bytevector (vector-or-bytevector? buf) "buf classified as bytevector only")
(assert-equal 'neither    (vector-or-bytevector? 42)  "non-collection neither")

;; Empty cases keep the disjointness
(assert-equal #t (vector? (make-vector 0))    "empty regular vector matches vector?")
(assert-equal #f (bytevector? (make-vector 0)) "empty regular vector rejected by bytevector?")
(assert-equal #f (vector? (make-bytevector 0))    "empty bytevector rejected by vector?")
(assert-equal #t (bytevector? (make-bytevector 0)) "empty bytevector matches bytevector?")

;; get-output-bytevector yields a bytevector, not a vector
(define op (open-output-bytevector))
(write-u8 65 op)
(write-u8 66 op)
(define snap (get-output-bytevector op))
(assert-equal #f (vector? snap)    "get-output-bytevector result is not vector?")
(assert-equal #t (bytevector? snap) "get-output-bytevector result is bytevector?")

;; read-bytevector likewise
(define rs (open-input-bytevector (bytevector 10 20 30)))
(define rb (read-bytevector 3 rs))
(assert-equal #f (vector? rb)    "read-bytevector result is not vector?")
(assert-equal #t (bytevector? rb) "read-bytevector result is bytevector?")

(test-summary)
