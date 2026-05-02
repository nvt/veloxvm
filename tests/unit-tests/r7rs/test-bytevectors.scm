;;; VeloxVM Unit Tests - R7RS bytevector procedures
;;; Tests for: bytevector?, bytevector-length, bytevector-u8-ref,
;;;            bytevector-u8-set!, make-bytevector, bytevector.

(include "../unit-test-framework.scm")

;; Inline definitions from
;; languages/scheme-racket/runtime/r7rs-bytevectors.scm
(define bytevector? buffer?)
(define bytevector-length vector-length)
(define (bytevector-u8-ref bv i) (char->integer (vector-ref bv i)))
(define (bytevector-u8-set! bv i byte) (vector-set! bv i byte))

(define (make-bytevector k . rest)
  (let ((bv (make-buffer k)))
    (if (null? rest)
        bv
        (let ((fill (car rest)))
          (let loop ((i 0))
            (if (< i k)
                (begin
                  (vector-set! bv i fill)
                  (loop (+ i 1)))
                bv))))))

(define (bytevector . bytes)
  (let* ((n (length bytes))
         (bv (make-buffer n)))
    (let loop ((i 0) (rest bytes))
      (if (null? rest)
          bv
          (begin
            (vector-set! bv i (car rest))
            (loop (+ i 1) (cdr rest)))))))

(test-suite "R7RS bytevectors")

;; bytevector?: positive and negative cases
(define bv1 (make-bytevector 4))
(assert-equal #t (bytevector? bv1) "bytevector? matches a make-bytevector result")
(assert-equal #f (bytevector? 42) "bytevector? rejects integer")
(assert-equal #f (bytevector? '(1 2 3)) "bytevector? rejects list")
(assert-equal #f (bytevector? "hello") "bytevector? rejects string")
(assert-equal #f (bytevector? (vector 1 2 3))
              "bytevector? rejects element-vector")

;; bytevector-length
(assert-equal 4 (bytevector-length bv1) "length of make-bytevector 4")
(assert-equal 0 (bytevector-length (make-bytevector 0)) "zero-length bytevector")
(assert-equal 16 (bytevector-length (make-bytevector 16)) "length 16")

;; make-bytevector with default fill (zero)
(define bvz (make-bytevector 3))
(assert-equal 0 (bytevector-u8-ref bvz 0) "default fill is 0 (slot 0)")
(assert-equal 0 (bytevector-u8-ref bvz 1) "default fill is 0 (slot 1)")
(assert-equal 0 (bytevector-u8-ref bvz 2) "default fill is 0 (slot 2)")

;; make-bytevector with explicit fill
(define bvf (make-bytevector 4 255))
(assert-equal 255 (bytevector-u8-ref bvf 0) "fill 255 (slot 0)")
(assert-equal 255 (bytevector-u8-ref bvf 1) "fill 255 (slot 1)")
(assert-equal 255 (bytevector-u8-ref bvf 2) "fill 255 (slot 2)")
(assert-equal 255 (bytevector-u8-ref bvf 3) "fill 255 (slot 3)")

;; bytevector-u8-set!: write and read back
(define bvm (make-bytevector 5))
(bytevector-u8-set! bvm 0 10)
(bytevector-u8-set! bvm 1 20)
(bytevector-u8-set! bvm 4 99)
(assert-equal 10 (bytevector-u8-ref bvm 0) "set/ref slot 0")
(assert-equal 20 (bytevector-u8-ref bvm 1) "set/ref slot 1")
(assert-equal 0 (bytevector-u8-ref bvm 2) "untouched slot remains 0")
(assert-equal 99 (bytevector-u8-ref bvm 4) "set/ref last slot")

;; bytevector variadic constructor
(define bvv (bytevector 1 2 3 4 5))
(assert-equal 5 (bytevector-length bvv) "variadic length")
(assert-equal 1 (bytevector-u8-ref bvv 0) "variadic slot 0")
(assert-equal 2 (bytevector-u8-ref bvv 1) "variadic slot 1")
(assert-equal 5 (bytevector-u8-ref bvv 4) "variadic slot 4")

;; Empty bytevector via variadic constructor
(define bve (bytevector))
(assert-equal 0 (bytevector-length bve) "empty variadic bytevector")
(assert-equal #t (bytevector? bve) "empty bytevector is still a bytevector")

;; Byte values 0..255 round-trip cleanly
(define bvr (bytevector 0 1 127 128 255))
(assert-equal 0 (bytevector-u8-ref bvr 0) "byte 0 round-trip")
(assert-equal 1 (bytevector-u8-ref bvr 1) "byte 1 round-trip")
(assert-equal 127 (bytevector-u8-ref bvr 2) "byte 127 round-trip")
(assert-equal 128 (bytevector-u8-ref bvr 3) "byte 128 (sign-bit) round-trip")
(assert-equal 255 (bytevector-u8-ref bvr 4) "byte 255 round-trip")

(test-summary)
