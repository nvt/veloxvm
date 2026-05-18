;;; VeloxVM Unit Tests - R7RS §6.13 binary I/O on ports
;;;
;;; Exercises read-u8, peek-u8, write-u8, read-bytevector,
;;; read-bytevector!, and write-bytevector against in-memory
;;; bytevector ports so the tests need no external state.

(include "../unit-test-framework.scm")

(test-suite "R7RS binary I/O")

;; The Racket reader rejects R7RS #u8(...) literals; bytevector
;; constructors from the prelude provide an equivalent.
(define (bv . xs) (apply bytevector xs))

;; --- read-u8 / peek-u8 -------------------------------------------------
(define ip (open-input-bytevector (bv 10 20 30 40)))
(assert-equal 10 (peek-u8 ip)  "peek-u8 returns first byte without consuming")
(assert-equal 10 (peek-u8 ip)  "peek-u8 is idempotent at same position")
(assert-equal 10 (read-u8 ip)  "read-u8 consumes peeked byte")
(assert-equal 20 (read-u8 ip)  "read-u8 advances after peek")
(assert-equal 30 (peek-u8 ip)  "peek-u8 sees next byte after consumption")
(assert-equal 30 (read-u8 ip)  "read-u8 consumes after peek")
(assert-equal 40 (read-u8 ip)  "read-u8 final byte")
(assert-equal #t (eof-object? (read-u8 ip)) "read-u8 returns eof past end")
(assert-equal #t (eof-object? (peek-u8 ip)) "peek-u8 returns eof past end")

;; --- read-u8 yields exact integers in [0,255], not characters ----------
(define ip2 (open-input-bytevector (bv 0 127 255)))
(assert-equal 0   (read-u8 ip2)               "low byte")
(assert-equal 127 (read-u8 ip2)               "mid byte")
(assert-equal 255 (read-u8 ip2)               "high byte")
(assert-equal #t  (integer? (read-u8 (open-input-bytevector (bv 7))))
              "read-u8 returns integer type")

;; --- write-u8 ----------------------------------------------------------
(define op (open-output-bytevector))
(write-u8 65 op)
(write-u8 0 op)
(write-u8 255 op)
(assert-equal (bv 65 0 255) (get-output-bytevector op)
              "write-u8 appends bytes in order")

;; --- read-bytevector ---------------------------------------------------
(define src (open-input-bytevector (bv 1 2 3 4 5)))
(assert-equal (bv 1 2 3) (read-bytevector 3 src)
              "read-bytevector returns first k bytes")
(assert-equal (bv 4 5) (read-bytevector 10 src)
              "read-bytevector returns short tail when fewer than k remain")
(assert-equal #t (eof-object? (read-bytevector 1 src))
              "read-bytevector returns eof when stream is empty")

(define empty-src (open-input-bytevector (bv)))
(assert-equal (bv) (read-bytevector 0 empty-src)
              "read-bytevector with k=0 returns empty bytevector even at EOF")
(assert-equal #t (eof-object? (read-bytevector 1 empty-src))
              "read-bytevector with k>0 at EOF returns eof")

;; --- read-bytevector! --------------------------------------------------
(define dst (make-bytevector 6 0))
(define rsrc (open-input-bytevector (bv 11 22 33 44)))
(assert-equal 4 (read-bytevector! dst rsrc)
              "read-bytevector! returns count when k <= available")
(assert-equal 11 (bytevector-u8-ref dst 0) "dst[0] after read-bytevector!")
(assert-equal 22 (bytevector-u8-ref dst 1) "dst[1] after read-bytevector!")
(assert-equal 33 (bytevector-u8-ref dst 2) "dst[2] after read-bytevector!")
(assert-equal 44 (bytevector-u8-ref dst 3) "dst[3] after read-bytevector!")
(assert-equal 0  (bytevector-u8-ref dst 4) "untouched tail untouched (4)")
(assert-equal 0  (bytevector-u8-ref dst 5) "untouched tail untouched (5)")
(assert-equal #t (eof-object? (read-bytevector! dst rsrc))
              "read-bytevector! returns eof when nothing remains")

;; read-bytevector! with explicit start/end slice
(define dst2 (make-bytevector 6 99))
(define rsrc2 (open-input-bytevector (bv 7 8 9 10)))
(assert-equal 3 (read-bytevector! dst2 rsrc2 2 5)
              "read-bytevector! with start/end fills the slice")
(assert-equal 99 (bytevector-u8-ref dst2 0) "before slice untouched")
(assert-equal 99 (bytevector-u8-ref dst2 1) "before slice untouched")
(assert-equal 7  (bytevector-u8-ref dst2 2) "slice index 0")
(assert-equal 8  (bytevector-u8-ref dst2 3) "slice index 1")
(assert-equal 9  (bytevector-u8-ref dst2 4) "slice index 2")
(assert-equal 99 (bytevector-u8-ref dst2 5) "after slice untouched")

;; start == end is a zero-byte read that doesn't trigger EOF
(define dst3 (make-bytevector 4 0))
(assert-equal 0 (read-bytevector! dst3 (open-input-bytevector (bv)) 2 2)
              "read-bytevector! start=end returns 0 without EOF")

;; --- write-bytevector --------------------------------------------------
(define wop (open-output-bytevector))
(write-bytevector (bv 1 2 3 4) wop)
(write-bytevector (bv 5 6) wop)
(assert-equal (bv 1 2 3 4 5 6) (get-output-bytevector wop)
              "write-bytevector appends bytes in order")

(define wop2 (open-output-bytevector))
(write-bytevector (bv 0 10 20 30 40 50) wop2 1 4)
(assert-equal (bv 10 20 30) (get-output-bytevector wop2)
              "write-bytevector with start/end writes the slice only")

(define wop3 (open-output-bytevector))
(write-bytevector (bv 1 2 3) wop3 1 1)
(assert-equal (bv) (get-output-bytevector wop3)
              "write-bytevector with start=end writes nothing")

(test-summary)
