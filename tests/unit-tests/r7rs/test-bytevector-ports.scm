;;; VeloxVM Unit Tests - R7RS bytevector ports
;;; R7RS §6.13.1, §6.13.3:
;;;   (open-input-bytevector bv)        -> input port
;;;   (open-output-bytevector)          -> output port
;;;   (get-output-bytevector port)      -> bytevector

(include "../unit-test-framework.scm")

(test-suite "R7RS bytevector ports")

;; --- Output port: accumulate bytes via write-char.

(define op (open-output-bytevector))
(assert-equal #t (port? op)        "open-output-bytevector returns a port")
(assert-equal #t (output-port? op) "output-bv port is an output port")
(assert-equal #f (input-port? op)  "output-bv port is not an input port")

(write-char #\A op)
(write-char #\B op)
(write-char #\C op)
(define bv (get-output-bytevector op))
(assert-equal #t (buffer? bv)      "snapshot is a byte buffer")
(assert-equal 3  (vector-length bv) "snapshot length")

;; Subsequent writes append; second snapshot is longer.
(write-char #\D op)
(define bv2 (get-output-bytevector op))
(assert-equal 4 (vector-length bv2) "second snapshot includes new write")

;; --- Input port: read bytes from a bytevector.

(define ip (open-input-bytevector bv))
(assert-equal #t (input-port? ip) "input-bv port is an input port")
(assert-equal 65 (char->integer (read-char ip)) "byte 0 = 'A'")
(assert-equal 66 (char->integer (read-char ip)) "byte 1 = 'B'")
(assert-equal 67 (char->integer (read-char ip)) "byte 2 = 'C'")
(assert-equal #t (eof-object? (read-char ip))   "EOF after exhaustion")
(assert-equal #t (eof-object? (read-char ip))   "EOF is sticky")

;; Empty input bytevector immediately EOFs.
(define empty-op (open-output-bytevector))
(define empty-bv (get-output-bytevector empty-op))
(assert-equal 0 (vector-length empty-bv) "fresh output port snapshot is empty")
(define empty-ip (open-input-bytevector empty-bv))
(assert-equal #t (eof-object? (read-char empty-ip)) "empty source -> EOF")

;; --- Larger payload (no 32 KB cap; vectors use int32_t length).
(define op-big (open-output-bytevector))
(let loop ((i 0))
  (when (< i 1000)
    (write-char (integer->char (modulo i 256)) op-big)
    (loop (+ i 1))))
(define big (get-output-bytevector op-big))
(assert-equal 1000 (vector-length big)
              "1000-byte output exceeds the string-port cap")

(test-summary)
