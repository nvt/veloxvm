;;; ============================================================================
;;; VeloxVM R7RS Bytevector Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small bytevector procedures implemented as wrappers over the
;;; existing buffer primitives (make-buffer, buffer?, vector-length,
;;; vector-ref, vector-set! with the BUFFER flag). Bytevectors and
;;; buffer-flagged vectors share the same underlying storage.
;;;
;;; The variadic forms (bytevector b1 b2 ...) and (make-bytevector k
;;; [fill]) use rest arguments and require the bind_function_rest
;;; primitive.
;;;
;;; The `include` directive currently has issues in nested scopes; copy
;;; these definitions directly into your program.
;;; ============================================================================

;; bytevector?: alias for the existing buffer? predicate.
(define bytevector? buffer?)

;; bytevector-length: length of a bytevector. vector-length works on
;; both element-vectors and buffer-vectors transparently.
(define bytevector-length vector-length)

;; bytevector-u8-ref: returns the byte at index i as an exact integer
;; in [0, 255]. The underlying vector-ref returns a character for
;; buffer-vectors, so we round-trip through char->integer.
(define (bytevector-u8-ref bv i)
  (char->integer (vector-ref bv i)))

;; bytevector-u8-set!: stores byte (0..255) at index i. The underlying
;; vector-set! on buffer-vectors accepts either an integer or a
;; character, so this is a direct delegate.
(define (bytevector-u8-set! bv i byte)
  (vector-set! bv i byte))

;; make-bytevector: (make-bytevector k) returns a fresh bytevector of
;; length k filled with zeros. (make-bytevector k fill) fills with the
;; given byte instead. The k=0 case is allowed.
(define (make-bytevector k . rest)
  (let ((bv (make-buffer k)))
    (if (null? rest)
        bv  ; make-buffer initialises to zeros
        (let ((fill (car rest)))
          (let loop ((i 0))
            (if (< i k)
                (begin
                  (vector-set! bv i fill)
                  (loop (+ i 1)))
                bv))))))

;; bytevector: variadic constructor. (bytevector b1 b2 ...) returns a
;; bytevector containing the given bytes in order.
(define (bytevector . bytes)
  (let* ((n (length bytes))
         (bv (make-buffer n)))
    (let loop ((i 0) (rest bytes))
      (if (null? rest)
          bv
          (begin
            (vector-set! bv i (car rest))
            (loop (+ i 1) (cdr rest)))))))

;;; End of r7rs-bytevectors.scm
