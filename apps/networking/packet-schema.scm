;; packet-schema.scm — a symbolic layer over construct-packet /
;; deconstruct-packet. Schemas are lists of field specs:
;;
;;   (NAME TYPE [ARG])
;;
;; Field types:
;;   bit              1-bit unsigned
;;   bits N           N-bit unsigned (1..32)
;;   u8               unsigned byte
;;   u16, u24, u32    unsigned big-endian
;;   u16/le, u32/le   unsigned little-endian
;;   s8, s16, s32     signed two's-complement, big-endian
;;   s16/le, s32/le   signed two's-complement, little-endian
;;   bytes N          N raw bytes (read/written as a Scheme vector of bytes)
;;
;; Sub-byte fields (bit, bits) are packed MSB-first within a byte, matching
;; RFC bit-numbering. Multi-byte fields are big-endian unless suffixed /le.
;;
;; Entry points:
;;   (schema-construct SCHEMA BINDINGS-ALIST)  -> byte buffer
;;   (schema-deconstruct SCHEMA BYTE-BUFFER)   -> bindings alist
;;   (schema-bit-width SCHEMA)                 -> total width in bits

(define (pkt-error msg . irritants)
  (raise (cons 'packet-schema-error (cons msg irritants))))

;; --- Type table ----------------------------------------------------------

(define (pkt-field-bits spec)
  (let ((type (cadr spec)))
    (cond
      ((eq? type 'bit) 1)
      ((eq? type 'bits) (caddr spec))
      ((memq type '(u8 s8)) 8)
      ((memq type '(u16 u16/le s16 s16/le)) 16)
      ((memq type '(u24 u24/le)) 24)
      ((memq type '(u32 u32/le s32 s32/le)) 32)
      ((eq? type 'bytes) (* 8 (caddr spec)))
      (else (pkt-error "unknown field type" type)))))

;; --- Bit/byte helpers ----------------------------------------------------

(define (pkt-byte-swap value byte-count)
  (let loop ((v value) (r 0) (n byte-count))
    (if (zero? n)
        r
        (loop (bit-shift v -8)
              (bit-or (bit-shift r 8) (bit-and v 255))
              (- n 1)))))

(define (pkt-twos-comp value width)
  (if (negative? value)
      (+ value (bit-shift 1 width))
      value))

(define (pkt-sign-extend value width)
  (if (>= value (bit-shift 1 (- width 1)))
      (- value (bit-shift 1 width))
      value))

(define (pkt-int->bytes value n)
  (let ((v (make-vector n 0)))
    (let loop ((i (- n 1)) (val value))
      (if (negative? i)
          v
          (begin (vector-set! v i (bit-and val 255))
                 (loop (- i 1) (bit-shift val -8)))))))

;; --- Per-field encode / decode ------------------------------------------

(define (pkt-encode value type spec)
  (cond
    ((memq type '(bit bits u8 u16 u24 u32)) value)
    ((eq? type 'u16/le) (pkt-byte-swap value 2))
    ((eq? type 'u32/le) (pkt-byte-swap value 4))
    ((eq? type 's8)     (pkt-twos-comp value 8))
    ((eq? type 's16)    (pkt-twos-comp value 16))
    ((eq? type 's32)    (pkt-twos-comp value 32))
    ((eq? type 's16/le) (pkt-byte-swap (pkt-twos-comp value 16) 2))
    ((eq? type 's32/le) (pkt-byte-swap (pkt-twos-comp value 32) 4))
    ((eq? type 'bytes)  value)
    (else (pkt-error "unknown field type" type))))

(define (pkt-decode raw type spec)
  (cond
    ((memq type '(bit bits u8 u16 u24 u32)) raw)
    ((eq? type 'u16/le) (pkt-byte-swap raw 2))
    ((eq? type 'u32/le) (pkt-byte-swap raw 4))
    ((eq? type 's8)     (pkt-sign-extend raw 8))
    ((eq? type 's16)    (pkt-sign-extend raw 16))
    ((eq? type 's32)    (pkt-sign-extend raw 32))
    ((eq? type 's16/le) (pkt-sign-extend (pkt-byte-swap raw 2) 16))
    ((eq? type 's32/le) (pkt-sign-extend (pkt-byte-swap raw 4) 32))
    ((eq? type 'bytes)
     ;; deconstruct-packet emits a vector for fields > 32 bits and an integer
     ;; otherwise; normalise to a vector either way.
     (if (vector? raw) raw (pkt-int->bytes raw (caddr spec))))
    (else (pkt-error "unknown field type" type))))

;; --- Public API ----------------------------------------------------------

(define (schema-bit-width schema)
  (let loop ((rest schema) (acc 0))
    (if (null? rest) acc
        (loop (cdr rest) (+ acc (pkt-field-bits (car rest)))))))

(define (pkt-schema-widths schema)
  (list->vector (map pkt-field-bits schema)))

(define (schema-construct schema bindings)
  (let ((widths (pkt-schema-widths schema))
        (values
         (list->vector
          (map (lambda (spec)
                 (let ((name (car spec))
                       (type (cadr spec)))
                   (let ((slot (assq name bindings)))
                     (if slot
                         (pkt-encode (cdr slot) type spec)
                         (pkt-error "missing field in bindings" name)))))
               schema))))
    (construct-packet widths values)))

(define (schema-deconstruct schema buffer)
  (let* ((widths (pkt-schema-widths schema))
         (raw    (deconstruct-packet widths buffer)))
    (let loop ((i 0) (rest schema) (acc '()))
      (if (null? rest)
          (reverse acc)
          (let* ((spec (car rest))
                 (name (car spec))
                 (type (cadr spec))
                 (val  (pkt-decode (vector-ref raw i) type spec)))
            (loop (+ i 1) (cdr rest) (cons (cons name val) acc)))))))
