;; packet-schema.scm — a symbolic layer over construct-packet /
;; deconstruct-packet. Schemas are lists of field specs:
;;
;;   (NAME TYPE [ARG])
;;
;; Field types:
;;   bit              1-bit unsigned
;;   bits N           N-bit unsigned (1..8 — sub-byte; use uK/sK for wider)
;;   u8               unsigned byte
;;   u16, u24, u32    unsigned big-endian
;;   u16/le, u32/le   unsigned little-endian
;;   s8, s16, s32     signed two's-complement, big-endian
;;   s16/le, s32/le   signed two's-complement, little-endian
;;   bytes N          N raw bytes (read/written as a Scheme vector of bytes)
;;
;; Sub-byte fields (bit, bits) are packed MSB-first within a byte, matching
;; RFC bit-numbering. Multi-byte fields are big-endian unless suffixed /le
;; and must start at a byte-aligned position; total schema width must be a
;; whole number of bytes.
;;
;; Entry points:
;;   (schema-validate SCHEMA)                  -> #t, or raises on error
;;   (schema-construct SCHEMA BINDINGS-ALIST)  -> byte buffer
;;   (schema-deconstruct SCHEMA BYTE-BUFFER)   -> bindings alist
;;   (schema-bit-width SCHEMA)                 -> total width in bits

(define (pkt-error msg . irritants)
  (raise (cons 'packet-schema-error (cons msg irritants))))

;; --- Type table ----------------------------------------------------------

(define pkt-known-types
  '(bit bits u8 u16 u24 u32 u16/le u32/le
        s8 s16 s32 s16/le s32/le bytes))

;; Every type except bit/bits must start at a byte boundary; bit-packed
;; types are the only ones that may live mid-byte.
(define (pkt-byte-aligned-type? type)
  (not (memq type '(bit bits))))

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

;; --- Schema validation ---------------------------------------------------

(define (pkt-validate-spec spec)
  (unless (and (list? spec) (>= (length spec) 2) (<= (length spec) 3))
    (pkt-error "field spec must be (name type [arg])" spec))
  (let ((name (car spec)) (type (cadr spec)))
    (unless (symbol? name)
      (pkt-error "field name must be a symbol" 'spec spec))
    (unless (memq type pkt-known-types)
      (pkt-error "unknown field type" 'type type 'spec spec))
    (cond
      ((eq? type 'bit)
       (when (= (length spec) 3)
         (pkt-error "'bit takes no argument" 'spec spec)))
      ((eq? type 'bits)
       (unless (= (length spec) 3)
         (pkt-error "'bits requires a width" 'spec spec))
       (let ((n (caddr spec)))
         (unless (and (integer? n) (>= n 1) (<= n 8))
           (pkt-error "'bits width must be 1..8 (use u16/u24/u32 for wider)"
                      'spec spec))))
      ((eq? type 'bytes)
       (unless (= (length spec) 3)
         (pkt-error "'bytes requires a length" 'spec spec))
       (let ((n (caddr spec)))
         (unless (and (integer? n) (>= n 1))
           (pkt-error "'bytes length must be >= 1" 'spec spec))))
      (else
       (when (= (length spec) 3)
         (pkt-error "type does not take an argument" 'spec spec))))))

(define (schema-validate schema)
  (when (or (not (list? schema)) (null? schema))
    (pkt-error "schema must be a non-empty list of field specs"))
  (let loop ((rest schema) (pos 0) (seen '()))
    (if (null? rest)
        (if (zero? (modulo pos 8))
            #t
            (pkt-error "total schema width is not a multiple of 8" 'bits pos))
        (let ((spec (car rest)))
          (pkt-validate-spec spec)
          (let ((name (car spec))
                (type (cadr spec))
                (bits (pkt-field-bits spec)))
            (when (memq name seen)
              (pkt-error "duplicate field name in schema" 'name name))
            (when (and (pkt-byte-aligned-type? type)
                       (not (zero? (modulo pos 8))))
              (pkt-error "byte-aligned field does not start at a byte boundary"
                         'field name 'at-bit pos))
            (loop (cdr rest) (+ pos bits) (cons name seen)))))))

;; --- Value validation ----------------------------------------------------

(define (pkt-check-int value lo hi name)
  (unless (integer? value)
    (pkt-error "expected integer" 'field name 'got value))
  (when (or (< value lo) (> value hi))
    (pkt-error "value out of range"
               'field name 'value value 'range (list lo hi))))

(define (pkt-check-bytes value n name)
  (unless (vector? value)
    (pkt-error "expected byte vector" 'field name 'got value))
  (unless (= (vector-length value) n)
    (pkt-error "wrong vector length"
               'field name 'expected n 'got (vector-length value)))
  (let loop ((i 0))
    (when (< i n)
      (let ((b (vector-ref value i)))
        (unless (and (integer? b) (>= b 0) (<= b 255))
          (pkt-error "byte out of range 0..255"
                     'field name 'index i 'value b)))
      (loop (+ i 1)))))

;; vm_integer_t is signed int32 here, so unsigned types beyond 31 bits
;; cap at the positive int32 ceiling and signed s32 covers the whole int32.
(define pkt-max-u32 #x7FFFFFFF)
(define pkt-min-s32 -2147483648)
(define pkt-max-s32 2147483647)

(define (pkt-validate-value value type spec)
  (let ((name (car spec)))
    (cond
      ((eq? type 'bit)
       (pkt-check-int value 0 1 name))
      ((eq? type 'bits)
       (pkt-check-int value 0 (- (bit-shift 1 (caddr spec)) 1) name))
      ((eq? type 'u8)
       (pkt-check-int value 0 255 name))
      ((memq type '(u16 u16/le))
       (pkt-check-int value 0 65535 name))
      ((memq type '(u24 u24/le))
       (pkt-check-int value 0 #xFFFFFF name))
      ((memq type '(u32 u32/le))
       (pkt-check-int value 0 pkt-max-u32 name))
      ((eq? type 's8)
       (pkt-check-int value -128 127 name))
      ((memq type '(s16 s16/le))
       (pkt-check-int value -32768 32767 name))
      ((memq type '(s32 s32/le))
       (pkt-check-int value pkt-min-s32 pkt-max-s32 name))
      ((eq? type 'bytes)
       (pkt-check-bytes value (caddr spec) name))
      (else
       (pkt-error "internal: unvalidated type" type)))))

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

;; --- Bindings validation ------------------------------------------------

(define (pkt-schema-has-name? schema name)
  (let loop ((rest schema))
    (cond
      ((null? rest) #f)
      ((eq? (car (car rest)) name) #t)
      (else (loop (cdr rest))))))

(define (pkt-validate-bindings schema bindings)
  (unless (list? bindings)
    (pkt-error "bindings must be a list of (name . value) pairs"
               'got bindings))
  (let loop ((rest bindings) (seen '()))
    (cond
      ((null? rest) #t)
      ((not (pair? (car rest)))
       (pkt-error "binding must be a (name . value) pair"
                  'got (car rest)))
      (else
       (let ((name (caar rest)))
         (unless (symbol? name)
           (pkt-error "binding name must be a symbol" 'got name))
         (when (memq name seen)
           (pkt-error "duplicate binding" 'name name))
         (unless (pkt-schema-has-name? schema name)
           (pkt-error "binding name not in schema" 'name name))
         (loop (cdr rest) (cons name seen)))))))

;; --- Public API ----------------------------------------------------------

(define (schema-bit-width schema)
  (let loop ((rest schema) (acc 0))
    (if (null? rest) acc
        (loop (cdr rest) (+ acc (pkt-field-bits (car rest)))))))

(define (pkt-schema-widths schema)
  (list->vector (map pkt-field-bits schema)))

(define (schema-construct schema bindings)
  (schema-validate schema)
  (pkt-validate-bindings schema bindings)
  (let ((widths (pkt-schema-widths schema))
        (values
         (list->vector
          (map (lambda (spec)
                 (let ((name (car spec))
                       (type (cadr spec)))
                   (let ((slot (assq name bindings)))
                     (if slot
                         (let ((v (cdr slot)))
                           (pkt-validate-value v type spec)
                           (pkt-encode v type spec))
                         (pkt-error "missing field in bindings" 'field name)))))
               schema))))
    (construct-packet widths values)))

(define (schema-deconstruct schema buffer)
  (schema-validate schema)
  (unless (buffer? buffer)
    (pkt-error "expected a byte buffer" 'got buffer))
  (let ((expected (quotient (schema-bit-width schema) 8)))
    (unless (= (vector-length buffer) expected)
      (pkt-error "buffer length mismatch"
                 'expected expected 'got (vector-length buffer))))
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
