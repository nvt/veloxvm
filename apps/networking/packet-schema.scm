;; packet-schema.scm — a symbolic layer over construct-packet /
;; deconstruct-packet. Schemas are lists of field specs:
;;
;;   (NAME TYPE [ARG])
;;
;; Fixed-width field types:
;;   bit              1-bit unsigned
;;   bits N           N-bit unsigned (1..8 — sub-byte; use uK/sK for wider)
;;   u8               unsigned byte
;;   u16, u24, u32    unsigned big-endian
;;   u16/le, u32/le   unsigned little-endian
;;   s8, s16, s32     signed two's-complement, big-endian
;;   s16/le, s32/le   signed two's-complement, little-endian
;;   bytes N          N raw bytes (read/written as a Scheme vector of bytes)
;;
;; Variable-length field types (require enclosing field to be byte-aligned):
;;   rest                        all remaining bytes; must be the last field.
;;   length-prefixed PREFIX-T    a PREFIX-T (u8/u16/u24/u32) length field
;;                               followed by that many raw bytes. The
;;                               binding value is the byte vector only —
;;                               the prefix is computed/consumed implicitly.
;;
;; Sub-byte fields (bit, bits) are packed MSB-first within a byte, matching
;; RFC bit-numbering. Multi-byte fields are big-endian unless suffixed /le.
;;
;; Entry points:
;;   (schema-validate SCHEMA)                  -> #t, or raises on error
;;   (schema-construct SCHEMA BINDINGS-ALIST)  -> byte buffer
;;   (schema-deconstruct SCHEMA BYTE-BUFFER)   -> bindings alist
;;   (schema-bit-width SCHEMA)                 -> fixed-portion width in bits
;;   (schema-variable? SCHEMA)                 -> #t if any variable field

(define (pkt-error msg . irritants)
  (raise (cons 'packet-schema-error (cons msg irritants))))

;; --- Type table ----------------------------------------------------------

;; A 16-element quoted-list literal compiles to a 17-object form, which
;; exceeds VM_OBJECT_STACK_SIZE (16). Split into chunks small enough to
;; load as forms.
(define (pkt-known-type? type)
  (or (memq type '(bit bits u8 u16 u24 u32 u16/le u32/le))
      (memq type '(s8 s16 s32 s16/le s32/le bytes))
      (memq type '(rest length-prefixed))))

(define pkt-variable-types '(rest length-prefixed))

(define pkt-length-prefix-types '(u8 u16 u24 u32))

;; Every type except bit/bits must start at a byte boundary; bit-packed
;; types are the only ones that may live mid-byte.
(define (pkt-byte-aligned-type? type)
  (not (memq type '(bit bits))))

(define (pkt-variable-type? type)
  (and (memq type pkt-variable-types) #t))

(define (schema-variable? schema)
  (let loop ((rest schema))
    (cond
      ((null? rest) #f)
      ((pkt-variable-type? (cadr (car rest))) #t)
      (else (loop (cdr rest))))))

;; Width in bits of a *fixed* field. Variable-length fields raise.
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
      ((pkt-variable-type? type)
       (pkt-error "variable-length field has no fixed width"
                  'spec spec))
      (else (pkt-error "unknown field type" type)))))

;; Width in bits of the prefix portion (for length-prefixed) or 0 (for rest).
(define (pkt-variable-prefix-bits spec)
  (let ((type (cadr spec)))
    (cond
      ((eq? type 'rest) 0)
      ((eq? type 'length-prefixed)
       (let ((pt (caddr spec)))
         (cond
           ((eq? pt 'u8) 8)
           ((eq? pt 'u16) 16)
           ((eq? pt 'u24) 24)
           ((eq? pt 'u32) 32)
           (else (pkt-error "internal: bad length-prefixed prefix" spec)))))
      (else 0))))

;; --- Schema validation ---------------------------------------------------

(define (pkt-validate-spec spec)
  (unless (and (list? spec) (>= (length spec) 2) (<= (length spec) 3))
    (pkt-error "field spec must be (name type [arg])" spec))
  (let ((name (car spec)) (type (cadr spec)))
    (unless (symbol? name)
      (pkt-error "field name must be a symbol" 'spec spec))
    (unless (pkt-known-type? type)
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
      ((eq? type 'rest)
       (when (= (length spec) 3)
         (pkt-error "'rest takes no argument" 'spec spec)))
      ((eq? type 'length-prefixed)
       (unless (= (length spec) 3)
         (pkt-error "'length-prefixed requires a prefix type" 'spec spec))
       (unless (memq (caddr spec) pkt-length-prefix-types)
         (pkt-error "'length-prefixed prefix must be one of u8/u16/u24/u32"
                    'spec spec)))
      (else
       (when (= (length spec) 3)
         (pkt-error "type does not take an argument" 'spec spec))))))

(define (schema-validate schema)
  (when (or (not (list? schema)) (null? schema))
    (pkt-error "schema must be a non-empty list of field specs"))
  ;; pos counts bits since the last byte boundary the fixed-width math can
  ;; reason about; it resets to 0 across any variable-length field, whose
  ;; contents are byte-aligned by definition.
  (let loop ((rest schema) (pos 0) (seen '()) (after-rest? #f))
    (if (null? rest)
        (if (zero? (modulo pos 8))
            #t
            (pkt-error "trailing fixed-width portion not a multiple of 8"
                       'bits pos))
        (let ((spec (car rest)))
          (when after-rest?
            (pkt-error "no field allowed after 'rest" 'field (car spec)))
          (pkt-validate-spec spec)
          (let ((name (car spec))
                (type (cadr spec)))
            (when (memq name seen)
              (pkt-error "duplicate field name in schema" 'name name))
            (when (and (pkt-byte-aligned-type? type)
                       (not (zero? (modulo pos 8))))
              (pkt-error "byte-aligned field does not start at a byte boundary"
                         'field name 'at-bit pos))
            (cond
              ((eq? type 'rest)
               (loop (cdr rest) 0 (cons name seen) #t))
              ((eq? type 'length-prefixed)
               (loop (cdr rest) 0 (cons name seen) #f))
              (else
               (loop (cdr rest)
                     (+ pos (pkt-field-bits spec))
                     (cons name seen)
                     #f))))))))

;; --- Value validation ----------------------------------------------------

(define (pkt-check-int value lo hi name)
  (unless (integer? value)
    (pkt-error "expected integer" 'field name 'got value))
  (when (or (< value lo) (> value hi))
    (pkt-error "value out of range"
               'field name 'value value 'range (list lo hi))))

(define (pkt-byte-at v i)
  ;; Read a byte from a regular vector (integer element) or a byte buffer
  ;; (character element).
  (let ((x (vector-ref v i)))
    (if (integer? x) x (char->integer x))))

(define (pkt-check-bytes value n name)
  (unless (vector? value)
    (pkt-error "expected byte vector" 'field name 'got value))
  (unless (= (vector-length value) n)
    (pkt-error "wrong vector length"
               'field name 'expected n 'got (vector-length value)))
  (pkt-check-each-byte value n name))

(define (pkt-check-each-byte value n name)
  ;; Byte buffers can only hold 0..255 by construction; skip the per-element
  ;; check for that path so a large buffer doesn't blow the call stack.
  (unless (buffer? value)
    (let loop ((i 0))
      (when (< i n)
        (let ((b (vector-ref value i)))
          (unless (and (integer? b) (>= b 0) (<= b 255))
            (pkt-error "byte out of range 0..255"
                       'field name 'index i 'value b)))
        (loop (+ i 1))))))

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
      ((memq type '(rest length-prefixed))
       (pkt-check-bytes-any value name)
       (when (eq? type 'length-prefixed)
         (let ((max (pkt-length-prefix-max (caddr spec))))
           (when (> (vector-length value) max)
             (pkt-error "length-prefixed value too long for prefix"
                        'field name 'length (vector-length value)
                        'max max)))))
      (else
       (pkt-error "internal: unvalidated type" type)))))

(define (pkt-check-bytes-any value name)
  ;; Same as pkt-check-bytes but length isn't fixed up front.
  (unless (vector? value)
    (pkt-error "expected byte vector" 'field name 'got value))
  (pkt-check-each-byte value (vector-length value) name))

(define (pkt-length-prefix-max prefix-type)
  (cond
    ((eq? prefix-type 'u8) 255)
    ((eq? prefix-type 'u16) 65535)
    ((eq? prefix-type 'u24) #xFFFFFF)
    ((eq? prefix-type 'u32) pkt-max-u32)
    (else (pkt-error "internal: bad length-prefixed prefix" prefix-type))))

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

;; --- Variable-length block splitting ------------------------------------

;; Group a schema into a sequence of contiguous fixed runs separated by
;; variable-length fields. Each element of the result is either:
;;   (fixed (SPEC ...))
;;   (var SPEC)
(define (pkt-schema-blocks schema)
  (let loop ((rest schema) (current-fixed '()) (acc '()))
    (cond
      ((null? rest)
       (reverse (if (null? current-fixed)
                    acc
                    (cons (list 'fixed (reverse current-fixed)) acc))))
      ((pkt-variable-type? (cadr (car rest)))
       (let ((acc2 (if (null? current-fixed)
                       acc
                       (cons (list 'fixed (reverse current-fixed)) acc))))
         (loop (cdr rest) '() (cons (list 'var (car rest)) acc2))))
      (else
       (loop (cdr rest) (cons (car rest) current-fixed) acc)))))

;; --- Variable-length construct / deconstruct ----------------------------

(define (pkt-bytes->buffer v)
  ;; Normalise a byte-vector binding to a byte buffer. Accepts either
  ;; (vector b0 b1 ...) (regular vector of byte-valued integers) or an
  ;; existing byte buffer.
  (if (buffer? v)
      v
      (let ((buf (make-buffer (vector-length v))))
        (let loop ((i 0))
          (if (= i (vector-length v))
              buf
              (begin
                (buffer-append buf i (vector-ref v i))
                (loop (+ i 1))))))))

;; Pulled out of pkt-concat-buffers so the iteration is a top-level
;; tail-recursive function; gives TCO the best chance against stack
;; pressure from deeply nested callers.
(define (pkt-concat-walk bufs result pos)
  (if (null? bufs)
      result
      (let ((chunk (car bufs)))
        ;; buffer-append rejects index == dst-length even for a zero-byte
        ;; source, so skip empty chunks rather than emit a spurious error.
        (when (> (vector-length chunk) 0)
          (buffer-append result pos chunk))
        (pkt-concat-walk (cdr bufs)
                         result
                         (+ pos (vector-length chunk))))))

(define (pkt-concat-buffers chunks)
  (let* ((bufs  (map pkt-bytes->buffer chunks))
         (total (apply + (map vector-length bufs))))
    (pkt-concat-walk bufs (make-buffer total) 0)))

;; Encode/decode a single prefix-type integer as a small standalone buffer,
;; reusing the fixed-width primitive rather than re-implementing the math.
(define (pkt-encode-prefix prefix-type value)
  (let ((bits (pkt-field-bits (list 'p prefix-type))))
    (construct-packet (vector bits) (vector value))))

(define (pkt-decode-prefix prefix-type buffer offset)
  (let* ((bits  (pkt-field-bits (list 'p prefix-type)))
         (bytes (quotient bits 8))
         (chunk (slice buffer offset (+ offset bytes)))
         (out   (deconstruct-packet (vector bits) chunk)))
    (vector-ref out 0)))

(define (pkt-construct-fixed-block specs bindings)
  ;; Build a sub-buffer for a fixed run by delegating to construct-packet.
  (let ((widths (list->vector (map pkt-field-bits specs)))
        (values
         (list->vector
          (map (lambda (spec)
                 (let ((slot (assq (car spec) bindings)))
                   (pkt-encode (cdr slot) (cadr spec) spec)))
               specs))))
    (construct-packet widths values)))

(define (pkt-deconstruct-fixed-block specs buffer offset)
  ;; Returns (values alist-fragment next-offset).
  (let* ((widths (list->vector (map pkt-field-bits specs)))
         (bits   (apply + (vector->list widths)))
         (bytes  (quotient bits 8))
         (chunk  (slice buffer offset (+ offset bytes)))
         (raw    (deconstruct-packet widths chunk)))
    (let loop ((i 0) (rest specs) (acc '()))
      (if (null? rest)
          (cons (reverse acc) (+ offset bytes))
          (let* ((spec (car rest))
                 (val  (pkt-decode (vector-ref raw i) (cadr spec) spec)))
            (loop (+ i 1) (cdr rest)
                  (cons (cons (car spec) val) acc)))))))

(define (schema-construct-var schema bindings)
  (let loop ((blocks (pkt-schema-blocks schema)) (chunks '()))
    (if (null? blocks)
        (pkt-concat-buffers (reverse chunks))
        (let ((block (car blocks)))
          (cond
            ((eq? (car block) 'fixed)
             (loop (cdr blocks)
                   (cons (pkt-construct-fixed-block (cadr block) bindings)
                         chunks)))
            ((eq? (car block) 'var)
             (let* ((spec  (cadr block))
                    (name  (car spec))
                    (type  (cadr spec))
                    (slot  (assq name bindings))
                    (value (cdr slot)))
               (cond
                 ((eq? type 'rest)
                  (loop (cdr blocks) (cons value chunks)))
                 ((eq? type 'length-prefixed)
                  (let ((prefix (pkt-encode-prefix (caddr spec)
                                                   (vector-length value))))
                    (loop (cdr blocks)
                          (cons value (cons prefix chunks)))))
                 (else (pkt-error "internal: unknown var type" type)))))
            (else (pkt-error "internal: unknown block tag" block)))))))

(define (schema-deconstruct-var schema buffer)
  (let loop ((blocks (pkt-schema-blocks schema)) (offset 0) (acc '()))
    (if (null? blocks)
        (begin
          (unless (= offset (vector-length buffer))
            (pkt-error "trailing bytes in buffer after schema consumed"
                       'consumed offset
                       'length (vector-length buffer)))
          (reverse acc))
        (let ((block (car blocks)))
          (cond
            ((eq? (car block) 'fixed)
             (let ((result (pkt-deconstruct-fixed-block
                            (cadr block) buffer offset)))
               (loop (cdr blocks)
                     (cdr result)
                     (append (reverse (car result)) acc))))
            ((eq? (car block) 'var)
             (let* ((spec (cadr block))
                    (name (car spec))
                    (type (cadr spec)))
               (cond
                 ((eq? type 'rest)
                  (let ((tail (slice buffer offset (vector-length buffer))))
                    (loop (cdr blocks)
                          (vector-length buffer)
                          (cons (cons name tail) acc))))
                 ((eq? type 'length-prefixed)
                  (let* ((prefix-t (caddr spec))
                         (prefix-bits (pkt-field-bits (list 'p prefix-t)))
                         (prefix-bytes (quotient prefix-bits 8))
                         (len (pkt-decode-prefix prefix-t buffer offset))
                         (start (+ offset prefix-bytes))
                         (end (+ start len)))
                    (when (> end (vector-length buffer))
                      (pkt-error "length-prefixed runs past buffer end"
                                 'field name 'need end
                                 'have (vector-length buffer)))
                    (loop (cdr blocks) end
                          (cons (cons name (slice buffer start end))
                                acc))))
                 (else (pkt-error "internal: unknown var type" type)))))
            (else (pkt-error "internal: unknown block tag" block)))))))

;; --- Public API ----------------------------------------------------------

;; Width of the *fixed* portion of a schema, in bits. Variable-length fields
;; contribute their prefix bits (0 for rest, prefix-type bits for
;; length-prefixed) so the result is always a multiple of 8 for a valid
;; schema.
(define (schema-bit-width schema)
  (let loop ((rest schema) (acc 0))
    (if (null? rest)
        acc
        (let ((spec (car rest)))
          (loop (cdr rest)
                (+ acc (if (pkt-variable-type? (cadr spec))
                           (pkt-variable-prefix-bits spec)
                           (pkt-field-bits spec))))))))

(define (pkt-validate-value-for-each schema bindings)
  (for-each (lambda (spec)
              (let* ((name (car spec))
                     (type (cadr spec))
                     (slot (assq name bindings)))
                (unless slot
                  (pkt-error "missing field in bindings" 'field name))
                (pkt-validate-value (cdr slot) type spec)))
            schema))

(define (schema-construct schema bindings)
  (schema-validate schema)
  (pkt-validate-bindings schema bindings)
  (pkt-validate-value-for-each schema bindings)
  (if (schema-variable? schema)
      (schema-construct-var schema bindings)
      ;; Fixed-only fast path: one construct-packet call.
      (let ((widths (list->vector (map pkt-field-bits schema)))
            (values (list->vector
                     (map (lambda (spec)
                            (pkt-encode (cdr (assq (car spec) bindings))
                                        (cadr spec) spec))
                          schema))))
        (construct-packet widths values))))

(define (schema-deconstruct schema buffer)
  (schema-validate schema)
  (unless (buffer? buffer)
    (pkt-error "expected a byte buffer" 'got buffer))
  (if (schema-variable? schema)
      (schema-deconstruct-var schema buffer)
      ;; Fixed-only fast path: exact-length check + one deconstruct-packet.
      (let ((expected (quotient (schema-bit-width schema) 8)))
        (unless (= (vector-length buffer) expected)
          (pkt-error "buffer length mismatch"
                     'expected expected 'got (vector-length buffer)))
        (let* ((widths (list->vector (map pkt-field-bits schema)))
               (raw    (deconstruct-packet widths buffer)))
          (let loop ((i 0) (rest schema) (acc '()))
            (if (null? rest)
                (reverse acc)
                (let* ((spec (car rest))
                       (val  (pkt-decode (vector-ref raw i)
                                          (cadr spec) spec)))
                  (loop (+ i 1) (cdr rest)
                        (cons (cons (car spec) val) acc)))))))))
