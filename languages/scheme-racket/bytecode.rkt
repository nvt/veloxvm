#lang racket

;; VeloxVM Racket Compiler - Bytecode Generation
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Generates VeloxVM bytecode files
;; Based on: include/vm-bytecode.h, core/vm-loader.c

(require "primitives.rkt")

(provide (struct-out bytecode)
         (struct-out expr-encoding)
         bytecode-strings
         bytecode-symbols
         bytecode-expressions
         bytecode-expression-count
         make-bytecode
         add-string
         add-symbol
         add-expr
         replace-expr
         write-bytecode-file
         ;; Encoding functions
         encode-integer
         encode-rational
         encode-real
         encode-string
         encode-symbol
         encode-boolean
         encode-character
         encode-nil
         encode-form-inline
         encode-form-lambda
         encode-form-ref)

;; ============================================================================
;; Data Structures
;; ============================================================================

;; Bytecode file structure
;;
;; The internal list fields are stored in REVERSE insertion order so that
;; add-* can cons onto the head in O(1). The public accessors
;; (bytecode-strings / -symbols / -expressions) reverse once on read.
;; Parallel hash indices provide O(1) dedup/lookup.
(struct bytecode
  (magic              ; Magic number: #x5E #xB5
   version            ; Bytecode format version
   strings-rev        ; (listof string), reverse insertion order
   strings-index      ; hash: string -> index
   symbols-rev        ; (listof symbol), reverse insertion order
   symbols-index      ; hash: symbol -> index
   exprs-rev          ; (listof expr-encoding), reverse insertion order
   expr-count)        ; number of expressions added
  #:mutable
  #:transparent)

;; Expression encoding
(struct expr-encoding
  (type           ; 'atom or 'form
   data)          ; (listof byte)
  #:transparent)

;; ============================================================================
;; Bytecode Construction
;; ============================================================================

;; Create new bytecode container
(define (make-bytecode)
  (bytecode #x5EB5        ; Magic number
            3             ; Version
            '()           ; strings-rev
            (make-hash)   ; strings-index
            '()           ; symbols-rev
            (make-hash)   ; symbols-index
            '()           ; exprs-rev
            0))           ; expr-count

;; Ordered (insertion-order) accessors. Reverse is O(n); callers that need
;; to iterate should cache the result rather than call in a loop.
(define (bytecode-strings bc)
  (reverse (bytecode-strings-rev bc)))

(define (bytecode-symbols bc)
  (reverse (bytecode-symbols-rev bc)))

(define (bytecode-expressions bc)
  (reverse (bytecode-exprs-rev bc)))

(define (bytecode-expression-count bc)
  (bytecode-expr-count bc))

;; Add string to table, return index (deduplicating via hash).
(define (add-string bc str)
  (define index (bytecode-strings-index bc))
  (or (hash-ref index str #f)
      (let ([idx (hash-count index)])
        (hash-set! index str idx)
        (set-bytecode-strings-rev! bc (cons str (bytecode-strings-rev bc)))
        idx)))

;; Add symbol to table, return index (deduplicating via hash).
(define (add-symbol bc sym)
  (define index (bytecode-symbols-index bc))
  (or (hash-ref index sym #f)
      (let ([idx (hash-count index)])
        (hash-set! index sym idx)
        (set-bytecode-symbols-rev! bc (cons sym (bytecode-symbols-rev bc)))
        idx)))

;; Add compiled expression; returns the new expression's id.
(define (add-expr bc encoding)
  (let ([idx (bytecode-expr-count bc)])
    (set-bytecode-exprs-rev! bc (cons encoding (bytecode-exprs-rev bc)))
    (set-bytecode-expr-count! bc (+ idx 1))
    idx))

;; Replace expression at given index.
(define (replace-expr bc index encoding)
  (let ([count (bytecode-expr-count bc)])
    (when (>= index count)
      (error 'replace-expr "Index ~a out of bounds (length ~a)" index count))
    ;; exprs-rev is stored in reverse, so logical index i lives at
    ;; reversed position (count - 1 - i).
    (set-bytecode-exprs-rev! bc
      (list-set (bytecode-exprs-rev bc) (- count 1 index) encoding))))

;; ============================================================================
;; Bit Packing Utilities
;; ============================================================================

;; Constants from vm-bytecode.h
(define VM-TOKEN-ATOM 0)
(define VM-TOKEN-FORM 1)
(define VM-LENGTH-TOKEN 1)
(define VM-LENGTH-EMBEDDED 4)
(define VM-LENGTH-TYPE 3)

;; Pack atom header: 1-bit token + 4-bit info + 3-bit type = 8 bits (1 byte)
(define (pack-atom-header info type)
  (bitwise-ior
   (arithmetic-shift VM-TOKEN-ATOM 7)          ; bit 7
   (bitwise-and (arithmetic-shift info 3) #x78) ; bits 6-3
   (bitwise-and type #x07)))                    ; bits 2-0

;; Calculate bytes needed to store an integer
(define (integer-byte-length n)
  (max 1 (quotient (+ (integer-length n) 7) 8)))

;; Convert integer to byte list (big-endian)
(define (integer->bytes n num-bytes)
  (for/list ([i (in-range (- num-bytes 1) -1 -1)])
    (bitwise-and (arithmetic-shift n (* -8 i)) #xFF)))

;; ============================================================================
;; Encoding Functions
;; ============================================================================

(define (encode-boolean b)
  (let ([header (pack-atom-header (if b 1 0) VM-TYPE-BOOLEAN)])
    (expr-encoding 'atom (list header))))

;; Maximum integer byte count accepted by the VM
;; (see core/vm-bytecode.c:get_integer — rejects nbytes > 4).
(define VM-MAX-INTEGER-BYTES 4)

(define (encode-integer n)
  (let* ([value (abs n)]
         [bytes-needed (integer-byte-length value)])
    (when (> bytes-needed VM-MAX-INTEGER-BYTES)
      (error 'encode-integer
             "integer out of range (~a needs ~a bytes, VM limit is ~a): ~a"
             n bytes-needed VM-MAX-INTEGER-BYTES n))
    (let* ([sign-bit (if (negative? n) 1 0)]
           ;; Embedded field (bits 6-3): bit 3 = sign, bits 2-0 = size
           [info (bitwise-ior (arithmetic-shift sign-bit 3) bytes-needed)]
           [header (pack-atom-header info VM-TYPE-INTEGER)]
           [value-bytes (integer->bytes value bytes-needed)])
      (expr-encoding 'atom (cons header value-bytes)))))

(define (encode-rational n)
  ;; Rational number - encode as header + numerator encoding + denominator encoding
  (let* ([header (pack-atom-header 0 VM-TYPE-RATIONAL)]
         [num (numerator n)]
         [den (denominator n)]
         [num-enc (encode-integer num)]
         [den-enc (encode-integer den)]
         [num-bytes (expr-encoding-data num-enc)]
         [den-bytes (expr-encoding-data den-enc)])
    (expr-encoding 'atom (append (list header) num-bytes den-bytes))))

(define (encode-real r)
  (let ([header (pack-atom-header 0 VM-TYPE-REAL)])
    ;; Real is encoded as header + 4-byte IEEE 754 float
    ;; Convert real to 32-bit IEEE 754 representation
    (define bytes (real->floating-point-bytes r 4 #f))
    (expr-encoding 'atom (cons header (bytes->list bytes)))))

(define (encode-character ch)
  (let ([header (pack-atom-header 0 VM-TYPE-CHARACTER)]
        [code (char->integer ch)])
    ;; The VM represents characters as uint8_t (see include/vm-objects.h:
    ;; vm_character_t), so codepoints above 0xFF cannot be encoded.
    (when (> code 255)
      (error 'encode-character
             "character codepoint out of range (~a = U+~a, VM max is U+00FF): ~a"
             code (number->string code 16) ch))
    (expr-encoding 'atom (list header code))))

;; String-table index: 7 bits (simple) or 15 bits (extended, 7+8).
(define VM-MAX-STRING-INDEX (- (expt 2 15) 1))

(define (encode-string str bc)
  (define idx (add-string bc str))
  (when (> idx VM-MAX-STRING-INDEX)
    (error 'encode-string
           "too many strings in program (index ~a exceeds VM limit ~a)"
           idx VM-MAX-STRING-INDEX))
  (let ([header (pack-atom-header 0 VM-TYPE-STRING)])
    ;; String encoding: header + table index (1 or 2 bytes for extended)
    (if (< idx 128)
        ;; Simple form: 1 byte with bit 7=0, bits 6-0 = ID
        (expr-encoding 'atom (list header idx))
        ;; Extended form: 2 bytes with bit 7=1, bits 6-0 = high 7 bits, byte 2 = low 8 bits
        (let* ([high-bits (bitwise-and (arithmetic-shift idx -8) #x7F)]
               [low-byte (bitwise-and idx #xFF)]
               [byte1 (bitwise-ior #x80 high-bits)])  ; bit 7=1 for extended
          (expr-encoding 'atom (list header byte1 low-byte))))))

;; Pre-built hash for O(1) primitive-id lookup. vm-primitives is a fixed
;; list compiled into the module, so we can memoize once at load time.
(define primitive-id-table
  (let ([h (make-hash)])
    (for ([p (in-list vm-primitives)]
          [i (in-naturals)])
      (hash-set! h p i))
    h))

(define (get-primitive-id sym)
  (hash-ref primitive-id-table sym #f))

(define (encode-symbol sym bc [env '()])
  ;; Check if this is a lambda parameter, VM primitive (core scope), or user symbol (app scope)
  ;; Check environment FIRST to handle cases where parameter names shadow primitives
  (cond
    ;; Lambda parameter - add-symbol handles dedup via its hash index.
    [(member sym env)
     (encode-symbol-with-id (add-symbol bc sym) 1)]  ; scope=1 (app)
    ;; VM primitive - use core scope
    [(get-primitive-id sym)
     => (lambda (prim-id)
          (encode-symbol-with-id prim-id 0))]  ; scope=0 (core)
    ;; User symbol - use app scope
    [else
     (encode-symbol-with-id (add-symbol bc sym) 1)]))

;; Symbol-table index: 6 bits (simple) or 14 bits (extended, 6+8).
(define VM-MAX-SYMBOL-INDEX (- (expt 2 14) 1))

(define (encode-symbol-with-id idx scope)
  ;; Encode symbol given its ID and scope
  ;; bit 6=0 for simple form (1 byte ID), bit 6=1 for extended form (2 byte ID)
  (when (> idx VM-MAX-SYMBOL-INDEX)
    (error 'encode-symbol-with-id
           "too many symbols in program (index ~a exceeds VM limit ~a)"
           idx VM-MAX-SYMBOL-INDEX))
  (let ([header (pack-atom-header 0 VM-TYPE-SYMBOL)])
    (if (< idx 64)
        ;; Simple form: 1 byte with scope bit (7), extended=0 bit (6), and ID (5-0)
        (let ([sym-byte (bitwise-ior (arithmetic-shift scope 7)
                                      ;; NO 0x40 - bit 6=0 for simple form
                                      idx)])
          (expr-encoding 'atom (list header sym-byte)))
        ;; Extended form: 2 bytes (bit 6=1, bits 5-0 = high 6 bits, byte 2 = low 8 bits)
        (let* ([high-bits (bitwise-and (arithmetic-shift idx -8) #x3F)]
               [low-bits (bitwise-and idx #xFF)]
               [byte1 (bitwise-ior (arithmetic-shift scope 7)
                                   #x40  ; bit 6=1 for extended form
                                   high-bits)])
          (expr-encoding 'atom (list header byte1 low-bits))))))

(define (encode-nil)
  ;; NIL/empty list - encoded as a boolean false for now
  ;; (VeloxVM uses #f to represent empty list)
  (encode-boolean #f))

;; ============================================================================
;; Form Encoding
;; ============================================================================

;; Inline form arg-count field is 6 bits.
(define VM-MAX-INLINE-ARGC 63)

(define (encode-form-inline arg-count)
  ;; Inline form: single byte encoding
  ;; Bit 7: 1 (FORM token)
  ;; Bits 6-5: 00 (form-type=INLINE, extracted via >> 5 & 3)
  ;; Bits 5-0: arg-count (6 bits, max 63)
  (when (> arg-count VM-MAX-INLINE-ARGC)
    (error 'encode-form-inline
           "too many arguments in form (~a exceeds VM inline limit of ~a)"
           arg-count VM-MAX-INLINE-ARGC))
  (let ([header (bitwise-ior #x80                     ; bit 7 = 1 (FORM token)
                             (arithmetic-shift 0 5)   ; bits 6-5 = 0 (INLINE)
                             (bitwise-and arg-count #x3F))]) ; bits 5-0 = argc
    (expr-encoding 'form (list header))))

;; Lambda/ref form expr-id: 4 bits (simple) or 12 bits (extended, 4+8).
(define VM-MAX-EXPR-ID (- (expt 2 12) 1))

(define (encode-form-lambda expr-id)
  ;; Lambda form: bit 7=1, form-type=1 (lambda), expr-id reference
  ;; Byte 0: 1 (FORM) | 01 (form-type=LAMBDA) | simple | expr-id (lower 4 bits)
  ;; For simple form: bit 7=1, bits 6-5=01 (LAMBDA), bit 4=1 (simple), bits 3-0=expr_id
  (when (> expr-id VM-MAX-EXPR-ID)
    (error 'encode-form-lambda
           "too many expressions in program (id ~a exceeds VM limit ~a)"
           expr-id VM-MAX-EXPR-ID))
  (if (< expr-id 16)
      ;; Simple form: 4-bit ID embedded in first byte
      (let ([header (bitwise-ior #x80                          ; bit 7 = 1 (FORM)
                                  (arithmetic-shift 1 5)        ; bits 6-5 = 01 (LAMBDA)
                                  #x10                          ; bit 4 = 1 (simple)
                                  (bitwise-and expr-id #x0F))]) ; bits 3-0 = ID
        (expr-encoding 'form (list header)))
      ;; Extended form: 12-bit ID
      ;; VM reads: expr_id = (header_bits_3_0 << 8) | next_byte
      ;; So for expr_id with bits [11:0], we encode:
      ;;   header bits 3-0 = expr_id bits 11-8
      ;;   next byte = expr_id bits 7-0
      (let* ([bits-11-8 (bitwise-and (arithmetic-shift expr-id -8) #x0F)]   ; upper nibble
             [bits-7-0 (bitwise-and expr-id #xFF)]                           ; lower byte
             [header (bitwise-ior #x80                        ; bit 7 = 1 (FORM)
                                  (arithmetic-shift 1 5)      ; bits 6-5 = 01 (LAMBDA)
                                  bits-11-8)])                ; bits 3-0 = expr_id bits 11-8
        (expr-encoding 'form (list header bits-7-0)))))

(define (encode-form-ref expr-id)
  ;; Form reference: bit 7=1, form-type=2 (REF), expr-id reference
  ;; Byte 0: 1 (FORM) | 10 (form-type=REF) | simple-flag | expr-id (lower 4 bits)
  ;; [Byte 1]: expr-id (upper 8 bits) - only if simple-flag is 0
  ;;
  ;; Form type encoding:
  ;; - Bit 7 = 1 (FORM token)
  ;; - Bits 6-5 = 10 (REF type = 2)
  ;; - Bit 4 = simple flag (1 = 4-bit ID, 0 = extended 12-bit ID)
  ;; - Bits 3-0 = expr_id (lower 4 bits)
  (when (> expr-id VM-MAX-EXPR-ID)
    (error 'encode-form-ref
           "too many expressions in program (id ~a exceeds VM limit ~a)"
           expr-id VM-MAX-EXPR-ID))
  (if (< expr-id 16)
      ;; Simple form: 4-bit ID embedded in first byte
      (let ([header (bitwise-ior #x80                          ; bit 7 = 1 (FORM)
                                  (arithmetic-shift 2 5)        ; bits 6-5 = 10 (REF)
                                  #x10                          ; bit 4 = 1 (simple)
                                  (bitwise-and expr-id #x0F))]) ; bits 3-0 = ID
        (expr-encoding 'form (list header)))
      ;; Extended form: 12-bit ID
      ;; VM reads: expr_id = (header_bits_3_0 << 8) | next_byte
      (let* ([bits-11-8 (bitwise-and (arithmetic-shift expr-id -8) #x0F)]
             [bits-7-0 (bitwise-and expr-id #xFF)]
             [header (bitwise-ior #x80                          ; bit 7 = 1 (FORM)
                                  (arithmetic-shift 2 5)        ; bits 6-5 = 10 (REF)
                                  bits-11-8)])                  ; bits 3-0 = expr_id bits 11-8
        (expr-encoding 'form (list header bits-7-0)))))

;; ============================================================================
;; Bytecode File Writing
;; ============================================================================

(define (write-bytecode-file filename bc)
  (with-output-to-file filename
    #:exists 'replace
    #:mode 'binary
    (lambda ()
      ;; Magic number
      (write-bytes (integer->integer-bytes (bytecode-magic bc) 2 #f #t))

      ;; Version
      (write-byte (bytecode-version bc))

      ;; String table
      (write-table (bytecode-strings bc) write-string-entry)

      ;; Symbol table
      (write-table (bytecode-symbols bc) write-symbol-entry)

      ;; Expression table
      (write-table (bytecode-expressions bc) write-expr-entry)

      ;; Captures section: count of {expr_id, [symbol_id, ...]} entries.
      ;; Each entry is uint16-length-prefixed, then uint16 expr_id +
      ;; uint16 symbol_id per captured free variable. v1 of the
      ;; closure work emits an empty section -- the compiler doesn't
      ;; yet do free-variable analysis. The C loader allocates per-
      ;; expression captures storage on demand from this table.
      (write-u16 0))))

(define (write-table items write-item)
  (write-u16 (length items))
  (for-each write-item items))

(define (write-string-entry str)
  (let ([bytes (string->bytes/utf-8 str)])
    (write-u16 (bytes-length bytes))
    (write-bytes bytes)))

(define (write-symbol-entry sym)
  (write-string-entry (symbol->string sym)))

(define (write-expr-entry encoding)
  ;; Write expression entry: 16-bit length + bytes
  (let ([data (expr-encoding-data encoding)])
    (write-u16 (length data))
    (for ([byte data])
      (write-u8 byte))))

;; Helper functions
(define (write-u16 n)
  (write-bytes (integer->integer-bytes n 2 #f #f)))

(define (write-u8 n)
  (write-byte n))
