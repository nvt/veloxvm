;; VeloxVM Unit Tests - Packet schema (fixed-width fields)
;; Tests for: schema-validate, schema-construct, schema-deconstruct
;; from languages/scheme/runtime/packet-schema.scm
;; (resolved via the reader's include search path).

(include "../unit-test-framework.scm")
(include "packet-schema.scm")

;; Helpers to build bindings at runtime (the Scheme compiler doesn't
;; round-trip dotted-pair literals through quoted forms).
(define (b1 name val) (list (cons name val)))
(define (b2 n1 v1 n2 v2) (list (cons n1 v1) (cons n2 v2)))

;; Read a byte from either a buffer (character element) or a regular
;; vector (integer element).
(define (byte-at v i)
  (let ((x (vector-ref v i)))
    (if (integer? x) x (char->integer x))))

(test-suite "Packet schema validation - shape errors")

(assert-error (schema-validate '())                    "empty schema")
(assert-error (schema-validate 42)                     "non-list schema")
(assert-error (schema-validate '(foo))                 "spec not a list")
(assert-error (schema-validate '(("x" u8)))            "name not a symbol")
(assert-error (schema-validate '((x glorp)))           "unknown type")
(assert-error (schema-validate '((x bits)))            "bits without width")
(assert-error (schema-validate '((x bits 9)))          "bits too wide")
(assert-error (schema-validate '((x bits 0)))          "bits zero width")
(assert-error (schema-validate '((x bytes)))           "bytes missing length")
(assert-error (schema-validate '((x u8 4)))            "u8 with stray arg")
(assert-error (schema-validate '((x bit) (y bits 3)))  "total width not byte-aligned")
(assert-error (schema-validate '((a bit) (b u8) (c bits 7)))
              "byte-aligned field starts mid-byte")
(assert-error (schema-validate '((a u8) (a u8)))       "duplicate field name")

(assert-true (schema-validate '((a u8) (b u16) (c bit) (d bits 7)))
             "small valid schema passes")

(test-suite "Packet schema value validation - range errors")

(define u8-schema    '((x u8)))
(define s8-schema    '((x s8)))
(define bit3-schema  '((x bits 3) (pad bits 5)))
(define bytes4-schema '((x bytes 4)))

(assert-error (schema-construct u8-schema (b1 'x 300))   "u8 too large")
(assert-error (schema-construct u8-schema (b1 'x -1))    "u8 negative")
(assert-error (schema-construct u8-schema (b1 'x "hi"))  "u8 non-integer")
(assert-error (schema-construct s8-schema (b1 'x -129))  "s8 underflow")
(assert-error (schema-construct s8-schema (b1 'x 128))   "s8 overflow")
(assert-error (schema-construct bit3-schema (b2 'x 8 'pad 0))
              "bits 3 overflow")
(assert-error (schema-construct bytes4-schema (b1 'x (vector 1 2 3)))
              "bytes wrong length")
(assert-error (schema-construct bytes4-schema (b1 'x 42))
              "bytes not a vector")
(assert-error (schema-construct bytes4-schema (b1 'x (vector 1 2 300 4)))
              "byte value out of 0..255")
(assert-error (schema-construct '((a u8) (b u8)) (b1 'a 1))
              "missing binding")

(test-suite "Packet schema bindings - structural errors")

(assert-error (schema-construct '((a u8)) 42)
              "bindings not a list")
(assert-error (schema-construct '((a u8)) (list 'oops))
              "binding not a pair")
(assert-error (schema-construct '((a u8) (b u8))
                                (list (cons 'a 1) (cons 'a 2) (cons 'b 3)))
              "duplicate binding key")
(assert-error (schema-construct '((a u8)) (list (cons 'a 1) (cons 'extra 2)))
              "binding name not in schema")

(test-suite "Packet schema deconstruct - buffer errors")

(assert-error (schema-deconstruct '((a u8)) 42)
              "deconstruct non-buffer")
(assert-error (schema-deconstruct '((a u8)) (vector 0))
              "deconstruct regular vector (not a buffer)")
(assert-error (let ((buf (schema-construct '((a u8)) (b1 'a 1))))
                (schema-deconstruct '((a u8) (b u8)) buf))
              "deconstruct wrong-length buffer")

(test-suite "Packet schema construct/deconstruct - happy paths")

;; Limit values exercise the range check boundaries. schema-construct
;; returns a buffer-backed bytevector, which is disjoint from vector?
;; per R7RS §3.2.
(assert-true (bytevector? (schema-construct u8-schema (b1 'x 0)))
             "u8 at 0")
(assert-true (bytevector? (schema-construct u8-schema (b1 'x 255)))
             "u8 at 255")
(assert-true (bytevector? (schema-construct '((x s16)) (b1 'x -32768)))
             "s16 at min")
(assert-true (bytevector? (schema-construct '((x s16)) (b1 'x 32767)))
             "s16 at max")

;; bytes round-trip preserves contents.
(let* ((buf (schema-construct bytes4-schema (b1 'x (vector 1 2 3 4))))
       (out (schema-deconstruct bytes4-schema buf))
       (data (cdr (assq 'x out))))
  (assert-equal 4 (vector-length data) "bytes 4 length")
  (assert-equal 1 (byte-at data 0) "bytes byte 0")
  (assert-equal 4 (byte-at data 3) "bytes byte 3"))

;; DIO header round-trips and produces the wire byte expected by RFC 6550.
(let* ((dio-schema '((rpl-instance-id u8) (version u8) (rank u16)
                     (g bit) (zero bit) (mop bits 3) (prf bits 3)
                     (dtsn u8) (flags u8) (reserved u8)
                     (dodag-id bytes 16)))
       (dodag (make-vector 16 0)))
  (vector-set! dodag 0 #xaa) (vector-set! dodag 1 #xaa)
  (vector-set! dodag 15 1)
  (let* ((buf (schema-construct dio-schema
                (list (cons 'rpl-instance-id 0)
                      (cons 'version 0)
                      (cons 'rank 256)
                      (cons 'g 1) (cons 'zero 0)
                      (cons 'mop 0) (cons 'prf 0)
                      (cons 'dtsn 0) (cons 'flags 0)
                      (cons 'reserved 0)
                      (cons 'dodag-id dodag))))
         (out (schema-deconstruct dio-schema buf)))
    (assert-equal 24 (vector-length buf) "DIO buffer length")
    (assert-equal #x80 (byte-at buf 4)
                  "DIO flags byte (G=1 MSB-first) is 0x80")
    (assert-equal 1 (byte-at buf 2)
                  "DIO rank big-endian high byte is 0x01")
    (assert-equal 256 (cdr (assq 'rank out)) "DIO rank round-trip")
    (assert-equal 1 (cdr (assq 'g out)) "DIO G flag round-trip")))

;; Little-endian and signed round-trip.
(let* ((schema '((be u16) (le u16/le) (neg s16)))
       (buf (schema-construct schema
              (list (cons 'be #x1234)
                    (cons 'le #x1234)
                    (cons 'neg -1))))
       (out (schema-deconstruct schema buf)))
  (assert-equal #x12 (byte-at buf 0) "be byte 0")
  (assert-equal #x34 (byte-at buf 1) "be byte 1")
  (assert-equal #x34 (byte-at buf 2) "le byte 0")
  (assert-equal #x12 (byte-at buf 3) "le byte 1")
  (assert-equal #xff (byte-at buf 4) "s16 -1 byte 0")
  (assert-equal #xff (byte-at buf 5) "s16 -1 byte 1")
  (assert-equal #x1234 (cdr (assq 'be out))  "u16 round-trip")
  (assert-equal #x1234 (cdr (assq 'le out))  "u16/le round-trip")
  (assert-equal -1     (cdr (assq 'neg out)) "s16 -1 round-trip"))

(test-summary)
