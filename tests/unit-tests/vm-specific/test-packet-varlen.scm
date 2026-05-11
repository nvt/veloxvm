;; VeloxVM Unit Tests - Packet schema variable-length fields
;; Tests for: rest, length-prefixed
;; over the apps/networking/packet-schema.scm library.

(include "../unit-test-framework.scm")
(include "../../../apps/networking/packet-schema.scm")

(define (b name val) (list (cons name val)))

;; Read a byte from a buffer or regular vector as an integer.
(define (byte-at v i)
  (let ((x (vector-ref v i)))
    (if (integer? x) x (char->integer x))))

(test-suite "Packet schema variable-length - shape validation")

(assert-error (schema-validate '((x rest 3)))
              "'rest takes no arg")
(assert-error (schema-validate '((x rest) (y u8)))
              "field after 'rest")
(assert-error (schema-validate '((a bit) (b rest)))
              "'rest preceded by mid-byte fixed")
(assert-error (schema-validate '((x length-prefixed)))
              "'length-prefixed missing prefix type")
(assert-error (schema-validate '((x length-prefixed bit)))
              "'length-prefixed unsupported prefix")

(assert-true (schema-validate '((id u16) (payload rest)))
             "valid 'rest schema")
(assert-true (schema-validate '((tag u8) (val length-prefixed u8)))
             "valid 'length-prefixed schema")
(assert-true (schema-validate '((val length-prefixed u8) (crc u16)))
             "fixed-after-length-prefixed is allowed")

(test-suite "Packet schema variable-length - construct/deconstruct")

(define rest-schema '((id u16) (payload rest)))

;; rest round-trip preserves bytes.
(let* ((buf (schema-construct rest-schema
              (list (cons 'id #xabcd)
                    (cons 'payload (vector 1 2 3 4 5)))))
       (out (schema-deconstruct rest-schema buf))
       (payload (cdr (assq 'payload out))))
  (assert-equal 7 (vector-length buf) "rest buffer length = id (2) + payload (5)")
  (assert-equal #xabcd (cdr (assq 'id out)) "rest id round-trip")
  (assert-equal 5 (vector-length payload) "rest payload length")
  (assert-equal 1 (byte-at payload 0) "rest payload byte 0")
  (assert-equal 5 (byte-at payload 4) "rest payload byte 4"))

;; rest with empty payload.
(let* ((buf (schema-construct rest-schema
              (list (cons 'id 42)
                    (cons 'payload (vector)))))
       (out (schema-deconstruct rest-schema buf))
       (payload (cdr (assq 'payload out))))
  (assert-equal 2 (vector-length buf) "rest-empty buffer length = id only")
  (assert-equal 42 (cdr (assq 'id out)) "rest-empty id")
  (assert-equal 0 (vector-length payload) "rest-empty payload length"))

;; length-prefixed u8 round-trip.
(let* ((schema '((tag u8) (label length-prefixed u8) (crc u16)))
       (buf (schema-construct schema
              (list (cons 'tag #xa5)
                    (cons 'label (vector #x77 #x77 #x77))
                    (cons 'crc #x1234))))
       (out (schema-deconstruct schema buf))
       (label (cdr (assq 'label out))))
  (assert-equal 7 (vector-length buf) "lpb buffer length")
  (assert-equal #xa5  (byte-at buf 0) "lpb byte 0: tag")
  (assert-equal 3     (byte-at buf 1) "lpb byte 1: u8 prefix = 3")
  (assert-equal #x77  (byte-at buf 2) "lpb byte 2: label[0]")
  (assert-equal #x12  (byte-at buf 5) "lpb byte 5: crc hi")
  (assert-equal #x34  (byte-at buf 6) "lpb byte 6: crc lo")
  (assert-equal #x1234 (cdr (assq 'crc out)) "lpb crc round-trip")
  (assert-equal 3 (vector-length label) "lpb label length"))

;; length-prefixed u16 supports payloads > 255 bytes.
(let* ((payload (make-buffer 500)))
  (buffer-append payload 0 #x11)
  (buffer-append payload 499 #x22)
  (let* ((schema '((data length-prefixed u16)))
         (buf (schema-construct schema (b 'data payload)))
         (out (schema-deconstruct schema buf))
         (data (cdr (assq 'data out))))
    (assert-equal 502 (vector-length buf) "lpb-u16 length = 2 prefix + 500 data")
    (assert-equal 500 (vector-length data) "lpb-u16 data length")
    (assert-equal #x11 (byte-at data 0)   "lpb-u16 data byte 0")
    (assert-equal #x22 (byte-at data 499) "lpb-u16 data byte 499")))

(test-suite "Packet schema variable-length - error cases")

(assert-error (schema-construct '((x length-prefixed u8))
                                (b 'x (make-vector 300 0)))
              "length-prefixed u8 value too long for prefix")
(assert-error (schema-construct rest-schema
                                (list (cons 'id 0) (cons 'payload 42)))
              "rest with non-vector value")

;; A buffer whose prefix claims more bytes than are present.
(assert-error (let ((buf (make-buffer 3)))
                (buffer-append buf 0 5)
                (buffer-append buf 1 #x77)
                (buffer-append buf 2 #x77)
                (schema-deconstruct '((label length-prefixed u8)) buf))
              "length-prefixed prefix runs past buffer")

;; Extra bytes after the schema is consumed.
(assert-error (let ((buf (make-buffer 6)))
                (buffer-append buf 0 #x42)
                (buffer-append buf 1 2)
                (buffer-append buf 2 #x11)
                (buffer-append buf 3 #x22)
                (buffer-append buf 4 #x99)
                (buffer-append buf 5 #x99)
                (schema-deconstruct '((tag u8) (label length-prefixed u8)) buf))
              "trailing bytes after schema consumed")

(test-summary)
