(include "packet-schema.scm")

;; Lightweight expect-error harness using the VM's exception machinery:
;; (with-exception-handler) lets us catch the alist-tagged errors thrown
;; by pkt-error.

(define *passed* 0)
(define *failed* 0)

(define (expect-ok name thunk)
  (guard (exn (else
               (set! *failed* (+ *failed* 1))
               (display "FAIL: ") (display name)
               (display " — got error: ") (display exn) (newline)))
    (thunk)
    (set! *passed* (+ *passed* 1))
    (display "ok:   ") (display name) (newline)))

(define (expect-fail name thunk)
  (guard (exn (else
               (set! *passed* (+ *passed* 1))
               (display "ok:   ") (display name)
               (display " — rejected (") (display (cadr exn)) (display ")")
               (newline)))
    (thunk)
    (set! *failed* (+ *failed* 1))
    (display "FAIL: ") (display name)
    (display " — call unexpectedly succeeded") (newline)))

(define (b1 name val) (list (cons name val)))
(define (b2 n1 v1 n2 v2) (list (cons n1 v1) (cons n2 v2)))

;; --- Schema validation: shape errors ------------------------------------

(expect-fail "empty schema"
  (lambda () (schema-validate '())))

(expect-fail "non-list schema"
  (lambda () (schema-validate 42)))

(expect-fail "spec not a list"
  (lambda () (schema-validate '(foo))))

(expect-fail "name not a symbol"
  (lambda () (schema-validate '(("x" u8)))))

(expect-fail "unknown type"
  (lambda () (schema-validate '((x glorp)))))

(expect-fail "bits without width"
  (lambda () (schema-validate '((x bits)))))

(expect-fail "bits too wide"
  (lambda () (schema-validate '((x bits 9)))))

(expect-fail "bits zero width"
  (lambda () (schema-validate '((x bits 0)))))

(expect-fail "bytes missing length"
  (lambda () (schema-validate '((x bytes)))))

(expect-fail "u8 with stray arg"
  (lambda () (schema-validate '((x u8 4)))))

(expect-fail "total width not multiple of 8"
  (lambda () (schema-validate '((x bit) (y bits 3)))))

(expect-fail "multi-byte field not byte-aligned"
  ;; bit (1) + u8 (8) would put u8 at bit 1 — not byte-aligned.
  (lambda () (schema-validate '((a bit) (b u8) (c bits 7)))))

(expect-ok "small valid schema"
  (lambda () (schema-validate '((a u8) (b u16) (c bit) (d bits 7)))))

(expect-fail "duplicate field name"
  (lambda () (schema-validate '((a u8) (a u8)))))

;; --- Binding-shape errors -----------------------------------------------

(expect-fail "bindings not a list"
  (lambda () (schema-construct '((a u8)) 42)))

(expect-fail "binding not a pair"
  (lambda () (schema-construct '((a u8)) (list 'oops))))

(expect-fail "duplicate binding"
  (lambda () (schema-construct '((a u8) (b u8))
                               (list (cons 'a 1) (cons 'a 2) (cons 'b 3)))))

(expect-fail "extra binding not in schema"
  (lambda () (schema-construct '((a u8))
                               (list (cons 'a 1) (cons 'extra 2)))))

;; --- Deconstruct buffer-shape errors -----------------------------------

(expect-fail "deconstruct non-buffer"
  (lambda () (schema-deconstruct '((a u8)) 42)))

(expect-fail "deconstruct regular vector (not a buffer)"
  (lambda () (schema-deconstruct '((a u8)) (vector 0))))

(expect-fail "deconstruct wrong-length buffer"
  (lambda ()
    ;; Build a 1-byte buffer from a schema, then try to read it back through
    ;; a 2-byte schema.
    (let ((buf (schema-construct '((a u8)) (b1 'a 1))))
      (schema-deconstruct '((a u8) (b u8)) buf))))

;; --- Value validation: range errors -------------------------------------

(define u8-schema '((x u8)))
(define s8-schema '((x s8)))
(define bit3-schema '((x bits 3) (pad bits 5)))
(define bytes4-schema '((x bytes 4)))

(expect-fail "u8 too large"
  (lambda () (schema-construct u8-schema (b1 'x 300))))

(expect-fail "u8 negative"
  (lambda () (schema-construct u8-schema (b1 'x -1))))

(expect-fail "u8 non-integer"
  (lambda () (schema-construct u8-schema (b1 'x "hi"))))

(expect-fail "s8 underflow"
  (lambda () (schema-construct s8-schema (b1 'x -129))))

(expect-fail "s8 overflow"
  (lambda () (schema-construct s8-schema (b1 'x 128))))

(expect-fail "bits 3 overflow"
  (lambda () (schema-construct bit3-schema (b2 'x 8 'pad 0))))

(expect-fail "bytes wrong length"
  (lambda () (schema-construct bytes4-schema (b1 'x (vector 1 2 3)))))

(expect-fail "bytes not a vector"
  (lambda () (schema-construct bytes4-schema (b1 'x 42))))

(expect-fail "byte value out of 0..255"
  (lambda () (schema-construct bytes4-schema (b1 'x (vector 1 2 300 4)))))

(expect-fail "missing binding"
  (lambda () (schema-construct '((a u8) (b u8)) (b1 'a 1))))

;; --- Happy paths --------------------------------------------------------

(expect-ok "u8 at limits"
  (lambda ()
    (schema-construct u8-schema (b1 'x 0))
    (schema-construct u8-schema (b1 'x 255))))

(expect-ok "s16 at limits"
  (lambda ()
    (schema-construct '((x s16)) (b1 'x -32768))
    (schema-construct '((x s16)) (b1 'x 32767))))

(expect-ok "bytes 4 round-trip"
  (lambda ()
    (let* ((buf (schema-construct bytes4-schema
                                  (b1 'x (vector 1 2 3 4))))
           (out (schema-deconstruct bytes4-schema buf)))
      (unless (equal? (cdr (assq 'x out)) (vector 1 2 3 4))
        (pkt-error "bytes 4 round-trip wrong" (cdr (assq 'x out)))))))

(display "---") (newline)
(display "passed: ") (display *passed*) (newline)
(display "failed: ") (display *failed*) (newline)
