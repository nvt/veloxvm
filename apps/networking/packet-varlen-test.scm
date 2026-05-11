(include "packet-schema.scm")

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

(define (b name val) (list (cons name val)))

;; equal? on byte buffers segfaults in this VM (vm_objects_deep_equal
;; indexes ->elements, which is NULL for buffer-flagged vectors). Compare
;; byte-by-byte instead. Accepts buffer or regular vector for either side.
(define (byte-at v i)
  (let ((x (vector-ref v i)))
    (if (integer? x) x (char->integer x))))

(define (bytes-equal? a b)
  (and (= (vector-length a) (vector-length b))
       (let loop ((i 0))
         (cond
           ((= i (vector-length a)) #t)
           ((= (byte-at a i) (byte-at b i)) (loop (+ i 1)))
           (else #f)))))

;; --- Schema validation: variable-field shape ---------------------------

(expect-fail "rest takes no arg"
  (lambda () (schema-validate '((x rest 3)))))

(expect-fail "field after rest"
  (lambda () (schema-validate '((x rest) (y u8)))))

(expect-fail "rest preceded by mid-byte fixed"
  (lambda () (schema-validate '((a bit) (b rest)))))

(expect-fail "length-prefixed missing prefix type"
  (lambda () (schema-validate '((x length-prefixed)))))

(expect-fail "length-prefixed unsupported prefix"
  (lambda () (schema-validate '((x length-prefixed bit)))))

(expect-ok "valid rest schema"
  (lambda () (schema-validate '((id u16) (payload rest)))))

(expect-ok "valid length-prefixed schema"
  (lambda () (schema-validate '((tag u8) (val length-prefixed u8)))))

(expect-ok "fixed after length-prefixed is allowed"
  (lambda () (schema-validate '((val length-prefixed u8) (crc u16)))))

;; --- Construct/deconstruct round-trips ---------------------------------

(define rest-schema '((id u16) (payload rest)))

(expect-ok "rest round-trip preserves bytes"
  (lambda ()
    (let* ((buf (schema-construct
                  rest-schema
                  (list (cons 'id #xabcd)
                        (cons 'payload (vector 1 2 3 4 5)))))
           (out (schema-deconstruct rest-schema buf)))
      (unless (and (= (cdr (assq 'id out)) #xabcd)
                   (bytes-equal? (cdr (assq 'payload out)) (vector 1 2 3 4 5)))
        (pkt-error "rest mismatch" out)))))

(expect-ok "rest with empty payload"
  (lambda ()
    (let* ((buf (schema-construct rest-schema
                                  (list (cons 'id 42)
                                        (cons 'payload (vector)))))
           (out (schema-deconstruct rest-schema buf)))
      (unless (and (= (cdr (assq 'id out)) 42)
                   (bytes-equal? (cdr (assq 'payload out)) (vector)))
        (pkt-error "rest-empty mismatch" out)))))

(define lpb-schema '((tag u8) (label length-prefixed u8) (crc u16)))

(expect-ok "length-prefixed u8 round-trip"
  (lambda ()
    (let* ((label (vector #x77 #x77 #x77))
           (buf (schema-construct
                  lpb-schema
                  (list (cons 'tag #xa5)
                        (cons 'label label)
                        (cons 'crc #x1234))))
           (out (schema-deconstruct lpb-schema buf)))
      ;; Wire form: a5 03 77 77 77 12 34
      (unless (and (= (vector-length buf) 7)
                   (bytes-equal? (cdr (assq 'label out)) label)
                   (= (cdr (assq 'crc out)) #x1234))
        (pkt-error "lpb mismatch" 'buf buf 'out out)))))

(expect-ok "length-prefixed u16 with payload > 255 bytes"
  (lambda ()
    ;; Build the payload as a byte buffer (not a regular vector) so the
    ;; schema layer can hand it through to the wire without an iterative
    ;; vector->buffer copy.
    (let ((payload (make-buffer 500)))
      (buffer-append payload 0 #x11)
      (buffer-append payload 499 #x22)
      (let* ((schema '((data length-prefixed u16)))
             (buf (schema-construct schema (b 'data payload)))
             (out (schema-deconstruct schema buf))
             (data (cdr (assq 'data out))))
        (unless (and (= (vector-length buf) 502)
                     (= (byte-at data 0) #x11)
                     (= (byte-at data 499) #x22))
          (pkt-error "lpb-u16 mismatch"))))))

;; --- Construct-time value errors ---------------------------------------

(expect-fail "length-prefixed u8 too long"
  (lambda ()
    (let ((schema '((x length-prefixed u8))))
      (schema-construct schema (b 'x (make-vector 300 0))))))

(expect-fail "rest with non-vector value"
  (lambda ()
    (schema-construct rest-schema (list (cons 'id 0) (cons 'payload 42)))))

;; --- Deconstruct-time errors -------------------------------------------

(expect-fail "length-prefixed runs past buffer"
  (lambda ()
    ;; Build a truncated buffer where the prefix claims more bytes than are
    ;; actually there.
    (let ((buf (make-buffer 3)))
      (buffer-append buf 0 5)   ;; prefix says 5 bytes follow
      (buffer-append buf 1 #x77)
      (buffer-append buf 2 #x77)
      (schema-deconstruct '((label length-prefixed u8)) buf))))

(expect-fail "trailing bytes after schema with length-prefixed"
  (lambda ()
    ;; tag + lpb(u8) = 1 + 1 + N. Buffer has extra bytes after.
    (let ((buf (make-buffer 6)))
      (buffer-append buf 0 #x42)   ;; tag
      (buffer-append buf 1 2)      ;; prefix says 2 bytes
      (buffer-append buf 2 #x11)
      (buffer-append buf 3 #x22)
      (buffer-append buf 4 #x99)   ;; <-- extra
      (buffer-append buf 5 #x99)
      (schema-deconstruct '((tag u8) (label length-prefixed u8)) buf))))

(display "---") (newline)
(display "passed: ") (display *passed*) (newline)
(display "failed: ") (display *failed*) (newline)
