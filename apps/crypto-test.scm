;;; Test program for the crypto library
;;;
;;; Covers:
;;;   (crypto-hash 'sha-256 ...)
;;;   (crypto-mac / crypto-mac-verify 'hmac-sha-256 ...)
;;;   (crypto-aead-encrypt / crypto-aead-decrypt 'aes-128-ccm ...)

(import "crypto")

(define (print-hex-byte b)
  (let ((hex "0123456789abcdef")
        (byte (if (char? b) (char->integer b) b)))
    (display (string-ref hex (quotient byte 16)))
    (display (string-ref hex (remainder byte 16)))))

(define (print-hex vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (print-hex-byte (vector-ref vec i))
            (loop (+ i 1)))))))

;;; SHA-256

(display "=== SHA-256 ===") (newline)

(display "SHA-256('') = ")
(print-hex (crypto-hash 'sha-256 ""))
(newline)
;; Expected: e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855

(display "SHA-256('hello') = ")
(print-hex (crypto-hash 'sha-256 "hello"))
(newline)
;; Expected: 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824

;;; HMAC-SHA-256

(newline)
(display "=== HMAC-SHA-256 ===") (newline)

(let ((tag (crypto-mac 'hmac-sha-256 "key"
                       "The quick brown fox jumps over the lazy dog")))
  (display "tag = ")
  (print-hex tag)
  (newline)
  ;; Expected: f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8

  (display "verify correct tag: ")
  (display (crypto-mac-verify 'hmac-sha-256 "key"
                              "The quick brown fox jumps over the lazy dog"
                              tag))
  (newline)

  (display "verify wrong data: ")
  (display (crypto-mac-verify 'hmac-sha-256 "key" "tampered" tag))
  (newline))

;;; AEAD round-trip with AES-128-CCM

(newline)
(display "=== AES-128-CCM ===") (newline)

(let* ((key   (make-buffer 16))   ; make-buffer zero-fills
       (nonce (make-buffer 13))
       (aad   "header")
       (plaintext "secret message")
       (ciphertext (crypto-aead-encrypt 'aes-128-ccm
                                        key nonce aad plaintext)))
  (display "ciphertext length = ")
  (display (vector-length ciphertext))
  (newline)

  (let ((recovered (crypto-aead-decrypt 'aes-128-ccm
                                        key nonce aad ciphertext)))
    (display "round-trip decrypt: ")
    (display (if (equal? (string->list plaintext)
                         (vector->list recovered))
                 "PASS"
                 "FAIL"))
    (newline))

  ;; Tampering: flip one bit of the first ciphertext byte and verify
  ;; decrypt rejects it.
  (let ((tampered (make-buffer (vector-length ciphertext))))
    (let loop ((i 0))
      (if (< i (vector-length ciphertext))
          (begin
            (vector-set! tampered i (vector-ref ciphertext i))
            (loop (+ i 1)))))
    (vector-set! tampered 0
                 (bit-xor 1 (char->integer (vector-ref tampered 0))))
    (let ((result (crypto-aead-decrypt 'aes-128-ccm
                                       key nonce aad tampered)))
      (display "tampered decrypt rejected: ")
      (display (not result))
      (newline))))

(newline)
(display "=== done ===") (newline)
