;;; VeloxVM Unit Tests - R7RS string ports
;;; R7RS §6.13.1, §6.13.3:
;;;   (open-input-string str)         -> input port
;;;   (open-output-string)            -> output port
;;;   (get-output-string port)        -> string

(include "../unit-test-framework.scm")

(test-suite "R7RS string ports")

;; --- Input port basics

(define ip (open-input-string "abc"))
(assert-equal #t (port? ip)        "open-input-string returns a port")
(assert-equal #t (input-port? ip)  "input port is an input port")
(assert-equal #f (output-port? ip) "input port is not an output port")

;; Reading 'abc' character by character then EOF.
(assert-equal 97  (char->integer (read-char ip)) "read-char returns 'a'")
(assert-equal 98  (char->integer (read-char ip)) "read-char returns 'b'")
(assert-equal 99  (char->integer (read-char ip)) "read-char returns 'c'")
(assert-equal #t  (eof-object? (read-char ip))   "read past end returns EOF")
;; Sticky EOF: subsequent reads keep returning EOF.
(assert-equal #t  (eof-object? (read-char ip))   "EOF is sticky")

;; peek-char on a string port doesn't advance position.
(define ip2 (open-input-string "xy"))
(assert-equal 120 (char->integer (peek-char ip2)) "peek-char returns 'x'")
(assert-equal 120 (char->integer (peek-char ip2)) "second peek still 'x'")
(assert-equal 120 (char->integer (read-char ip2)) "read-char now consumes 'x'")
(assert-equal 121 (char->integer (read-char ip2)) "next read returns 'y'")

;; Empty input string immediately EOF.
(define ip-empty (open-input-string ""))
(assert-equal #t (eof-object? (read-char ip-empty)) "empty source -> EOF")

;; --- Output port basics

(define op (open-output-string))
(assert-equal #t (port? op)        "open-output-string returns a port")
(assert-equal #t (output-port? op) "output port is an output port")
(assert-equal #f (input-port? op)  "output port is not an input port")

;; Writes accumulate; get-output-string snapshots without closing.
(display "hello" op)
(display " " op)
(display 42 op)
(assert-equal "hello 42" (get-output-string op) "first snapshot")

;; Subsequent writes append to the same buffer.
(display "!" op)
(assert-equal "hello 42!" (get-output-string op) "second snapshot includes new write")

;; write-char and newline both work via the io->write callback.
(define op2 (open-output-string))
(write-char #\A op2)
(write-char #\B op2)
(newline op2)
(write-char #\C op2)
(assert-equal "AB\nC" (get-output-string op2) "write-char and newline accumulate")

;; --- Larger payloads (exercises the >127-byte string widening)
(define op-big (open-output-string))
(let loop ((i 0))
  (when (< i 200)
    (write-char #\x op-big)
    (loop (+ i 1))))
(assert-equal 200 (string-length (get-output-string op-big))
              "200-character output exceeds the old int8_t cap")

(test-summary)
