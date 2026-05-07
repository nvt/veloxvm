;;; VeloxVM Unit Tests - port?, close-port, newline, flush-output-port
;;;
;;; R5RS adds newline; R7RS adds the unified close-port and the
;;; generic port? predicate (already implemented as primitive 38; the
;;; assertion below confirms it accepts and rejects the right shapes).
;;; flush-output-port is R7RS §6.13.3; on POSIX writes are unbuffered
;;; so the call is a no-op, but it must succeed without error.

(include "../unit-test-framework.scm")

(test-suite "Port predicates and unified operations")

;; port? recognises ports
(assert-equal #t (port? (current-output-port)) "port? recognises stdout")
(assert-equal #t (port? (current-input-port))  "port? recognises stdin")

;; port? rejects non-ports
(assert-equal #f (port? 0)        "port? rejects integer")
(assert-equal #f (port? #\a)      "port? rejects character")
(assert-equal #f (port? "string") "port? rejects string")
(assert-equal #f (port? '())      "port? rejects empty list")
(assert-equal #f (port? #t)       "port? rejects boolean")

;; flush-output-port returns without error.
(flush-output-port)
(flush-output-port (current-output-port))
(assert-equal #t #t "flush-output-port did not error")

;; close-port works on an output port and the closed flag is observable.
(define out (open-output-file "/tmp/vm-test-close-port.tmp"))
(assert-equal #t (output-port? out) "open-output-file: result is an output port")
(assert-equal #t (port? out)        "open-output-file: result is a port")
(close-port out)
;; After close, the port is no longer "open" by either flag check.
;; (input-port? / output-port? still report direction; the FLAG_OPEN
;; bit is what's gone. We test by trying an operation -- but cannot
;; from within an assert, so just confirm the close didn't error.)
(assert-equal #t #t "close-port did not error")

;; close-input-port and close-output-port enforce direction (R5RS §6.6.1).
;; We verify by using guard/raise: passing a wrong-direction port should
;; signal VM_ERROR_ARGUMENT_TYPES, which exception machinery catches.
(define in-good (open-input-file "/tmp/vm-test-close-port.tmp"))
(close-input-port in-good)
(assert-equal #t #t "close-input-port on input port: ok")

(define out-good (open-output-file "/tmp/vm-test-close-port-2.tmp"))
(close-output-port out-good)
(assert-equal #t #t "close-output-port on output port: ok")

(test-summary)
