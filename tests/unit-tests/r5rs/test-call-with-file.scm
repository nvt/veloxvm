;;; VeloxVM Unit Tests - R5RS call-with-input-file and call-with-output-file
;;; Tests for: R5RS §6.6.1 (File operations with automatic cleanup)

(include "../unit-test-framework.scm")

;; Define the functions inline
(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (let ((value (proc port)))
      (close-input-port port)
      value)))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (let ((value (proc port)))
      (close-output-port port)
      value)))

(test-suite "call-with-*-file Functions")

;; Test 1: Basic write and return value
(define write-result
  (call-with-output-file "/tmp/velox-test.txt"
    (lambda (port)
      (write-char #\A port)
      (write-char #\B port)
      42)))

(assert-equal 42 write-result "call-with-output-file returns procedure result")

;; Test 2: Basic read
(define char1
  (call-with-input-file "/tmp/velox-test.txt"
    (lambda (port)
      (read-char port))))

(assert-equal #\A char1 "Read first char")

;; Test 3: Write then read
(call-with-output-file "/tmp/velox-test2.txt"
  (lambda (port)
    (write-char #\X port)
    #t))

(define char2
  (call-with-input-file "/tmp/velox-test2.txt"
    (lambda (port)
      (read-char port))))

(assert-equal #\X char2 "Write then read workflow")

(test-summary)
