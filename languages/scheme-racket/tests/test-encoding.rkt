#lang racket

;; Simple test script for bytecode encoding (without rackunit)

(require "../bytecode.rkt")
(require "../primitives.rkt")

(displayln "Testing bytecode encoding functions...\n")

;; Test boolean encoding
(displayln "Test 1: Boolean encoding")
(define bool-true (encode-boolean #t))
(define bool-false (encode-boolean #f))
(displayln (format "  encode-boolean #t:  ~a" bool-true))
(displayln (format "  encode-boolean #f:  ~a" bool-false))
(displayln (format "  Expected: type=~a, info bits differ" VM-TYPE-BOOLEAN))

;; Test integer encoding
(displayln "\nTest 2: Integer encoding")
(define int42 (encode-integer 42))
(define int-5 (encode-integer -5))
(define int1000 (encode-integer 1000))
(displayln (format "  encode-integer 42:    ~a" int42))
(displayln (format "  encode-integer -5:    ~a" int-5))
(displayln (format "  encode-integer 1000:  ~a" int1000))
(displayln (format "  Expected: type=~a, varying lengths" VM-TYPE-INTEGER))

;; Test character encoding
(displayln "\nTest 3: Character encoding")
(define char-a (encode-character #\a))
(define char-newline (encode-character #\newline))
(displayln (format "  encode-character #\\a:       ~a" char-a))
(displayln (format "  encode-character #\\newline: ~a" char-newline))
(displayln (format "  Expected: type=~a" VM-TYPE-CHARACTER))

;; Test real encoding
(displayln "\nTest 4: Real number encoding")
(define real-pi (encode-real 3.14))
(displayln (format "  encode-real 3.14: ~a" real-pi))
(displayln (format "  Expected: type=~a, 4 bytes for float" VM-TYPE-REAL))

;; Test bytecode structure
(displayln "\nTest 5: Bytecode container")
(define bc (make-bytecode))
(displayln (format "  Empty bytecode: ~a" bc))
(define idx1 (add-string bc "hello"))
(define idx2 (add-string bc "world"))
(define idx3 (add-string bc "hello"))  ; Duplicate
(displayln (format "  Added 'hello' at index: ~a" idx1))
(displayln (format "  Added 'world' at index: ~a" idx2))
(displayln (format "  Added 'hello' again:    ~a (should reuse)" idx3))
(displayln (format "  Strings table: ~a" (bytecode-strings bc)))

(define sym1 (add-symbol bc 'foo))
(define sym2 (add-symbol bc 'bar))
(define sym3 (add-symbol bc 'foo))  ; Duplicate
(displayln (format "  Added 'foo at index: ~a" sym1))
(displayln (format "  Added 'bar at index: ~a" sym2))
(displayln (format "  Added 'foo again:    ~a (should reuse)" sym3))
(displayln (format "  Symbols table: ~a" (bytecode-symbols bc)))

(displayln "\nAll encoding tests completed!")
