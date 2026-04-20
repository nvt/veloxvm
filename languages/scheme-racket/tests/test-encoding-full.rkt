#lang racket

;; Test updated encoding functions

(require "../bytecode.rkt")
(require "../primitives.rkt")

(displayln "Testing updated encoding functions...\n")

;; Test rational encoding
(displayln "Test: Rational encoding")
(define rat1 (encode-rational 3/4))
(displayln (format "  encode-rational 3/4:  ~a" rat1))
(displayln (format "  Bytes: ~a" (expr-encoding-data rat1)))

(define rat2 (encode-rational -5/7))
(displayln (format "  encode-rational -5/7: ~a" rat2))
(displayln (format "  Bytes: ~a" (expr-encoding-data rat2)))

;; Test string encoding with bytecode container
(displayln "\nTest: String encoding")
(define bc (make-bytecode))
(define str1 (encode-string "hello" bc))
(define str2 (encode-string "world" bc))
(displayln (format "  encode-string 'hello': ~a" str1))
(displayln (format "  Bytes: ~a" (expr-encoding-data str1)))
(displayln (format "  encode-string 'world': ~a" str2))
(displayln (format "  Bytes: ~a" (expr-encoding-data str2)))
(displayln (format "  String table: ~a" (bytecode-strings bc)))

;; Test symbol encoding
(displayln "\nTest: Symbol encoding")
(define bc2 (make-bytecode))
(define sym1 (encode-symbol '+ bc2 #:scope 0))  ; Core symbol
(define sym2 (encode-symbol 'my-var bc2 #:scope 1))  ; App symbol
(displayln (format "  encode-symbol '+ (core):    ~a" sym1))
(displayln (format "  Bytes: ~a" (expr-encoding-data sym1)))
(displayln (format "  encode-symbol 'my-var (app): ~a" sym2))
(displayln (format "  Bytes: ~a" (expr-encoding-data sym2)))
(displayln (format "  Symbol table: ~a" (bytecode-symbols bc2)))

(displayln "\nAll encoding tests completed!")
