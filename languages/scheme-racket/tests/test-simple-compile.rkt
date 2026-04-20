#lang racket

;; Test simple compilation

(require "../compiler.rkt")
(require "../bytecode.rkt")

(displayln "Testing simple expression compilation...\n")

;; Test compiling a simple arithmetic expression
(displayln "Test: (+ 1 2)")
(define bc1 (make-bytecode))
(define result1 (compile-expr '(+ 1 2) bc1))
(displayln (format "  Result: ~a" result1))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc1)))
(displayln (format "  Expressions: ~a" (bytecode-expressions bc1)))

;; Test compiling quoted symbol
(displayln "\nTest: (quote foo)")
(define bc2 (make-bytecode))
(define result2 (compile-expr '(quote foo) bc2))
(displayln (format "  Result: ~a" result2))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc2)))

(displayln "\nSimple compilation tests completed!")
