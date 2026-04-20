#lang racket

;; Test lambda compilation

(require "../compiler.rkt")
(require "../bytecode.rkt")

(displayln "Testing lambda compilation...\n")

;; Test simple lambda
(displayln "Test 1: Simple lambda (lambda (x) x)")
(define bc1 (make-bytecode))
(define result1 (compile-expr '(lambda (x) x) bc1))
(displayln (format "  Result: ~a" result1))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc1)))
(displayln (format "  Expressions count: ~a" (length (bytecode-expressions bc1))))

;; Test lambda with arithmetic
(displayln "\nTest 2: Lambda with arithmetic (lambda (x y) (+ x y))")
(define bc2 (make-bytecode))
(define result2 (compile-expr '(lambda (x y) (+ x y)) bc2))
(displayln (format "  Result: ~a" result2))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc2)))
(displayln (format "  Expressions count: ~a" (length (bytecode-expressions bc2))))

;; Test lambda with multiple body expressions
(displayln "\nTest 3: Lambda with multiple body (lambda (x) (print x) x)")
(define bc3 (make-bytecode))
(define result3 (compile-expr '(lambda (x) (print x) x) bc3))
(displayln (format "  Result: ~a" result3))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc3)))
(displayln (format "  Expressions count: ~a" (length (bytecode-expressions bc3))))

;; Test define with lambda
(displayln "\nTest 4: Define with lambda (define square (lambda (x) (* x x)))")
(define bc4 (make-bytecode))
(define result4 (compile-expr '(define square (lambda (x) (* x x))) bc4))
(displayln (format "  Result: ~a" result4))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc4)))
(displayln (format "  Expressions count: ~a" (length (bytecode-expressions bc4))))

;; Test function definition syntax sugar
(displayln "\nTest 5: Function definition syntax (define (square x) (* x x))")
(define bc5 (make-bytecode))
(define result5 (compile-expr '(define (square x) (* x x)) bc5))
(displayln (format "  Result: ~a" result5))
(displayln (format "  Symbols: ~a" (bytecode-symbols bc5)))
(displayln (format "  Expressions count: ~a" (length (bytecode-expressions bc5))))

(displayln "\nLambda compilation tests completed!")
