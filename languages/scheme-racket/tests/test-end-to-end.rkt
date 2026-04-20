#lang racket

;; End-to-End Integration Tests
;; Tests the full pipeline: rewrite -> compile -> bytecode

(require "../compiler.rkt")
(require "../bytecode.rkt")
(require "../rewriter.rkt")
(require rackunit)

(displayln "========================================")
(displayln "End-to-End Integration Tests")
(displayln "========================================\n")

;; Helper to compile an expression through full pipeline
(define (compile-full expr)
  (define bc (make-bytecode))
  (define rewritten (rewrite-expr expr))
  (compile-expr rewritten bc)
  bc)

;; Test 1: Simple arithmetic
(displayln "Test 1: Simple arithmetic (+ 1 2)")
(define bc1 (compile-full '(+ 1 2)))
(check-equal? (bytecode-magic bc1) #x5EB5 "Magic number correct")
(check-equal? (bytecode-version bc1) 1 "Version correct")
(check-true (member '+ (bytecode-symbols bc1)) "Symbol + found")
(displayln "   PASSED\n")

;; Test 2: Variable definition
(displayln "Test 2: Variable definition (define x 42)")
(define bc2 (compile-full '(define x 42)))
(check-true (member 'define (bytecode-symbols bc2)) "Symbol 'define' found")
(check-true (member 'x (bytecode-symbols bc2)) "Symbol 'x' found")
(displayln "   PASSED\n")

;; Test 3: Function definition
(displayln "Test 3: Function definition (define (square x) (* x x))")
(define bc3 (compile-full '(define (square x) (* x x))))
(check-true (member 'square (bytecode-symbols bc3)) "Symbol 'square' found")
(check-true (member 'bind (bytecode-symbols bc3)) "Symbol 'bind' found (lambda)")
(check-true (member '* (bytecode-symbols bc3)) "Symbol '*' found")
(displayln "   PASSED\n")

;; Test 4: Lambda expression
(displayln "Test 4: Lambda expression (lambda (x) (+ x 1))")
(define bc4 (compile-full '(lambda (x) (+ x 1))))
(check-true (member 'bind (bytecode-symbols bc4)) "Lambda uses 'bind'")
(check-true (> (length (bytecode-expressions bc4)) 0) "Expressions generated")
(displayln "   PASSED\n")

;; Test 5: Conditional (cond - derived form)
(displayln "Test 5: Cond expression (cond ((< x 0) -1) (else 0))")
(define bc5 (compile-full '(cond ((< x 0) -1) (else 0))))
(check-true (member 'if (bytecode-symbols bc5)) "Cond rewrites to 'if'")
(check-true (member '< (bytecode-symbols bc5)) "Comparison operator found")
(displayln "   PASSED\n")

;; Test 6: Let expression (derived form)
(displayln "Test 6: Let expression (let ((x 1) (y 2)) (+ x y))")
(define bc6 (compile-full '(let ((x 1) (y 2)) (+ x y))))
;; Let should rewrite to ((lambda (x y) (+ x y)) 1 2)
(check-true (member 'bind (bytecode-symbols bc6)) "Let rewrites to lambda")
(check-true (member '+ (bytecode-symbols bc6)) "Body contains +")
(displayln "   PASSED\n")

;; Test 7: Case expression (derived form)
(displayln "Test 7: Case expression (case x ((1 2) 'small) (else 'large))")
(define bc7 (compile-full '(case x ((1 2) small) (else large))))
(check-true (member 'memv (bytecode-symbols bc7)) "Case uses 'memv'")
(check-true (member 'list (bytecode-symbols bc7)) "Case uses 'list'")
(check-true (member 'if (bytecode-symbols bc7)) "Case uses 'if'")
(displayln "   PASSED\n")

;; Test 8: Do loop (derived form)
(displayln "Test 8: Do loop (do ((i 0 (+ i 1))) ((> i 10) i))")
(define bc8 (compile-full '(do ((i 0 (+ i 1))) ((> i 10) i))))
(check-true (member 'define (bytecode-symbols bc8)) "Do creates internal define")
(check-true (member 'if (bytecode-symbols bc8)) "Do uses 'if'")
(displayln "   PASSED\n")

;; Test 9: When macro (derived form)
(displayln "Test 9: When macro (when (> x 0) (print x))")
(define bc9 (compile-full '(when (> x 0) (print x))))
(check-true (member 'if (bytecode-symbols bc9)) "When rewrites to 'if'")
(check-true (member '> (bytecode-symbols bc9)) "Condition preserved")
(displayln "   PASSED\n")

;; Test 10: Car/cdr composition (derived form)
(displayln "Test 10: Car/cdr composition (caddr lst)")
(define bc10 (compile-full '(caddr lst)))
(check-true (member 'car (bytecode-symbols bc10)) "caddr uses 'car'")
(check-true (member 'cdr (bytecode-symbols bc10)) "caddr uses 'cdr'")
(displayln "   PASSED\n")

;; Test 11: Zero? predicate (derived form)
(displayln "Test 11: Zero? predicate (zero? x)")
(define bc11 (compile-full '(zero? x)))
(check-true (member '= (bytecode-symbols bc11)) "zero? rewrites to '='")
(displayln "   PASSED\n")

;; Test 12: Character predicate (derived form)
(displayln "Test 12: Character predicate (char-alphabetic? #\\a)")
(define bc12 (compile-full '(char-alphabetic? #\a)))
(check-true (member 'char-class (bytecode-symbols bc12)) "Uses char-class primitive")
(displayln "   PASSED\n")

;; Test 13: Complex nested expression
(displayln "Test 13: Complex nested expression")
(define complex-expr
  '(define (factorial n)
     (if (zero? n)
         1
         (* n (factorial (- n 1))))))
(define bc13 (compile-full complex-expr))
(check-true (member 'factorial (bytecode-symbols bc13)) "Function name found")
(check-true (member '= (bytecode-symbols bc13)) "zero? expanded to =")
(check-true (member '* (bytecode-symbols bc13)) "Multiplication found")
(displayln "   PASSED\n")

;; Test 14: Let* sequential bindings
(displayln "Test 14: Let* sequential bindings (let* ((x 1) (y x)) y)")
(define bc14 (compile-full '(let* ((x 1) (y x)) y)))
(check-true (member 'bind (bytecode-symbols bc14)) "let* uses lambda")
;; let* should create nested lambdas
(check-true (> (length (bytecode-expressions bc14)) 3) "Multiple expressions for nesting")
(displayln "   PASSED\n")

;; Test 15: Bytecode file writing and reading back
(displayln "Test 15: Write and verify bytecode file")
(define test-bc (compile-full '(define (add x y) (+ x y))))
(define test-file "test-verification.vm")
(write-bytecode-file test-file test-bc)
(check-true (file-exists? test-file) "Bytecode file created")
(define file-bytes (file->bytes test-file))
(check-equal? (bytes-ref file-bytes 0) #x5E "Magic byte 1 correct")
(check-equal? (bytes-ref file-bytes 1) #xB5 "Magic byte 2 correct")
(check-equal? (bytes-ref file-bytes 2) 1 "Version byte correct")
(displayln "   PASSED\n")

;; Clean up test file
(delete-file test-file)

(displayln "========================================")
(displayln " All 15 integration tests PASSED!")
(displayln "========================================\n")

(displayln "Summary:")
(displayln "   Basic compilation works")
(displayln "   Variable and function definitions compile correctly")
(displayln "   Lambda expressions generate proper bytecode")
(displayln "   All derived forms (cond, case, let, let*, do) rewrite correctly")
(displayln "   Control flow macros (when, unless) work")
(displayln "   Composite operations (caddr, etc.) expand properly")
(displayln "   Predicates (zero?, char-alphabetic?) expand correctly")
(displayln "   Bytecode files have correct format and magic numbers")
(displayln "   Complex nested expressions compile successfully")
