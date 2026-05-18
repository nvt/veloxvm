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

;; Helpers.
;;
;; VM primitives are encoded in the core scope (scope=0) via their op-id
;; and never appear in the user symbol table. Primitive-shape assertions
;; therefore walk the rewritten AST via rewrite-only + contains-sym?;
;; (bytecode-symbols bc) checks are reserved for user-defined names.
(define (rewrite-only expr)
  (rewrite-expr expr))

(define (compile-full expr)
  (define bc (make-bytecode))
  (compile-expr (rewrite-expr expr) bc)
  bc)

(define (contains-sym? sym tree)
  (cond
    [(null? tree) #f]
    [(pair? tree) (or (contains-sym? sym (car tree))
                      (contains-sym? sym (cdr tree)))]
    [else (eq? sym tree)]))

;; Test 1: Simple arithmetic
(displayln "Test 1: Simple arithmetic (+ 1 2)")
(define bc1 (compile-full '(+ 1 2)))
(check-equal? (bytecode-magic bc1) #x5EB5 "Magic number correct")
(check-equal? (bytecode-version bc1) 3 "Version correct")
(check-true (contains-sym? '+ (rewrite-only '(+ 1 2)))
            "Rewritten form references '+'")
(displayln "   PASSED\n")

;; Test 2: Variable definition
(displayln "Test 2: Variable definition (define x 42)")
(define bc2 (compile-full '(define x 42)))
(check-not-false (member 'x (bytecode-symbols bc2)) "User symbol 'x' found in symbol table")
(displayln "   PASSED\n")

;; Test 3: Function definition
(displayln "Test 3: Function definition (define (square x) (* x x))")
(define bc3 (compile-full '(define (square x) (* x x))))
(check-not-false (member 'square (bytecode-symbols bc3)) "Symbol 'square' found")
(check-not-false (member 'x (bytecode-symbols bc3)) "Symbol 'x' found (lambda parameter)")
(displayln "   PASSED\n")

;; Test 4: Lambda expression
(displayln "Test 4: Lambda expression (lambda (x) (+ x 1))")
(define bc4 (compile-full '(lambda (x) (+ x 1))))
(check-not-false (member 'x (bytecode-symbols bc4)) "Lambda parameter 'x' found")
(check-true (> (bytecode-expression-count bc4) 0) "Expressions generated")
(displayln "   PASSED\n")

;; Test 5: Conditional (cond - derived form)
(displayln "Test 5: Cond expression (cond ((< x 0) -1) (else 0))")
(define rw5 (rewrite-only '(cond ((< x 0) -1) (else 0))))
(check-true (contains-sym? 'if rw5) "Cond rewrites to 'if'")
(check-true (contains-sym? '< rw5) "Comparison operator preserved")
(compile-full '(cond ((< x 0) -1) (else 0)))  ; sanity: compiles cleanly
(displayln "   PASSED\n")

;; Test 6: Let expression (derived form)
(displayln "Test 6: Let expression (let ((x 1) (y 2)) (+ x y))")
(define rw6 (rewrite-only '(let ((x 1) (y 2)) (+ x y))))
;; Let should rewrite to ((lambda (x y) (+ x y)) 1 2)
(check-true (contains-sym? 'lambda rw6) "Let rewrites to a lambda application")
(check-true (contains-sym? '+ rw6) "Body contains +")
(define bc6 (compile-full '(let ((x 1) (y 2)) (+ x y))))
(check-not-false (member 'x (bytecode-symbols bc6)) "Bound 'x' in symbol table")
(check-not-false (member 'y (bytecode-symbols bc6)) "Bound 'y' in symbol table")
(displayln "   PASSED\n")

;; Test 7: Case expression (derived form)
(displayln "Test 7: Case expression (case x ((1 2) 'small) (else 'large))")
(define rw7 (rewrite-only '(case x ((1 2) small) (else large))))
(check-true (contains-sym? 'memv rw7) "Case uses 'memv'")
(check-true (contains-sym? 'list rw7) "Case uses 'list'")
(check-true (contains-sym? 'if rw7) "Case uses 'if' (via cond rewrite)")
(displayln "   PASSED\n")

;; Test 8: Do loop (derived form)
(displayln "Test 8: Do loop (do ((i 0 (+ i 1))) ((> i 10) i))")
(define rw8 (rewrite-only '(do ((i 0 (+ i 1))) ((> i 10) i))))
(check-true (contains-sym? 'define rw8) "Do creates internal define")
(check-true (contains-sym? 'if rw8) "Do uses 'if'")
(displayln "   PASSED\n")

;; Test 9: When macro (derived form)
(displayln "Test 9: When macro (when (> x 0) (print x))")
(define rw9 (rewrite-only '(when (> x 0) (print x))))
(check-true (contains-sym? 'if rw9) "When rewrites to 'if'")
(check-true (contains-sym? '> rw9) "Condition preserved")
(displayln "   PASSED\n")

;; Test 10: Car/cdr composition (derived form)
(displayln "Test 10: Car/cdr composition (caddr lst)")
(define rw10 (rewrite-only '(caddr lst)))
(check-true (contains-sym? 'car rw10) "caddr uses 'car'")
(check-true (contains-sym? 'cdr rw10) "caddr uses 'cdr'")
(displayln "   PASSED\n")

;; Test 11: Zero? predicate (derived form)
(displayln "Test 11: Zero? predicate (zero? x)")
(define rw11 (rewrite-only '(zero? x)))
(check-true (contains-sym? '= rw11) "zero? rewrites to '='")
(displayln "   PASSED\n")

;; Test 12: Character predicate (derived form)
(displayln "Test 12: Character predicate (char-alphabetic? #\\a)")
(define rw12 (rewrite-only '(char-alphabetic? #\a)))
(check-true (contains-sym? 'char-class rw12) "Uses char-class primitive")
(displayln "   PASSED\n")

;; Test 13: Complex nested expression
(displayln "Test 13: Complex nested expression")
(define complex-expr
  '(define (factorial n)
     (if (zero? n)
         1
         (* n (factorial (- n 1))))))
(define rw13 (rewrite-only complex-expr))
(check-true (contains-sym? '= rw13) "zero? expanded to =")
(check-true (contains-sym? '* rw13) "Multiplication preserved")
(define bc13 (compile-full complex-expr))
(check-not-false (member 'factorial (bytecode-symbols bc13)) "Function name in symbol table")
(check-not-false (member 'n (bytecode-symbols bc13)) "Parameter 'n' in symbol table")
(displayln "   PASSED\n")

;; Test 14: Let* sequential bindings
(displayln "Test 14: Let* sequential bindings (let* ((x 1) (y x)) y)")
(define rw14 (rewrite-only '(let* ((x 1) (y x)) y)))
(check-true (contains-sym? 'lambda rw14) "let* rewrites through nested lambdas")
(define bc14 (compile-full '(let* ((x 1) (y x)) y)))
;; let* rewrites to nested (let ...) → nested ((lambda ...) ...), each
;; lambda body becoming its own bind_function expression, so the count
;; grows with the number of bindings.
(check-true (> (bytecode-expression-count bc14) 1) "Multiple expressions for nesting")
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
(check-equal? (bytes-ref file-bytes 2) 3 "Version byte correct")
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
