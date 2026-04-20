#lang racket

;; VeloxVM Racket Compiler - Macro System Tests
;; Comprehensive tests for pattern matching, template expansion, and define-syntax

(require rackunit
         "../expander.rkt")

;; ============================================================================
;; Test 1: Basic Pattern Matching
;; ============================================================================

(printf "Test 1: Basic pattern matching...\n")

;; Test 1.1: Simple pattern variable
(let ([result (match-pattern 'x 42 '())])
  (check-equal? (hash-ref result 'x) 42 "Pattern variable should bind to value"))

;; Test 1.2: Literal matching
(let ([result (match-pattern 'if '(if #t 1 2) '(if))])
  (check-false result "Literal 'if' should not match entire form"))

(let ([result (match-pattern 'if 'if '(if))])
  (check-not-false result "Literal 'if' should match itself"))

;; Test 1.3: Pair pattern
(let ([result (match-pattern '(a b) '(1 2) '())])
  (check-equal? (hash-ref result 'a) 1)
  (check-equal? (hash-ref result 'b) 2))

;; Test 1.4: Nested pattern
(let ([result (match-pattern '(if test (then body)) '(if #t (then (+ 1 2))) '(if then))])
  (check-equal? (hash-ref result 'test) #t)
  (check-equal? (hash-ref result 'body) '(+ 1 2)))

;; ============================================================================
;; Test 2: Ellipsis Pattern Matching
;; ============================================================================

(printf "Test 2: Ellipsis pattern matching...\n")

;; Test 2.1: Simple ellipsis
(let ([result (match-pattern '(a ...) '(1 2 3) '())])
  (check-equal? (hash-ref result 'a) '(1 2 3) "Ellipsis should bind to list"))

;; Test 2.2: Ellipsis with prefix
(let ([result (match-pattern '(let x ...) '(let 1 2 3) '(let))])
  (check-equal? (hash-ref result 'x) '(1 2 3)))

;; Test 2.3: Multiple ellipsis variables
(let ([result (match-pattern '((var val) ...) '((x 1) (y 2) (z 3)) '())])
  (check-equal? (hash-ref result 'var) '(x y z))
  (check-equal? (hash-ref result 'val) '(1 2 3)))

;; Test 2.4: Empty ellipsis match
(let ([result (match-pattern '(begin e ...) '(begin) '(begin))])
  (check-equal? (hash-ref result 'e) '() "Empty ellipsis should bind to empty list"))

;; ============================================================================
;; Test 3: Template Expansion
;; ============================================================================

(printf "Test 3: Template expansion...\n")

;; Test 3.1: Simple substitution
(let ([bindings (make-hash '((x . 42) (y . 100)))])
  (check-equal? (expand-template '(+ x y) bindings) '(+ 42 100)))

;; Test 3.2: Nested substitution
(let ([bindings (make-hash '((test . #t) (body . (+ 1 2))))])
  (check-equal? (expand-template '(if test body #f) bindings)
                '(if #t (+ 1 2) #f)))

;; Test 3.3: Ellipsis expansion
(let ([bindings (make-hash)])
  (hash-set! bindings 'x '(1 2 3))
  (check-equal? (expand-template '(x ...) bindings) '(1 2 3)))

;; Test 3.4: Complex ellipsis expansion
(let ([bindings (make-hash)])
  (hash-set! bindings 'var '(x y z))
  (hash-set! bindings 'val '(1 2 3))
  (check-equal? (expand-template '((var val) ...) bindings)
                '((x 1) (y 2) (z 3))))

;; Test 3.5: Ellipsis in larger template
(let ([bindings (make-hash)])
  (hash-set! bindings 'e '(1 2 3))
  (check-equal? (expand-template '(begin e ... 4) bindings)
                '(begin 1 2 3 4)))

;; ============================================================================
;; Test 4: Simple Macros
;; ============================================================================

(printf "Test 4: Simple macros...\n")

;; Test 4.1: when macro
(reset-macro-table!)
(process-define-syntax 'when
  '(syntax-rules ()
     [(when test body ...)
      (if test (begin body ...))]))

(let ([form '(when (> x 0) (print "positive") (print "yes"))])
  (define transformer (hash-ref (macro-table) 'when))
  (define result (transformer form))
  (check-equal? result '(if (> x 0) (begin (print "positive") (print "yes")))))

;; Test 4.2: unless macro
(reset-macro-table!)
(process-define-syntax 'unless
  '(syntax-rules ()
     [(unless test body ...)
      (if (not test) (begin body ...))]))

(let ([form '(unless (< x 0) (print "non-negative"))])
  (define transformer (hash-ref (macro-table) 'unless))
  (define result (transformer form))
  (check-equal? result '(if (not (< x 0)) (begin (print "non-negative")))))

;; ============================================================================
;; Test 5: let Macro
;; ============================================================================

(printf "Test 5: let macro...\n")

(reset-macro-table!)
(process-define-syntax 'my-let
  '(syntax-rules ()
     [(my-let ([var val] ...) body ...)
      ((lambda (var ...) body ...) val ...)]))

(let ([form '(my-let ([x 1] [y 2]) (+ x y))])
  (define transformer (hash-ref (macro-table) 'my-let))
  (define result (transformer form))
  (check-equal? result '((lambda (x y) (+ x y)) 1 2)))

;; ============================================================================
;; Test 6: cond Macro (with literals)
;; ============================================================================

(printf "Test 6: cond macro with literals...\n")

(reset-macro-table!)
(process-define-syntax 'my-cond
  '(syntax-rules (else)
     [(my-cond [else result ...])
      (begin result ...)]
     [(my-cond [test result ...])
      (if test (begin result ...))]
     [(my-cond [test result ...] clause ...)
      (if test
          (begin result ...)
          (my-cond clause ...))]))

;; Test 6.1: cond with else
(let ([form '(my-cond [else 42])])
  (define transformer (hash-ref (macro-table) 'my-cond))
  (define result (transformer form))
  (check-equal? result '(begin 42)))

;; Test 6.2: cond with single clause
(let ([form '(my-cond [(> x 0) (print "positive")])])
  (define transformer (hash-ref (macro-table) 'my-cond))
  (define result (transformer form))
  (check-equal? result '(if (> x 0) (begin (print "positive")))))

;; Test 6.3: cond with multiple clauses
(let ([form '(my-cond
              [(< x 0) (print "negative")]
              [(> x 0) (print "positive")]
              [else (print "zero")])])
  (define transformer (hash-ref (macro-table) 'my-cond))
  (define result (transformer form))
  (check-equal? result '(if (< x 0)
                            (begin (print "negative"))
                            (my-cond [(> x 0) (print "positive")]
                                     [else (print "zero")]))))

;; ============================================================================
;; Test 7: Nested Macro Expansion
;; ============================================================================

(printf "Test 7: Nested macro expansion...\n")

(reset-macro-table!)
(process-define-syntax 'when
  '(syntax-rules ()
     [(when test body ...)
      (if test (begin body ...))]))

;; Test expanding a when that's already expanded
(let ([expr '(when (> x 0) (print "yes"))])
  (define result (expand-macros expr))
  (check-equal? result '(if (> x 0) (begin (print "yes")))))

;; ============================================================================
;; Test 8: Full Program with define-syntax
;; ============================================================================

(printf "Test 8: Full program with define-syntax...\n")

(reset-macro-table!)

;; Simulate processing multiple top-level forms
(define forms '(
  (define-syntax when
    (syntax-rules ()
      [(when test body ...)
       (if test (begin body ...))]))

  (define x 10)

  (when (> x 5)
    (print "big"))
))

(define expanded (filter-map expand-macros forms))

;; define-syntax should be filtered out (returns #f)
;; define and when should be expanded
(check-equal? (length expanded) 2 "Should have 2 expressions after filtering define-syntax")
(check-equal? (car expanded) '(define x 10))
(check-equal? (cadr expanded) '(if (> x 5) (begin (print "big"))))

;; ============================================================================
;; Test 9: or Macro (derived from R5RS)
;; ============================================================================

(printf "Test 9: or macro (from R5RS)...\n")

(reset-macro-table!)
(process-define-syntax 'my-or
  '(syntax-rules ()
     [(my-or) #f]
     [(my-or test) test]
     [(my-or test1 test2 ...)
      (let ([temp test1])
        (if temp temp (my-or test2 ...)))]))

;; Test 9.1: Empty or
(let ([form '(my-or)])
  (define transformer (hash-ref (macro-table) 'my-or))
  (define result (transformer form))
  (check-equal? result #f))

;; Test 9.2: Single argument
(let ([form '(my-or (> x 0))])
  (define transformer (hash-ref (macro-table) 'my-or))
  (define result (transformer form))
  (check-equal? result '(> x 0)))

;; Test 9.3: Multiple arguments
(let ([form '(my-or (< x 0) (> x 0))])
  (define transformer (hash-ref (macro-table) 'my-or))
  (define result (transformer form))
  ;; Should expand to nested let/if
  (check-true (and (list? result) (eq? (car result) 'let))))

;; ============================================================================
;; Test 10: Pattern Variables Extraction
;; ============================================================================

(printf "Test 10: Pattern variable extraction...\n")

;; Test 10.1: Simple pattern
(check-equal? (pattern-variables 'x '()) '(x))
(check-equal? (pattern-variables 'else '(else)) '() "Literal should not be a variable")

;; Test 10.2: Complex pattern
(check-equal? (set->list (list->set (pattern-variables '(let ([var val] ...) body ...) '(let))))
              (set->list (list->set '(var val body))))

;; Test 10.3: Nested pattern with literals
(check-equal? (set->list (list->set (pattern-variables '(cond [test result] [else value]) '(cond else))))
              (set->list (list->set '(test result value))))

;; ============================================================================
;; Test 11: Ellipsis Variable Check
;; ============================================================================

(printf "Test 11: Ellipsis variable check...\n")

(let ([bindings (make-hash)])
  (hash-set! bindings 'x '(1 2 3))  ; List binding
  (hash-set! bindings 'y 42)         ; Regular binding
  (check-true (ellipsis-variable? 'x bindings))
  (check-false (ellipsis-variable? 'y bindings)))

;; ============================================================================
;; Summary
;; ============================================================================

(printf "\n All macro system tests passed!\n")
(printf "  - Pattern matching (simple, nested, ellipsis)\n")
(printf "  - Template expansion (simple, nested, ellipsis)\n")
(printf "  - Macro transformers (when, unless, let, cond, or)\n")
(printf "  - Full program expansion with define-syntax\n")
