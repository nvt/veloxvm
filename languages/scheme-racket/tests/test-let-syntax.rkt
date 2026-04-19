#lang racket

;; Tests for let-syntax implementation

(require rackunit
         "../expander.rkt")

(printf "Testing let-syntax...\n\n")

;; Test 1: Basic let-syntax with single macro
(printf "Test 1: Basic let-syntax with single local macro...\n")
(reset-macro-table!)

(define test1
  '(let-syntax ([my-when (syntax-rules ()
                           [(my-when test body ...)
                            (if test (begin body ...))])])
     (my-when #t (display "yes"))))

(define result1 (expand-macros test1))
(check-equal? result1 '(if #t (begin (display "yes"))))
(printf "   Single local macro expands correctly\n")

;; Test 2: Local macro doesn't leak outside let-syntax
(printf "\nTest 2: Local macro scope isolation...\n")
(reset-macro-table!)

;; Define a global macro
(process-define-syntax 'global-when
  '(syntax-rules () [(global-when test body ...)
                      (if test (begin body ...))]))

;; Use let-syntax with local macro
(define test2a
  '(let-syntax ([local-when (syntax-rules ()
                              [(local-when test body ...)
                               (if test (list body ...))])])
     (local-when #t 1 2 3)))

(define result2a (expand-macros test2a))
(check-equal? result2a '(if #t (list 1 2 3)))
(printf "   Local macro visible inside let-syntax\n")

;; Try to use local macro outside let-syntax
(define test2b '(local-when #t 4 5 6))
(define result2b (expand-macros test2b))
;; Should not expand (local-when not defined globally)
(check-equal? result2b '(local-when #t 4 5 6))
(printf "   Local macro not visible outside let-syntax\n")

;; But global macro should still work
(define test2c '(global-when #f (display "no")))
(define result2c (expand-macros test2c))
(check-equal? result2c '(if #f (begin (display "no"))))
(printf "   Global macro still works after let-syntax\n")

;; Test 3: Shadowing - local macro overrides global
(printf "\nTest 3: Local macro shadows global...\n")
(reset-macro-table!)

;; Define global version
(process-define-syntax 'foo
  '(syntax-rules () [(foo x) (list 'global x)]))

;; Use in global scope
(define test3a '(foo 42))
(check-equal? (expand-macros test3a) '(list 'global 42))
(printf "   Global 'foo' expands to (list 'global 42)\n")

;; Shadow with local version
(define test3b
  '(let-syntax ([foo (syntax-rules () [(foo x) (list 'local x)])])
     (foo 99)))

(check-equal? (expand-macros test3b) '(list 'local 99))
(printf "   Local 'foo' shadows global, expands to (list 'local 99)\n")

;; After let-syntax, global version works again
(define test3c '(foo 123))
(check-equal? (expand-macros test3c) '(list 'global 123))
(printf "   After let-syntax, global 'foo' works again\n")

;; Test 4: Multiple local macros
(printf "\nTest 4: Multiple local macros in one let-syntax...\n")
(reset-macro-table!)

(define test4
  '(let-syntax ([my-add (syntax-rules () [(my-add a b) (+ a b)])]
                [my-mul (syntax-rules () [(my-mul a b) (* a b)])])
     (my-add (my-mul 2 3) 4)))

(define result4 (expand-macros test4))
(check-equal? result4 '(+ (* 2 3) 4))
(printf "   Multiple local macros work together\n")

;; Test 5: Nested let-syntax
(printf "\nTest 5: Nested let-syntax...\n")
(reset-macro-table!)

(define test5
  '(let-syntax ([outer (syntax-rules () [(outer x) (list 'outer x)])])
     (outer
      (let-syntax ([inner (syntax-rules () [(inner y) (list 'inner y)])])
        (inner 42)))))

(define result5 (expand-macros test5))
(check-equal? result5 '(list 'outer (list 'inner 42)))
(printf "   Nested let-syntax works correctly\n")

;; Test 6: let-syntax with multiple body expressions
(printf "\nTest 6: let-syntax with multiple body expressions...\n")
(reset-macro-table!)

(define test6
  '(let-syntax ([m (syntax-rules () [(m x) (* x 2)])])
     (m 1)
     (m 2)
     (m 3)))

(define result6 (expand-macros test6))
;; Multiple expressions should be wrapped in begin
(check-equal? result6 '(begin (* 1 2) (* 2 2) (* 3 2)))
(printf "   Multiple body expressions wrapped in begin\n")

;; Test 7: let-syntax with single body expression (no begin)
(printf "\nTest 7: let-syntax with single body expression...\n")
(reset-macro-table!)

(define test7
  '(let-syntax ([m (syntax-rules () [(m x) (* x 2)])])
     (m 5)))

(define result7 (expand-macros test7))
;; Single expression should not be wrapped in begin
(check-equal? result7 '(* 5 2))
(printf "   Single body expression not wrapped in begin\n")

;; Test 8: Local macro using another local macro
(printf "\nTest 8: Local macro using another local macro...\n")
(reset-macro-table!)

(define test8
  '(let-syntax ([double (syntax-rules () [(double x) (+ x x)])]
                [quadruple (syntax-rules () [(quadruple x) (double (double x))])])
     (quadruple 5)))

(define result8 (expand-macros test8))
;; Should expand: quadruple 5 -> double (double 5) -> double (+ 5 5) -> (+ (+ 5 5) (+ 5 5))
(check-equal? result8 '(+ (+ 5 5) (+ 5 5)))
(printf "   Local macros can use other local macros\n")

;; Test 9: let-syntax inside macro expansion
(printf "\nTest 9: let-syntax inside macro expansion...\n")
(reset-macro-table!)

;; Define a macro that expands to let-syntax
(process-define-syntax 'with-local
  '(syntax-rules ()
     [(with-local body)
      (let-syntax ([local (syntax-rules () [(local) 'local-value])])
        body)]))

(define test9 '(with-local (local)))
(define result9 (expand-macros test9))
(check-equal? result9 ''local-value)
(printf "   let-syntax works inside macro expansion\n")

;; Test 10: Empty body (edge case)
(printf "\nTest 10: Empty body in let-syntax...\n")
(reset-macro-table!)

(define test10
  '(let-syntax ([m (syntax-rules () [(m) 'hello])])))

(define result10 (expand-macros test10))
(check-equal? result10 '(begin))
(printf "   Empty body returns (begin)\n")

(printf "\n All let-syntax tests passed!\n")
(printf "  - Basic local macros\n")
(printf "  - Scope isolation (no leakage)\n")
(printf "  - Shadowing global macros\n")
(printf "  - Multiple local macros\n")
(printf "  - Nested let-syntax\n")
(printf "  - Multiple body expressions\n")
(printf "  - Single body expression\n")
(printf "  - Local macros using each other\n")
(printf "  - let-syntax in macro expansion\n")
(printf "  - Edge cases\n")
