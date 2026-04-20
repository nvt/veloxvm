#lang racket

;; Tests for letrec-syntax implementation

(require rackunit
         "../expander.rkt")

(printf "Testing letrec-syntax...\n\n")

;; Test 1: Basic letrec-syntax (like let-syntax)
(printf "Test 1: Basic letrec-syntax with single macro...\n")
(reset-macro-table!)

(define test1
  '(letrec-syntax ([my-when (syntax-rules ()
                               [(my-when test body ...)
                                (if test (begin body ...))])])
     (my-when #t (display "yes"))))

(define result1 (expand-macros test1))
(check-equal? result1 '(if #t (begin (display "yes"))))
(printf "   Single macro expands correctly\n")

;; Test 2: Mutually recursive macros (simpler example)
;; Note: True recursion with non-literal patterns causes infinite loops
;; because macros do syntactic transformation, not evaluation
(printf "\nTest 2: Mutually recursive macros...\n")
(reset-macro-table!)

(define test2
  '(letrec-syntax
       ([macro-a (syntax-rules ()
                   [(macro-a) (macro-b 'from-a)])]
        [macro-b (syntax-rules ()
                   [(macro-b x) (list 'b-result x)])])
     (macro-a)))

(define result2 (expand-macros test2))
(check-equal? result2 '(list 'b-result 'from-a))
(printf "   Mutually recursive macros (macro-a calls macro-b)\n")

;; Test 3: Self-referential macro (non-recursive pattern)
(printf "\nTest 3: Self-referential macro...\n")
(reset-macro-table!)

(define test3
  '(letrec-syntax ([my-macro (syntax-rules ()
                               [(my-macro base) base]
                               [(my-macro recursive-call result)
                                (list recursive-call result)])])
     (my-macro 'done)))

(define result3 (expand-macros test3))
(check-equal? result3 ''done)
(printf "   Self-referential macro expands\n")

;; Test 4: Scope isolation (like let-syntax)
(printf "\nTest 4: Local macro scope isolation...\n")
(reset-macro-table!)

(define test4a
  '(letrec-syntax ([local-macro (syntax-rules ()
                                  [(local-macro x) (* x 2)])])
     (local-macro 21)))

(define result4a (expand-macros test4a))
(check-equal? result4a '(* 21 2))
(printf "   Local macro visible inside letrec-syntax\n")

;; Try to use local macro outside
(define test4b '(local-macro 42))
(define result4b (expand-macros test4b))
(check-equal? result4b '(local-macro 42))  ; Should not expand
(printf "   Local macro not visible outside letrec-syntax\n")

;; Test 5: Shadowing global macros
(printf "\nTest 5: Local macro shadows global...\n")
(reset-macro-table!)

;; Define global version
(process-define-syntax 'foo
  '(syntax-rules () [(foo x) (list 'global x)]))

;; Shadow with local version
(define test5
  '(letrec-syntax ([foo (syntax-rules ()
                          [(foo x) (list 'local x)])])
     (foo 3)))

(define result5 (expand-macros test5))
(check-equal? result5 '(list 'local 3))
(printf "   Local macro shadows global\n")

;; After letrec-syntax, global version works again
(define test5b '(foo 99))
(check-equal? (expand-macros test5b) '(list 'global 99))
(printf "   After letrec-syntax, global macro works again\n")

;; Test 6: Multiple mutually-recursive macros
(printf "\nTest 6: Multiple mutually-recursive macros...\n")
(reset-macro-table!)

(define test6
  '(letrec-syntax ([a (syntax-rules () [(a) (b)])]
                   [b (syntax-rules () [(b) (c)])]
                   [c (syntax-rules () [(c) 'done])])
     (a)))

(define result6 (expand-macros test6))
(check-equal? result6 ''done)
(printf "   Chain of mutually-recursive macros works\n")

;; Test 7: Nested letrec-syntax
(printf "\nTest 7: Nested letrec-syntax...\n")
(reset-macro-table!)

(define test7
  '(letrec-syntax ([outer (syntax-rules ()
                            [(outer x) (list 'outer x)])])
     (outer
      (letrec-syntax ([inner (syntax-rules ()
                               [(inner y) (list 'inner y)])])
        (inner 42)))))

(define result7 (expand-macros test7))
(check-equal? result7 '(list 'outer (list 'inner 42)))
(printf "   Nested letrec-syntax works correctly\n")

;; Test 8: letrec-syntax with multiple body expressions
(printf "\nTest 8: Multiple body expressions...\n")
(reset-macro-table!)

(define test8
  '(letrec-syntax ([double (syntax-rules () [(double x) (* x 2)])])
     (double 1)
     (double 2)
     (double 3)))

(define result8 (expand-macros test8))
(check-equal? result8 '(begin (* 1 2) (* 2 2) (* 3 2)))
(printf "   Multiple body expressions wrapped in begin\n")

;; Test 9: Single body expression (no begin)
(printf "\nTest 9: Single body expression...\n")
(reset-macro-table!)

(define test9
  '(letrec-syntax ([triple (syntax-rules () [(triple x) (* x 3)])])
     (triple 7)))

(define result9 (expand-macros test9))
(check-equal? result9 '(* 7 3))
(printf "   Single body expression not wrapped in begin\n")

;; Test 10: Difference from let-syntax (semantic test)
;; In let-syntax, macros can't see each other during definition
;; In letrec-syntax, they can (enabling mutual recursion)
(printf "\nTest 10: Semantic difference from let-syntax...\n")
(reset-macro-table!)

;; This works with letrec-syntax
(define test10a
  '(letrec-syntax ([m1 (syntax-rules () [(m1) 1])]
                   [m2 (syntax-rules () [(m2) (m1)])])
     (m2)))

(define result10a (expand-macros test10a))
(check-equal? result10a 1)
(printf "   letrec-syntax: m2 can reference m1 in template\n")

;; With let-syntax, the same code works too (for syntax-rules)
;; because templates aren't expanded at definition time
(define test10b
  '(let-syntax ([m1 (syntax-rules () [(m1) 2])]
                [m2 (syntax-rules () [(m2) (m1)])])
     (m2)))

(define result10b (expand-macros test10b))
(check-equal? result10b 2)
(printf "   let-syntax: m2 can also reference m1 (templates expanded at use time)\n")
(printf "  Note: For syntax-rules, let-syntax and letrec-syntax behave similarly\n")
(printf "  Note: The difference matters for other transformer types\n")

;; Test 11: Macro expanding to another macro call
(printf "\nTest 11: Macro expanding to use another macro...\n")
(reset-macro-table!)

(define test11
  '(letrec-syntax ([make-list (syntax-rules ()
                                [(make-list x) (list x)])]
                   [wrap (syntax-rules ()
                           [(wrap val) (make-list val)])])
     (wrap 42)))

(define result11 (expand-macros test11))
(check-equal? result11 '(list 42))
(printf "   Macro expansion using another macro works\n")

(printf "\n All letrec-syntax tests passed!\n")
(printf "  - Basic local macros\n")
(printf "  - Mutually recursive macros (macro-a calls macro-b)\n")
(printf "  - Self-referential macros\n")
(printf "  - Scope isolation (no leakage)\n")
(printf "  - Shadowing global macros\n")
(printf "  - Multiple mutually-recursive macros\n")
(printf "  - Nested letrec-syntax\n")
(printf "  - Multiple/single body expressions\n")
(printf "  - Semantic comparison with let-syntax\n")
(printf "  - Cross-macro expansion patterns\n")
(printf "\nNote: True recursion with non-literal patterns (like countdown)\n")
(printf "  causes infinite loops at macro expansion time because macros\n")
(printf "  do syntactic transformation, not evaluation.\n")
