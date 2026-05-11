#lang racket

;; VeloxVM Racket Compiler - Optimizer Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../optimizer.rkt")

(parameterize ([optimization-level 1])

  ;; ============================================================================
  ;; Bottom-up / fixpoint folding (the case that motivated post-order)
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ (- 3 1) 4)) 6
                "Outer + folds after inner - is folded")

  (check-equal? (optimize-expr '(* (+ 1 2) (- 5 1))) 12
                "Both children fold, then outer *")

  (check-equal? (optimize-expr '(+ (+ (+ 1 2) 3) 4)) 10
                "Deeply nested binary + collapses end-to-end")

  (check-equal? (optimize-expr '(if (= 1 1) 'a 'b)) ''a
                "if folds after its test folds")

  (check-equal? (optimize-expr '(if (= 1 2) 'a 'b)) ''b
                "if false-branch via folded test")

  (check-equal? (optimize-expr '(not (= 1 1))) #f
                "Chained rule firing: (= 1 1) -> #t, then (not #t) -> #f")

  (check-equal? (optimize-expr '(* (+ 0 5) 1)) 5
                "Identity rules compose with arithmetic folding")

  ;; ============================================================================
  ;; Existing rules still work (regression coverage)
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ 2 3)) 5
                "Simple constant folding")

  (check-equal? (optimize-expr '(+ x 0)) 'x
                "(+ x 0) identity")

  (check-equal? (optimize-expr '(* x 0)) 0
                "(* x 0) zero")

  (check-equal? (optimize-expr '(if #t 'yes 'no)) ''yes
                "(if #t ...) literal folding")

  (check-equal? (optimize-expr '(begin)) #f
                "Empty begin")

  (check-equal? (optimize-expr '(begin 42)) 42
                "Single-form begin")

  ;; ============================================================================
  ;; Quote opacity (must not fold inside quoted data, R5RS)
  ;; ============================================================================

  (check-equal? (optimize-expr '(quote (+ 1 2))) '(quote (+ 1 2))
                "Do not fold inside quoted lists")

  (check-equal? (optimize-expr '(quote (if #t a b))) '(quote (if #t a b))
                "Do not fold special forms inside quote")

  ;; ============================================================================
  ;; Lambda and define recurse only into bodies
  ;; ============================================================================

  (check-equal? (optimize-expr '(lambda (x) (+ 1 2)))
                '(lambda (x) 3)
                "Lambda body is optimized")

  (check-equal? (optimize-expr '(lambda (x . rest) (+ 1 2)))
                '(lambda (x . rest) 3)
                "Variadic lambda formals preserved")

  (check-equal? (optimize-expr '(define (f x) (+ (- 3 1) x)))
                '(define (f x) (+ 2 x))
                "Define-shorthand body optimized; (+ 2 x) not foldable")

  ;; ============================================================================
  ;; Don't fold on non-numeric arguments
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ x 3)) '(+ x 3)
                "Won't fold when one operand is a variable")

  (check-equal? (optimize-expr '(/ 10 0)) '(/ 10 0)
                "Don't fold division by zero literal")

  ;; ============================================================================
  ;; Optimization disabled
  ;; ============================================================================

  (parameterize ([enable-optimizations #f])
    (check-equal? (optimize-expr '(+ (- 3 1) 4)) '(+ (- 3 1) 4)
                  "No folding when optimizations disabled"))

  (parameterize ([optimization-level 0])
    (check-equal? (optimize-expr '(+ 2 3)) '(+ 2 3)
                  "No folding at level 0"))

  ;; ============================================================================
  ;; Beta-reduce single-use let bindings
  ;; (let rewriter has already turned (let ((x v)) body) into
  ;;  ((lambda (x) body) v) by the time the optimizer sees it.)
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) (+ x 1)) 5)) 6
                "Numeric literal inlined, then (+ 5 1) folds")

  (check-equal? (optimize-expr '((lambda (x) (+ x x)) 5)) 10
                "Literal inlined at multiple use sites")

  (check-equal? (optimize-expr '((lambda (x) (g x x)) 7))
                '(g 7 7)
                "Literal inlined twice into a non-foldable call")

  (check-equal? (optimize-expr '((lambda (flag) (if flag 'yes 'no)) #t))
                ''yes
                "Boolean inline + if folding")

  (check-equal? (optimize-expr '((lambda (x y) (+ x y)) 5 10)) 15
                "Multi-binding lambda: both literals inlined, then folded")

  (check-equal? (optimize-expr '((lambda (x y) (+ x y)) 5 (g)))
                '((lambda (y) (+ 5 y)) (g))
                "Partial inline: literal x inlined, non-literal y kept")

  (check-equal? (optimize-expr '((lambda (x) (begin (set! x 6) x)) 5))
                '((lambda (x) (begin (set! x 6) x)) 5)
                "set! on parameter blocks inlining")

  (check-equal? (optimize-expr '((lambda (x) (g x)) (h)))
                '((lambda (x) (g x)) (h))
                "Non-literal value: not inlined (would change order)")

  (check-equal? (optimize-expr '((lambda (x) ((lambda (y) (+ x y)) 4)) 3)) 7
                "Nested lambda apps: cascade through both inlines and folds")

  (check-equal? (optimize-expr '((lambda (x) (lambda (x) x)) 5))
                '(lambda (x) x)
                "Inner lambda shadows: outer x inlined, inner x stays")

  ;; ============================================================================
  ;; Variadic and arity-mismatched lambdas are left alone
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda args (length args)) 1 2 3))
                '((lambda args (length args)) 1 2 3)
                "Bare-symbol formals not beta-reduced")

  (check-equal? (optimize-expr '((lambda (x . rest) x) 1 2 3))
                '((lambda (x . rest) x) 1 2 3)
                "Dotted formals not beta-reduced")

  ;; ============================================================================
  ;; Quote opacity in substitution
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) '(x x x)) 5))
                ''(x x x)
                "Don't substitute into quoted data")

  (check-equal? (optimize-expr '((lambda (x) (quote x)) 5))
                ''x
                "Quoted symbol stays even if name matches")

  ;; ============================================================================
  ;; Quoted-atom literals inline
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) (eq? x 'a)) 'a))
                #t
                "Quoted symbol literal inlined and eq? folded all the way")

  (check-equal? (optimize-expr '((lambda (x) (null? x)) '()))
                '(null? '())
                "Quoted empty list literal inlined (null? not folded -- not a fold target)")

  ;; ============================================================================
  ;; Pure-builtin folding (item #7)
  ;; ============================================================================

  ;; length on a quoted list
  (check-equal? (optimize-expr '(length (quote (1 2 3)))) 3
                "(length '(1 2 3)) folds to 3")
  (check-equal? (optimize-expr '(length (quote ()))) 0
                "(length '()) folds to 0")
  (check-equal? (optimize-expr '(length x)) '(length x)
                "Variable arg: length isn't folded")

  ;; string-length
  (check-equal? (optimize-expr '(string-length "hello")) 5
                "(string-length \"hello\") folds to 5")
  (check-equal? (optimize-expr '(string-length "")) 0
                "(string-length \"\") folds to 0")
  (check-equal? (optimize-expr '(string-length x)) '(string-length x)
                "Variable arg: string-length isn't folded")

  ;; string-append (variadic)
  (check-equal? (optimize-expr '(string-append "foo" "bar")) "foobar"
                "binary string-append folds")
  (check-equal? (optimize-expr '(string-append "a" "b" "c" "d")) "abcd"
                "4-ary string-append folds")
  (check-equal? (optimize-expr '(string-append)) ""
                "zero-arg string-append folds to empty string")
  (check-equal? (optimize-expr '(string-append "hi")) "hi"
                "unary string-append is the identity")
  (check-equal? (optimize-expr '(string-append "a" x "b"))
                '(string-append "a" x "b")
                "non-string in the middle: don't fold")

  ;; char<->integer
  (check-equal? (optimize-expr '(char->integer #\A)) 65
                "(char->integer #\\A) folds to 65")
  (check-equal? (optimize-expr '(integer->char 65)) #\A
                "(integer->char 65) folds to #\\A")
  (check-equal? (optimize-expr '(integer->char 300))
                '(integer->char 300)
                "out-of-VM-range codepoint: don't fold (VM stores chars as uint8_t)")
  (check-equal? (optimize-expr '(integer->char -1))
                '(integer->char -1)
                "negative codepoint: don't fold")

  ;; string->symbol
  (check-equal? (optimize-expr '(string->symbol "foo")) ''foo
                "(string->symbol \"foo\") folds to 'foo")

  ;; eq? folding
  (check-equal? (optimize-expr '(eq? (quote foo) (quote foo))) #t
                "(eq? 'foo 'foo) folds to #t")
  (check-equal? (optimize-expr '(eq? (quote foo) (quote bar))) #f
                "(eq? 'foo 'bar) folds to #f")
  (check-equal? (optimize-expr '(eq? #t #t)) #t
                "(eq? #t #t) folds to #t")
  (check-equal? (optimize-expr '(eq? #t #f)) #f
                "(eq? #t #f) folds to #f")
  (check-equal? (optimize-expr '(eq? #\a #\a)) #t
                "(eq? #\\a #\\a) folds")
  (check-equal? (optimize-expr '(eq? 1 1)) '(eq? 1 1)
                "(eq? 1 1) NOT folded: eq? on numbers is implementation-defined")

  ;; eqv? folding (extends eq? with numbers)
  (check-equal? (optimize-expr '(eqv? 1 1)) #t
                "(eqv? 1 1) folds to #t")
  (check-equal? (optimize-expr '(eqv? 1 2)) #f
                "(eqv? 1 2) folds to #f")
  (check-equal? (optimize-expr '(eqv? #t #t)) #t
                "(eqv? #t #t) folds")

  ;; equal? folding
  (check-equal? (optimize-expr '(equal? "x" "x")) #t
                "(equal? \"x\" \"x\") folds to #t")
  (check-equal? (optimize-expr '(equal? "x" "y")) #f
                "(equal? \"x\" \"y\") folds to #f")
  (check-equal? (optimize-expr '(equal? (quote (1 2)) (quote (1 2)))) #t
                "(equal? '(1 2) '(1 2)) folds (structural)")
  (check-equal? (optimize-expr '(equal? (quote (1 2)) (quote (1 3)))) #f
                "(equal? '(1 2) '(1 3)) folds to #f")
  (check-equal? (optimize-expr '(equal? 5 5)) #t
                "(equal? 5 5) folds")

  ;; not folding on non-boolean literals
  (check-equal? (optimize-expr '(not 5)) #f
                "(not 5) folds to #f (any number is truthy)")
  (check-equal? (optimize-expr '(not "")) #f
                "(not \"\") folds to #f (empty string is truthy)")
  (check-equal? (optimize-expr '(not #\a)) #f
                "(not #\\a) folds to #f")
  (check-equal? (optimize-expr '(not (quote foo))) #f
                "(not 'foo) folds to #f (any non-#f symbol is truthy)")
  (check-equal? (optimize-expr '(not (quote ()))) #f
                "(not '()) folds to #f (empty list is truthy in Scheme)")
  (check-equal? (optimize-expr '(not (quote #f))) #t
                "(not '#f) folds to #t")

  ;; Cascade: folds compose via bottom-up rewrite (item #1)
  (check-equal? (optimize-expr '(if (eq? (quote foo) (quote foo)) 'yes 'no))
                ''yes
                "eq? folds, then if folds, leaving just the consequent")
  (check-equal? (optimize-expr '(+ (string-length "hello") 1))
                6
                "string-length folds, then arithmetic folds")
  (check-equal? (optimize-expr '(char->integer (integer->char 65)))
                65
                "round-trip integer->char->integer folds end-to-end")

  ;; ============================================================================
  ;; N-ary arithmetic folding (item #8)
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ 1 2 3))    6 "ternary + folds")
  (check-equal? (optimize-expr '(+ 1 2 3 4))  10 "4-ary + folds")
  (check-equal? (optimize-expr '(+))          0 "(+) folds to 0")
  (check-equal? (optimize-expr '(+ 7))        7 "single-arg + folds to the arg")
  (check-equal? (optimize-expr '(* 2 3 4))    24 "ternary * folds")
  (check-equal? (optimize-expr '(*))          1 "(*) folds to 1")
  (check-equal? (optimize-expr '(- 10 3 2))   5 "ternary - (left-assoc) folds")
  (check-equal? (optimize-expr '(- 5))        -5 "single-arg - negates")
  (check-equal? (optimize-expr '(/ 100 5 2))  10 "ternary / (left-assoc) folds")

  ;; Mixed literal/variable: not folded
  (check-equal? (optimize-expr '(+ 1 x 2))    '(+ 1 x 2) "non-literal in middle: don't fold")
  (check-equal? (optimize-expr '(* x 2 3))    '(* x 2 3) "non-literal head: don't fold")

  ;; Don't fold division by zero
  (check-equal? (optimize-expr '(/ 100 0))    '(/ 100 0) "binary div-by-zero: don't fold")
  (check-equal? (optimize-expr '(/ 100 5 0))  '(/ 100 5 0) "trailing zero divisor: don't fold")
  ;; A leading zero numerator is fine (0 divided by anything non-zero is 0).
  (check-equal? (optimize-expr '(/ 0 5 2))    0 "zero numerator with non-zero divisors: folds to 0")

  ;; N-ary comparisons
  (check-equal? (optimize-expr '(< 1 2 3))    #t "ascending < chain: #t")
  (check-equal? (optimize-expr '(< 1 3 2))    #f "non-monotone <: #f")
  (check-equal? (optimize-expr '(= 5 5 5))    #t "all-equal =: #t")
  (check-equal? (optimize-expr '(>= 9 5 5))   #t ">= chain: #t")

  ;; ============================================================================
  ;; Through-let folding (cascade via beta + arithmetic)
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) (+ x 1)) 5)) 6
                "(let ((x 5)) (+ x 1)) folds to 6 via beta + fold")

  (check-equal? (optimize-expr '((lambda (x y) (+ x y 1)) 2 3)) 6
                "(let ((x 2) (y 3)) (+ x y 1)) cascades")

  ;; ============================================================================
  ;; Begin: drop pure non-final expressions
  ;; ============================================================================

  ;; Pure atoms in non-final position are dropped.
  (check-equal? (optimize-expr '(begin 1 2 3)) 3
                "(begin 1 2 3) -> 3 (all-pure -> just the last expr)")

  ;; Pure-by-primitives expressions are dropped. The (+ 1 2) and
  ;; (+ 3 4) also fold via the arithmetic rule, so the final form
  ;; preserves only the impure display and the folded final value.
  (check-equal? (optimize-expr '(begin (+ 1 2) (display "x") (+ 3 4)))
                '(begin (display "x") 7)
                "pure (+ 1 2) dropped; display kept; final (+ 3 4) folds to 7")

  ;; Several pure non-finals all collapse out.
  (check-equal? (optimize-expr '(begin (* 2 3) (length (quote (a b))) (foo)))
                '(foo)
                "all non-final exprs pure -> reduced to the impure final")

  ;; All-impure: nothing dropped.
  (check-equal? (optimize-expr '(begin (display "a") (display "b") (display "c")))
                '(begin (display "a") (display "b") (display "c"))
                "all-impure begin: unchanged")

  ;; Final expression's purity doesn't matter; it stays as the result.
  (check-equal? (optimize-expr '(begin (display "x") 42)) '(begin (display "x") 42)
                "pure final value is preserved")

  ;; Cascade: arithmetic folds first, then dead-code drops pure intermediates.
  (check-equal? (optimize-expr '(begin (+ 1 2) (* 3 4) (foo)))
                '(foo)
                "compose folding and dead-code elimination")

  ;; ============================================================================
  ;; (apply F '(...)) flattening (item #11)
  ;; ============================================================================

  ;; Basic flatten + arithmetic cascade.
  (check-equal? (optimize-expr '(apply + '(1 2 3))) 6
                "(apply + '(1 2 3)) flattens to (+ 1 2 3), then folds")

  ;; Prefix args before the final list.
  (check-equal? (optimize-expr '(apply + 10 '(1 2 3))) 16
                "prefix args preserved: (apply + 10 '(1 2 3)) -> (+ 10 1 2 3) -> 16")

  ;; Empty list: just (f) (or with prefix, (f prefix...)).
  (check-equal? (optimize-expr '(apply f '())) '(f)
                "empty list: zero-arg call")
  (check-equal? (optimize-expr '(apply f 'a 'b '())) '(f 'a 'b)
                "empty list with prefix args: prefix preserved")

  ;; Self-evaluating elements (numbers, strings, chars, booleans):
  ;; spliced as-is, no quote needed.
  (check-equal? (optimize-expr '(apply f '(1 "hi" #\c #t)))
                '(f 1 "hi" #\c #t)
                "self-evaluating elements spliced bare")

  ;; Symbol elements: re-quoted so they stay literal values, not
  ;; variable references.
  (check-equal? (optimize-expr '(apply f '(a b c)))
                '(f 'a 'b 'c)
                "symbol elements get re-quoted")

  ;; Nested list element: kept quoted.
  (check-equal? (optimize-expr '(apply f '(a (b c) d)))
                '(f 'a '(b c) 'd)
                "nested list element gets its quote back")

  ;; Mixed: each element handled individually.
  (check-equal? (optimize-expr '(apply f '(1 a "x" b)))
                '(f 1 'a "x" 'b)
                "mixed self-eval / symbol elements")

  ;; Function position can be a lambda; the result is then a beta-
  ;; reducible form and item #2 takes over.
  (check-equal? (optimize-expr '(apply (lambda (x) (* x x)) '(5))) 25
                "lambda in function position: cascade through beta + fold")

  ;; ============================================================================
  ;; Non-matching shapes are left alone
  ;; ============================================================================

  ;; Variable argument: not a literal list.
  (check-equal? (optimize-expr '(apply f xs)) '(apply f xs)
                "variable last arg: not folded")

  ;; Improper list literal: leave alone (compile-quote doesn't handle it).
  (check-equal? (optimize-expr '(apply f '(1 . 2))) '(apply f '(1 . 2))
                "improper list literal: not folded")

  ;; Single-arg apply (just function, no list): not the shape we touch.
  (check-equal? (optimize-expr '(apply f)) '(apply f)
                "no list arg at all: unchanged")

  ;; ============================================================================
  ;; Begin flattening (item #12)
  ;; ============================================================================

  ;; Simple inner-first nested begin.
  (check-equal? (optimize-expr '(begin (begin (display "a") (display "b")) (display "c")))
                '(begin (display "a") (display "b") (display "c"))
                "(begin (begin a b) c) -> (begin a b c)")

  ;; Inner-middle nested begin: order preserved.
  (check-equal? (optimize-expr '(begin (display "a") (begin (display "b") (display "c")) (display "d")))
                '(begin (display "a") (display "b") (display "c") (display "d"))
                "(begin a (begin b c) d) -> (begin a b c d)")

  ;; Two adjacent nested begins.
  (check-equal? (optimize-expr '(begin (begin (display "a") (display "b"))
                                       (begin (display "c") (display "d"))))
                '(begin (display "a") (display "b") (display "c") (display "d"))
                "two adjacent nested begins both flatten")

  ;; Deeply nested.
  (check-equal? (optimize-expr '(begin (begin (begin (display "x")))))
                '(display "x")
                "triple-nested begin collapses to its single contents")

  ;; Empty inner begin (folds to #f, then pure-drop removes the #f).
  (check-equal? (optimize-expr '(begin (begin) (display "x")))
                '(display "x")
                "empty inner begin collapses to #f, then pure-drop removes it")

  ;; Flatten + pure-drop cascade.
  (check-equal? (optimize-expr '(begin (begin (+ 1 2) (display "x")) (+ 3 4)))
                '(begin (display "x") 7)
                "nested begin flattens, then pure-drop and arithmetic fold cascade")

  ;; Non-begin children left in place.
  (check-equal? (optimize-expr '(begin (foo) (bar))) '(begin (foo) (bar))
                "no nested begin: rule doesn't fire")

  ;; ============================================================================
  ;; Identical-branch if (item #14)
  ;; ============================================================================

  ;; Pure test (variable reference is pure): drop the test entirely.
  (check-equal? (optimize-expr '(if x 5 5)) 5
                "pure test + identical branches: collapse to the branch")

  ;; Impure test (unknown user procedure): keep it via begin, drop the if.
  (check-equal? (optimize-expr '(if (foo) 5 5))
                '(begin (foo) 5)
                "impure test + identical branches: (begin test branch)")

  ;; Pure compound test (an arithmetic call) + identical branches:
  ;; collapse to just the branch.
  (check-equal? (optimize-expr '(if (+ 1 2) (h) (h)))
                '(h)
                "pure test (arithmetic) + identical branches: just the branch")

  ;; Different branches: rule does not fire.
  (check-equal? (optimize-expr '(if x 1 2)) '(if x 1 2)
                "different branches: unchanged")

  ;; Cascade: arithmetic folds first, then identical-branch fires.
  ;; (test is a bare variable, so pure; the begin disappears too.)
  (check-equal? (optimize-expr '(if test (+ 1 2) 3))
                3
                "arithmetic fold exposes identical branches"))

;; ============================================================================
;; Aggressive (level 2) strength reduction binds the argument to a temp
;; ============================================================================

(parameterize ([optimization-level 2])
  ;; (* expr 2) and (* 2 expr) should now produce a let-binding so the
  ;; argument is evaluated only once. After the let rewriter and
  ;; optimize-basic's beta-reduction (item #2) run on a literal arg,
  ;; the form folds further.
  (let ([result (optimize-expr '(* x 2))])
    (check-pred (lambda (r)
                  (match r
                    [`((lambda (,t1) (+ ,t2 ,t3)) x)
                     (and (eq? t1 t2) (eq? t1 t3))]
                    [_ #f]))
                result
                "(* x 2) binds x once and uses the temp twice"))

  (let ([result (optimize-expr '(* 2 x))])
    (check-pred (lambda (r)
                  (match r
                    [`((lambda (,t1) (+ ,t2 ,t3)) x)
                     (and (eq? t1 t2) (eq? t1 t3))]
                    [_ #f]))
                result
                "(* 2 x) (other order) also binds x once")))

