#lang racket

;; VeloxVM Racket Compiler - Rewriter Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../rewriter.rkt")

;; Test numerical predicates
(check-equal? (rewrite-expr '(positive? x))
              '(> x 0)
              "Rewrite positive?")

(check-equal? (rewrite-expr '(negative? x))
              '(< x 0)
              "Rewrite negative?")

(check-equal? (rewrite-expr '(odd? x))
              '(= (remainder x 2) 1)
              "Rewrite odd?")

(check-equal? (rewrite-expr '(even? x))
              '(= (remainder x 2) 0)
              "Rewrite even?")

;; Test abs. The rewriter binds the argument to a temp so the
;; expression isn't duplicated; after the let rewriter runs, the
;; result is a single-arg lambda application.
(let ([result (rewrite-expr '(abs x))])
  (check-pred (lambda (r)
                (match r
                  [`((lambda (,t1) (if (< ,t2 0) (- ,t3) ,t4)) x)
                   (and (eq? t1 t2) (eq? t1 t3) (eq? t1 t4))]
                  [_ #f]))
              result
              "abs binds the argument once and uses the temp three times"))

;; Test max/min. Both arguments bound first, then the if dispatches
;; over the temps.
(let ([result (rewrite-expr '(max a b))])
  (check-pred (lambda (r)
                (match r
                  [`((lambda (,ta ,tb) (if (> ,ta1 ,tb1) ,ta2 ,tb2)) a b)
                   (and (eq? ta ta1) (eq? ta ta2)
                        (eq? tb tb1) (eq? tb tb2)
                        (not (eq? ta tb)))]
                  [_ #f]))
              result
              "max binds both args once each"))

(let ([result (rewrite-expr '(min a b))])
  (check-pred (lambda (r)
                (match r
                  [`((lambda (,ta ,tb) (if (< ,ta1 ,tb1) ,ta2 ,tb2)) a b)
                   (and (eq? ta ta1) (eq? ta ta2)
                        (eq? tb tb1) (eq? tb tb2)
                        (not (eq? ta tb)))]
                  [_ #f]))
              result
              "min binds both args once each"))

;; Test I/O
(check-equal? (rewrite-expr '(newline))
              '(write-char #\newline)
              "Rewrite newline")

;; display is intentionally not rewritten -- it must reach the VM's
;; display operator so output flows through the port machinery (see
;; the comment above the println rewriter).
(check-equal? (rewrite-expr '(display "hello"))
              '(display "hello")
              "display passes through unchanged")

;; println chains one display per argument because the VM's display
;; operator only accepts 1 or 2 args (the second being a port). newline
;; is itself a rewriter that expands to (write-char #\newline), and
;; rewrite-expr applies rewriters recursively, so the trailing newline
;; flattens to write-char.
(check-equal? (rewrite-expr '(println "hello" "world"))
              '(begin (display "hello") (display "world")
                      (write-char #\newline))
              "Rewrite println")

(check-equal? (rewrite-expr '(println))
              '(begin (write-char #\newline))
              "Rewrite (println) with no args")

(check-equal? (rewrite-expr '(println "only"))
              '(begin (display "only") (write-char #\newline))
              "Rewrite single-arg println")

;; Test composite car/cdr
(check-equal? (rewrite-expr '(caar x))
              '(car (car x))
              "Rewrite caar")

(check-equal? (rewrite-expr '(cadr x))
              '(car (cdr x))
              "Rewrite cadr")

(check-equal? (rewrite-expr '(caddr x))
              '(car (cdr (cdr x)))
              "Rewrite caddr")

;; Test character comparisons
(check-equal? (rewrite-expr '(char=? a b))
              '(= (char-compare a b) 0)
              "Rewrite char=?")

(check-equal? (rewrite-expr '(char<? a b))
              '(< (char-compare a b) 0)
              "Rewrite char<?")

;; Test string comparisons
(check-equal? (rewrite-expr '(string=? a b))
              '(= (string-compare a b) 0)
              "Rewrite string=?")

(check-equal? (rewrite-expr '(string<? a b))
              '(< (string-compare a b) 0)
              "Rewrite string<?")

;; Test nested rewriting. abs and max each expand to a lambda
;; application around the arg-binding temp; check the shape rather
;; than exact symbol identity.
(let ([result (rewrite-expr '(+ (abs x) (max a b)))])
  (check-pred (lambda (r)
                (match r
                  [`(+ ((lambda (,_) (if (< ,_ 0) (- ,_) ,_)) x)
                       ((lambda (,_ ,_) (if (> ,_ ,_) ,_ ,_)) a b))
                   #t]
                  [_ #f]))
              result
              "nested abs/max each expand to a lambda app, leaving the outer + intact"))

;; Test no rewriting for unknown forms
(check-equal? (rewrite-expr '(unknown-form a b))
              '(unknown-form a b)
              "Don't rewrite unknown forms")

;; ============================================================================
;; letrec: no cross-references collapses to plain let
;; ============================================================================

;; Non-recursive bindings -- letrec should become plain let, which then
;; rewrites to a lambda application.
(check-equal? (rewrite-expr '(letrec ((x 5) (y 10)) (+ x y)))
              '((lambda (x y) (+ x y)) 5 10)
              "letrec with no cross-refs becomes a plain lambda app")

;; A binding whose value references its own name -- real self-recursion.
;; Keep the dummy-#f + set! dance.
(check-equal? (rewrite-expr '(letrec ((f (lambda (n) (f n)))) (f 0)))
              '((lambda (f) (set! f (lambda (n) (f n))) (f 0)) #f)
              "self-recursive letrec keeps the set! dance")

;; Mutual recursion -- one binding's value references the other.
(check-equal? (rewrite-expr '(letrec ((a (lambda () (b)))
                                      (b (lambda () (a))))
                               (a)))
              '((lambda (a b)
                  (set! a (lambda () (b)))
                  (set! b (lambda () (a)))
                  (a))
                #f #f)
              "mutual-recursive letrec keeps the set! dance")

;; Free reference inside a quoted datum is NOT a real reference.
(check-equal? (rewrite-expr '(letrec ((x 5)) (list 'x x)))
              '((lambda (x) (list 'x x)) 5)
              "quoted occurrence of the name doesn't count as a reference")

;; Free reference inside a lambda whose formals shadow the name doesn't
;; count either.
(check-equal? (rewrite-expr '(letrec ((x (lambda (x) (* x 2)))) (x 5)))
              '((lambda (x) (x 5)) (lambda (x) (* x 2)))
              "shadowed inner lambda doesn't count as a self-reference")

;; ============================================================================
;; Quasiquote append-fold (item #18)
;; ============================================================================

;; Splicing a quoted list followed by a literal tail: should fold to a
;; single quote instead of emitting a runtime (append ...).
(check-equal? (rewrite-expr `(quasiquote (,@'(a b) c)))
              ''(a b c)
              "(append '(a b) '(c)) folds to '(a b c)")

;; Two adjacent splices, both of quoted lists.
(check-equal? (rewrite-expr `(quasiquote (,@'(a b) ,@'(c d))))
              ''(a b c d)
              "two splices fold end-to-end")

;; Splicing an empty list.
(check-equal? (rewrite-expr `(quasiquote (,@'() x)))
              ''(x)
              "empty splice + literal tail folds")

;; Splicing a runtime value: no fold (the append stays at runtime).
(check-equal? (rewrite-expr `(quasiquote (,@xs c)))
              '(append xs '(c))
              "non-literal splice: append stays")

(displayln "All rewriter tests passed!")
