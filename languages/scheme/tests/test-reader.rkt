#lang racket

;; VeloxVM Scheme Compiler - Reader Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../reader.rkt")

;; Test single expression reading
(check-equal? (read-expr "(+ 1 2)")
              '(+ 1 2)
              "Read simple expression")

(check-equal? (read-expr "(lambda (x) (+ x 1))")
              '(lambda (x) (+ x 1))
              "Read lambda expression")

(check-equal? (read-expr "'foo")
              '(quote foo)
              "Read quoted symbol")

;; Test quasiquotation parsing
(check-equal? (read-expr "`(a b c)")
              '(quasiquote (a b c))
              "Read quasiquote")

(check-equal? (read-expr "`(a ,b c)")
              '(quasiquote (a (unquote b) c))
              "Read quasiquote with unquote")

(check-equal? (read-expr "`(a ,@lst c)")
              '(quasiquote (a (unquote-splicing lst) c))
              "Read quasiquote with splice")

;; Test reading multiple expressions
(check-equal? (read-all-exprs "(+ 1 2) (* 3 4)")
              '((+ 1 2) (* 3 4))
              "Read multiple expressions")

(check-equal? (read-all-exprs "(define x 42)\n(+ x 1)")
              '((define x 42) (+ x 1))
              "Read multiple lines")

;; Test numbers
(check-equal? (read-expr "42")
              42
              "Read integer")

(check-equal? (read-expr "3.14")
              3.14
              "Read real")

(check-equal? (read-expr "1/2")
              1/2
              "Read rational")

;; Test strings
(check-equal? (read-expr "\"hello\"")
              "hello"
              "Read string")

;; Test characters
(check-equal? (read-expr "#\\a")
              #\a
              "Read character")

(check-equal? (read-expr "#\\newline")
              #\newline
              "Read named character")

;; Test booleans
(check-equal? (read-expr "#t")
              #t
              "Read true")

(check-equal? (read-expr "#f")
              #f
              "Read false")

;; --- Include directive tests --------------------------------------------

(define test-dir (make-temporary-file "veloxvm-include-~a" 'directory))

(define (write-file rel-path contents)
  (with-output-to-file (build-path test-dir rel-path) #:exists 'replace
    (lambda () (display contents))))

(define (read-file rel-path)
  (let ([path (build-path test-dir rel-path)])
    (read-all-exprs (file->string path) (path->string path))))

(write-file "helper.scm" "(define helper-x 42) (define (helper-fn n) (* n 2))")

(write-file "top.scm" "(include \"helper.scm\") helper-x")
(check-equal? (read-file "top.scm")
              '((define helper-x 42)
                (define (helper-fn n) (* n 2))
                helper-x)
              "Top-level include splices file contents")

(write-file "nested.scm"
  "(define (outer) (include \"helper.scm\") (helper-fn helper-x))")
(check-equal? (read-file "nested.scm")
              '((define (outer)
                  (define helper-x 42)
                  (define (helper-fn n) (* n 2))
                  (helper-fn helper-x)))
              "Include inside define body splices into the body")

(write-file "let-nested.scm"
  "(let ((y 1)) (include \"helper.scm\") (+ y helper-x))")
(check-equal? (read-file "let-nested.scm")
              '((let ((y 1))
                  (define helper-x 42)
                  (define (helper-fn n) (* n 2))
                  (+ y helper-x)))
              "Include inside let body splices into the body")

(write-file "quote-protected.scm" "'(include \"helper.scm\")")
(check-equal? (read-file "quote-protected.scm")
              '((quote (include "helper.scm")))
              "Include inside quote is preserved as data, not expanded")

(write-file "syntax-protected.scm"
  "(define-syntax my-inc (syntax-rules () ((my-inc) (include \"helper.scm\"))))")
(check-equal? (read-file "syntax-protected.scm")
              '((define-syntax my-inc
                  (syntax-rules ()
                    ((my-inc) (include "helper.scm")))))
              "Include inside syntax-rules template is preserved")

(write-file "improper-formals.scm"
  "(define (variadic . rest) (include \"helper.scm\") (length rest))")
(check-equal? (read-file "improper-formals.scm")
              '((define (variadic . rest)
                  (define helper-x 42)
                  (define (helper-fn n) (* n 2))
                  (length rest)))
              "Include inside a define with dotted-rest formals")

;; Search-path resolution: if a relative include isn't found beside the
;; source file, the reader falls back to (include-search-paths).
(define search-dir (make-temporary-file "veloxvm-search-~a" 'directory))
(define lib-dir   (build-path search-dir "lib"))
(make-directory lib-dir)

(with-output-to-file (build-path lib-dir "shared.scm") #:exists 'replace
  (lambda () (display "(define shared-x 99)")))

;; Source file in test-dir; include path "shared.scm" only resolves via
;; the search path entry in lib-dir.
(write-file "use-shared.scm" "(include \"shared.scm\") shared-x")

(parameterize ([include-search-paths (list lib-dir)])
  (check-equal? (read-file "use-shared.scm")
                '((define shared-x 99) shared-x)
                "Include resolves through include-search-paths"))

(delete-directory/files search-dir)
(delete-directory/files test-dir)

(displayln "All reader tests passed!")
