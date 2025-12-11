;; Test inline vs named lambda calls

(display "Test 1: Inline lambda returning symbol")
(newline)
(display ((lambda () 'inline-symbol)))
(newline)
(newline)

(display "Test 2: Named lambda returning symbol")
(newline)
(define named-lambda (lambda () 'named-symbol))
(display (named-lambda))
(newline)

(display "Done")
(newline)
