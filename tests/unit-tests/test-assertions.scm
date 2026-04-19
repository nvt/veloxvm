;; Test Framework - Assertion Library
;; Pure assertion functions that return result objects
;; No side effects - these don't print or modify state

;; Helper: Convert value to string for display
(define (value->string v)
  (cond
    ((string? v) (string-append "\"" v "\""))
    ((char? v) (string-append "#\\" (string v)))
    ((boolean? v) (if v "#t" "#f"))
    ((null? v) "()")
    ((number? v) (number->string v))
    ((symbol? v) (symbol->string v))
    ((list? v)
     (string-append "("
                    (let loop ((lst v))
                      (cond
                        ((null? lst) "")
                        ((null? (cdr lst)) (value->string (car lst)))
                        (else (string-append (value->string (car lst))
                                           " "
                                           (loop (cdr lst))))))
                    ")"))
    ((vector? v)
     (string-append "#("
                    (let loop ((i 0))
                      (cond
                        ((= i (vector-length v)) "")
                        ((= i (- (vector-length v) 1))
                         (value->string (vector-ref v i)))
                        (else (string-append (value->string (vector-ref v i))
                                           " "
                                           (loop (+ i 1))))))
                    ")"))
    (else "<object>")))

;; Pure assertion functions - return result objects
;; Result format: (pass description) or (fail description detail-lines) or (skip description reason)

;; Check that two values are equal
(define (check-equal expected actual description)
  (if (equal? expected actual)
      (list 'pass description)
      (list 'fail description
            (list (string-append "Expected: " (value->string expected))
                  (string-append "Actual:   " (value->string actual))))))

;; Check that two values are NOT equal
(define (check-not-equal not-expected actual description)
  (if (not (equal? not-expected actual))
      (list 'pass description)
      (list 'fail description
            (list (string-append "Expected NOT: " (value->string not-expected))
                  (string-append "But got: " (value->string actual))))))

;; Check that a value is true
(define (check-true value description)
  (check-equal #t value description))

;; Check that a value is false
(define (check-false value description)
  (check-equal #f value description))

;; Check that a value is truthy (not false)
(define (check-truthy value description)
  (if value
      (list 'pass description)
      (list 'fail description (list "Expected: truthy value, got: #f"))))

;; Create a skip result
(define (make-skip description reason)
  (list 'skip description reason))
