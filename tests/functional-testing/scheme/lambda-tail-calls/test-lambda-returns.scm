;; Test what lambdas can return

(display "Test 1: Lambda returns integer")
(newline)
(define test1 (lambda () 42))
(display (test1))
(newline)
(newline)

(display "Test 2: Lambda returns string")
(newline)
(define test2 (lambda () "hello"))
(display (test2))
(newline)
(newline)

(display "Test 3: Lambda returns boolean")
(newline)
(define test3 (lambda () #t))
(display (test3))
(newline)
(newline)

(display "Test 4: Lambda returns quoted number")
(newline)
(define test4 (lambda () '42))
(display (test4))
(newline)
(newline)

(display "Test 5: Lambda returns quoted symbol")
(newline)
(define test5 (lambda () 'done))
(display (test5))
(newline)

(display "Completed!")
(newline)
