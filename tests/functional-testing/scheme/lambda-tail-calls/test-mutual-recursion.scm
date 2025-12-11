;; Test mutual recursion with tail calls
;; Tests whether tail call optimization works correctly
;; when two functions call each other in tail position

(display "Testing mutual recursion...")
(newline)

;; Test 1: Simple mutual recursion with booleans
(display "Test 1: is-even? and is-odd? with small numbers")
(newline)

(define is-even?
  (lambda (n)
    (if (= n 0)
        #t
        (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0)
        #f
        (is-even? (- n 1)))))

(display "is-even? 4: ")
(display (is-even? 4))
(newline)

(display "is-odd? 4: ")
(display (is-odd? 4))
(newline)

(display "is-even? 7: ")
(display (is-even? 7))
(newline)

(display "is-odd? 7: ")
(display (is-odd? 7))
(newline)

;; Test 2: Mutual recursion with larger numbers (tests TCO)
(display "Test 2: Large numbers requiring TCO")
(newline)

(display "is-even? 1000: ")
(display (is-even? 1000))
(newline)

(display "is-odd? 999: ")
(display (is-odd? 999))
(newline)

;; Test 3: Mutual recursion with accumulator
(display "Test 3: Mutual recursion with accumulator")
(newline)

(define count-a
  (lambda (n acc)
    (if (= n 0)
        acc
        (count-b (- n 1) (+ acc 1)))))

(define count-b
  (lambda (n acc)
    (if (= n 0)
        acc
        (count-a (- n 1) (+ acc 1)))))

(display "count-a 100 0: ")
(display (count-a 100 0))
(newline)

(display "All mutual recursion tests completed!")
(newline)
