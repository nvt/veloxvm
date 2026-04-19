;; VeloxVM Unit Tests - Named Let (R5RS section 4.2.4)
;; Tests for: named let (iterative loops with recursion)

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Named Let - Basic Iteration")

;; Simple countdown loop
(assert-equal 0
  (let loop ((n 5))
    (if (= n 0)
        n
        (loop (- n 1))))
  "Countdown to 0")

;; Sum from 1 to n
(assert-equal 55
  (let sum ((n 10) (acc 0))
    (if (= n 0)
        acc
        (sum (- n 1) (+ acc n))))
  "Sum from 1 to 10")

;; Power function (2^10)
(assert-equal 1024
  (let power ((base 2) (n 10))
    (if (= n 0)
        1
        (* base (power base (- n 1)))))
  "Power 2^10")

(test-suite "Named Let - Mathematical Functions")

;; Factorial using named let
(assert-equal 120
  (let fact ((n 5))
    (if (<= n 1)
        1
        (* n (fact (- n 1)))))
  "Factorial of 5")

;; Fibonacci with accumulator
(assert-equal 55
  (let fib ((n 10) (a 0) (b 1))
    (if (= n 0)
        a
        (fib (- n 1) b (+ a b))))
  "Fibonacci 10 with accumulator")

;; GCD using Euclidean algorithm
(assert-equal 6
  (let gcd-loop ((a 48) (b 18))
    (if (= b 0)
        a
        (gcd-loop b (remainder a b))))
  "GCD of 48 and 18")

;; Collatz sequence length
(assert-equal 111
  (let collatz ((n 27) (len 0))
    (if (= n 1)
        len
        (if (= (remainder n 2) 0)
            (collatz (/ n 2) (+ len 1))
            (collatz (+ (* 3 n) 1) (+ len 1)))))
  "Collatz sequence length for 27")

(test-suite "Named Let - List Operations")

;; List length using named let
(assert-equal 5
  (let len ((lst '(1 2 3 4 5)))
    (if (null? lst)
        0
        (+ 1 (len (cdr lst)))))
  "Length of 5-element list")

;; Reverse list with accumulator
(assert-equal '(5 4 3 2 1)
  (let rev ((lst '(1 2 3 4 5)) (acc '()))
    (if (null? lst)
        acc
        (rev (cdr lst) (cons (car lst) acc))))
  "Reverse list with accumulator")

;; Filter even numbers
(assert-equal '(2 4 6 8 10)
  (let filt ((lst '(1 2 3 4 5 6 7 8 9 10)) (acc '()))
    (if (null? lst)
        (let rev-acc ((l acc) (r '()))
          (if (null? l)
              r
              (rev-acc (cdr l) (cons (car l) r))))
        (if (= (remainder (car lst) 2) 0)
            (filt (cdr lst) (cons (car lst) acc))
            (filt (cdr lst) acc))))
  "Filter even numbers from 1-10")

(test-suite "Named Let - Edge Cases")

;; Named let with single iteration
(assert-equal 1
  (let once ((n 1))
    n)
  "Named let with single iteration")

;; Nested named let
(assert-equal 10
  (let outer ((x 5))
    (let inner ((y 2))
      (* x y)))
  "Nested named let")

;; Named let with multiple parameters
(assert-equal 12
  (let multi ((a 3) (b 5) (c 7))
    (if (> a 0)
        (multi (- a 1) b c)
        (+ b c)))
  "Named let with multiple parameters")

(test-summary)
