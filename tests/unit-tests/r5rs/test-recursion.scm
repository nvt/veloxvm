;; VeloxVM Unit Tests - R5RS Recursion and Tail Call Optimization
;; Tests for: Simple recursion, tail recursion, mutual recursion, named let

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Simple Recursion")

;; Classic factorial - non-tail recursive
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(assert-equal 1 (factorial 0) "factorial 0")
(assert-equal 1 (factorial 1) "factorial 1")
(assert-equal 120 (factorial 5) "factorial 5")
(assert-equal 3628800 (factorial 10) "factorial 10")

(test-suite "Tail Recursion")

;; Tail recursive factorial with accumulator
(define (factorial-tail n)
  (define (fact-iter n acc)
    (if (<= n 1)
        acc
        (fact-iter (- n 1) (* n acc))))
  (fact-iter n 1))

(assert-equal 120 (factorial-tail 5) "tail factorial 5")
(assert-equal 3628800 (factorial-tail 10) "tail factorial 10")
(assert-equal 479001600 (factorial-tail 12) "tail factorial 12")

;; Sum to n - tail recursive
(define (sum-to-n n)
  (define (sum-iter i acc)
    (if (> i n)
        acc
        (sum-iter (+ i 1) (+ acc i))))
  (sum-iter 1 0))

(assert-equal 5050 (sum-to-n 100) "sum to 100")
(assert-equal 500500 (sum-to-n 1000) "sum to 1000")

(test-suite "Mutual Recursion")

;; Mutually recursive even?/odd? predicates
(define (my-even? n)
  (if (= n 0)
      #t
      (my-odd? (- n 1))))

(define (my-odd? n)
  (if (= n 0)
      #f
      (my-even? (- n 1))))

(assert-equal #t (my-even? 0) "even? 0")
(assert-equal #f (my-even? 1) "even? 1")
(assert-equal #t (my-even? 2) "even? 2")
(assert-equal #t (my-even? 42) "even? 42")
(assert-equal #f (my-odd? 0) "odd? 0")
(assert-equal #t (my-odd? 1) "odd? 1")
(assert-equal #t (my-odd? 3) "odd? 3")
(assert-equal #f (my-odd? 42) "odd? 42")

(test-suite "Tail Recursive List Operations")

;; Tail recursive length
(define (length-tail lst)
  (define (len-iter lst acc)
    (if (null? lst)
        acc
        (len-iter (cdr lst) (+ acc 1))))
  (len-iter lst 0))

(assert-equal 0 (length-tail '()) "length-tail empty")
(assert-equal 1 (length-tail '(1)) "length-tail single")
(assert-equal 5 (length-tail '(1 2 3 4 5)) "length-tail multiple")

;; Tail recursive reverse
(define (reverse-tail lst)
  (define (rev-iter lst acc)
    (if (null? lst)
        acc
        (rev-iter (cdr lst) (cons (car lst) acc))))
  (rev-iter lst '()))

(assert-equal '() (reverse-tail '()) "reverse-tail empty")
(assert-equal '(1) (reverse-tail '(1)) "reverse-tail single")
(assert-equal '(5 4 3 2 1) (reverse-tail '(1 2 3 4 5)) "reverse-tail multiple")

(test-suite "Named Let (Recursive)")

;; Named let for iteration
(define (sum-list lst)
  (let loop ((items lst) (total 0))
    (if (null? items)
        total
        (loop (cdr items) (+ total (car items))))))

(assert-equal 0 (sum-list '()) "sum-list empty")
(assert-equal 42 (sum-list '(42)) "sum-list single")
(assert-equal 15 (sum-list '(1 2 3 4 5)) "sum-list multiple")

(test-suite "Nested Recursion")

;; Ackermann function - doubly recursive (basic tests only)
(define (ackermann m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ackermann (- m 1) 1))
        (else (ackermann (- m 1) (ackermann m (- n 1))))))

(assert-equal 1 (ackermann 0 0) "ackermann(0,0)")
(assert-equal 2 (ackermann 0 1) "ackermann(0,1)")
(assert-equal 3 (ackermann 1 1) "ackermann(1,1)")
(assert-equal 2 (ackermann 1 0) "ackermann(1,0)")

(test-suite "Tree Recursion")

;; Classic Fibonacci - tree recursive
(define (fib n)
  (cond ((<= n 1) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(assert-equal 0 (fib 0) "fibonacci 0")
(assert-equal 1 (fib 1) "fibonacci 1")
(assert-equal 5 (fib 5) "fibonacci 5")
(assert-equal 55 (fib 10) "fibonacci 10")

(test-suite "Deep Recursion (Tail Call Optimization Required)")

;; Count down - tests TCO with large call depth
(define (count-down n)
  (if (<= n 0)
      'done
      (count-down (- n 1))))

(assert-equal 'done (count-down 1000) "deep recursion 1000")
(assert-equal 'done (count-down 5000) "deep recursion 5000")

(test-suite "Mutual Recursion with State")

;; Count evens and odds using mutual recursion
(define (count-evens-odds lst)
  (define (count-e lst e-count o-count)
    (if (null? lst)
        (list e-count o-count)
        (if (even? (car lst))
            (count-e (cdr lst) (+ e-count 1) o-count)
            (count-o (cdr lst) e-count (+ o-count 1)))))
  (define (count-o lst e-count o-count)
    (if (null? lst)
        (list e-count o-count)
        (if (odd? (car lst))
            (count-o (cdr lst) e-count (+ o-count 1))
            (count-e (cdr lst) (+ e-count 1) o-count))))
  (count-e lst 0 0))

(assert-equal '(3 2) (count-evens-odds '(2 4 6 1 3)) "count evens and odds")

(test-summary)
