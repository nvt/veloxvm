;; Control Flow Demonstration
;; Shows: cond, case, named let, and conditional logic

(print "=== Control Flow Examples ===" #\Newline #\Newline)

;; 1. COND - Multi-way conditional
(print "1. Testing COND with score grading:" #\Newline)
(define (grade score)
  (cond ((>= score 90) 'A)
        ((>= score 80) 'B)
        ((>= score 70) 'C)
        ((>= score 60) 'D)
        (else 'F)))

(print "   Score 95: " (grade 95) #\Newline)
(print "   Score 75: " (grade 75) #\Newline)
(print "   Score 55: " (grade 55) #\Newline #\Newline)

;; 2. CASE - Pattern matching on values
(print "2. Testing CASE with day of week:" #\Newline)
(define (day-type day)
  (case day
    ((monday tuesday wednesday thursday friday) 'weekday)
    ((saturday sunday) 'weekend)
    (else 'invalid)))

(print "   Monday: " (day-type 'monday) #\Newline)
(print "   Saturday: " (day-type 'saturday) #\Newline)
(print "   Unknown: " (day-type 'holiday) #\Newline #\Newline)

;; 3. NAMED LET - Iteration with local recursion
(print "3. Testing NAMED LET - partition list:" #\Newline)
(define (partition-numbers numbers)
  (let loop ((nums numbers)
             (positive '())
             (negative '()))
    (cond ((null? nums)
           (list positive negative))
          ((>= (car nums) 0)
           (loop (cdr nums)
                 (cons (car nums) positive)
                 negative))
          (else
           (loop (cdr nums)
                 positive
                 (cons (car nums) negative))))))

(define test-list '(3 -2 8 -5 0 7 -1 4))
(define result (partition-numbers test-list))
(print "   Input: " test-list #\Newline)
(print "   Positive: " (car result) #\Newline)
(print "   Negative: " (car (cdr result)) #\Newline #\Newline)

;; 4. Combining control flow - find maximum
(print "4. Finding maximum using named let:" #\Newline)
(define (find-max numbers)
  (if (null? numbers)
      #f
      (let loop ((rest (cdr numbers))
                 (max-val (car numbers)))
        (cond ((null? rest) max-val)
              ((> (car rest) max-val)
               (loop (cdr rest) (car rest)))
              (else
               (loop (cdr rest) max-val))))))

(print "   Max of " test-list ": " (find-max test-list) #\Newline)
