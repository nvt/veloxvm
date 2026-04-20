;; String and Vector Operations
;; Shows: string manipulation, vector operations, character operations

(print "=== String Operations ===" #\Newline #\Newline)

;; 1. String construction and access
(print "1. String basics:" #\Newline)
(define str1 "Hello, VeloxVM!")
(print "   String: " str1 #\Newline)
(print "   Length: " (string-length str1) #\Newline)
(print "   Char at 0: " (string-ref str1 0) #\Newline)
(print "   Char at 7: " (string-ref str1 7) #\Newline #\Newline)

;; 2. String comparison
(print "2. String comparison:" #\Newline)
(print "   \"abc\" = \"abc\": " (string=? "abc" "abc") #\Newline)
(print "   \"abc\" < \"xyz\": " (string<? "abc" "xyz") #\Newline #\Newline)

;; 3. String concatenation
(print "3. String operations:" #\Newline)
(define greeting "Hello")
(define name "World")
(print "   Append: " (string-append greeting ", " name "!") #\Newline)
(print "   Make-string: " (make-string 5 #\*) #\Newline #\Newline)

(print "=== Vector Operations ===" #\Newline #\Newline)

;; 4. Vector construction
(print "4. Vector basics:" #\Newline)
(define vec1 (vector 10 20 30 40 50))
(define vec2 (make-vector 5 0))
(print "   vec1: " vec1 #\Newline)
(print "   vec2 (initialized to 0): " vec2 #\Newline)
(print "   Length of vec1: " (vector-length vec1) #\Newline #\Newline)

;; 5. Vector access and modification
(print "5. Vector access and mutation:" #\Newline)
(print "   vec1[2]: " (vector-ref vec1 2) #\Newline)
(vector-set! vec2 0 100)
(vector-set! vec2 1 200)
(print "   vec2 after modifications: " vec2 #\Newline #\Newline)

;; 6. Vector to list conversion
(print "6. Vector/List conversion:" #\Newline)
(define list-from-vec (vector->list vec1))
(define vec-from-list (list->vector (list 'a 'b 'c 'd)))
(print "   Vector->List: " list-from-vec #\Newline)
(print "   List->Vector: " vec-from-list #\Newline #\Newline)

;; 7. Working with vectors - sum example
(print "7. Computing with vectors:" #\Newline)
(define (vector-sum vec)
  (let loop ((i 0)
             (sum 0))
    (if (= i (vector-length vec))
        sum
        (loop (+ i 1)
              (+ sum (vector-ref vec i))))))

(print "   Sum of vec1: " (vector-sum vec1) #\Newline)
