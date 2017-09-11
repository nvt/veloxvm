(define testlist (list 1 2 3 (list 4 5) 6 7))
(print (car testlist) #\Newline)
(print (cdr testlist) #\Newline)
(print (cons "abc" (cons 0 testlist)) #\Newline)
(print "Length: " (length testlist) #\Newline)
(memv 3 testlist);
(memv 8 testlist);
(memq 6 testlist);
(memq (list 4 5) testlist)
(member (list 4 5) testlist)
