(current-input-port)
(current-output-port)

;; counter: Generate a list of numbers ranging from 1 to @limit.
(define counter
  (lambda (@list @limit)
    (if (> @limit 0)
	(counter (cons @limit @list) (- @limit 1))
	@list)))

(define numbers (counter (list) 10))
(define chars (list))

;; my-reader: Read a character from the current input port, and add
;; the character to chars.
(define my-reader
  (lambda (@x)
    (define chars (cons (read-char) chars))))

(print "Please enter " (length numbers) " characters." #\Newline)
(for-each my-reader numbers)

(print "Read characters: " (list->string (reverse chars)) #\Newline)
