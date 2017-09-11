(define copy-file
  (lambda (in-port out-port)
    (let ((buf (read in-port)))
      (when (vector? buf)
	    (write buf out-port)
	    (copy-file in-port out-port)))))

(define in-file (open-input-file "compile.sh"))
(define out-file (open-output-file "copy.sh"))

(copy-file in-file out-file)

(close-input-port in-file)
(close-input-port out-file)
