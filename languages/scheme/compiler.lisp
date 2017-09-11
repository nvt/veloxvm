; Copyright (c) 2012-2017, RISE SICS AB
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. Neither the name of the Institute nor the names of its contributors
;    may be used to endorse or promote products derived from this software
;    without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS
; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
; INSTITUTE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Author: Nicolas Tsiftes <nvt@acm.org>

(provide :compiler)

(defpackage :se.sics.vm-compiler
  (:use :common-lisp)
  (:export :compile))

(require :bit-stream (pathname "bit-stream"))
(require :fold (pathname "fold"))
(require :log (pathname "log"))
(require :scheme-lib (pathname "scheme-lib"))
(require :series (pathname "series"))

;; << Named log levels. >>>
(defconstant +log-silent+ 0)
(defconstant +log-info+ 1)
(defconstant +log-low-debug+ 5)
(defconstant +log-high-debug+ 10)

;; <<< Compiler configuration. >>>
(defconstant +output-app-debug+ nil)
(defconstant +folding-enabled+ t)
(defconstant +log-level+ +log-silent+)

;; <<< Virtual machine code definition >>>

;; Core functions supported by the VM. The index of the functions' symbol
;; names correspond to an instruction code in the VM bytecode format.
(defconstant vm-symbol-list
;; Mathematics functions.
  '(+ - * / gcd lcm numerator denominator quotient remainder modulo
    = /= < <= > >= zero?
;; Primitive functions.
    bind return begin if define set! and or apply quote
    number? integer? rational? real? complex? exact? inexact? procedure?
    boolean? port? not eq? eqv? equal?
;; System functions.
    system-info load-program import get-devices print random time
    get-programs program-info exit
;; List functions.
    list cons push pop car cdr list-ref list-tail append remove reverse
    length null? list? pair? set-car! set-cdr! memq memv member
    assq assv assoc
;; Higher-order list functions.
    map filter for-each reduce count
;; Character functions.
    char? char-compare char-class char->integer integer->char
    char-upcase char-downcase
;; String functions.
    make-string string string? string-length string-ref
    string-set! string->list list->string vector->string string-fill!
    string-compare substring string-append string-copy string-split
    number->string string->number
;; Exception and condition functions.
    guard raise
;; Thread functions.
    thread-create! thread-fork! thread-id thread-join!
    thread-sleep! thread-specific thread-specific-set!
    thread-terminate! thread-yield! thread-stats
;; Mutex functions.
    mutex? make-mutex mutex-name mutex-specific mutex-specific-set!
    mutex-state mutex-lock! mutex-unlock!
;; Vector functions.
    make-vector vector vector? buffer? vector-merge vector-length vector-ref
    vector-set! vector->list list->vector vector-fill! make-buffer
    buffer-append
;; Input/output functions.
    input-port? output-port? current-input-port current-output-port
    open-input-file open-output-file close-input-port close-output-port
    read-char read peek-char eof-object? char-ready? write-char write
    display with-input-from-file with-output-to-file
;; Internet socket functions.
    make-client make-server peer-name accept-client
    incoming-client? addr->string resolve-hostname
; Floating-point mathematics functions.
    floor ceiling round truncate exp log sin cos tan asin acos atan
    sqrt expt exact-to-inexact inexact-to-exact
;; Continuations functions.
    call-with-current-continuation values call-with-values dynamic-wind eval
;; Bit manipulation functions.
    bit-and bit-or bit-invert bit-not bit-xor bit-shift
;; Data packet functions.
    construct-packet deconstruct-packet))

;; The following values are stored in the first two bytes of a VM app
;; to be able to identify the app.
(defconstant vm-file-magic '(94 181))

;; Bytecode format version.
(defconstant vm-bytecode-version 1)

;; VM bytecode tokens.
(defconstant vm-token-atom 0)
(defconstant vm-token-form 1)

;; VM form types.
(defconstant vm-form-inline 0)
(defconstant vm-form-lambda 1)
(defconstant vm-form-ref    2)

;; VM object types.
(defconstant vm-type-boolean   0)
(defconstant vm-type-integer   1)
(defconstant vm-type-rational  2)
(defconstant vm-type-real      3)
(defconstant vm-type-string    4)
(defconstant vm-type-symbol    5)
(defconstant vm-type-character 6)

;; The lengths of various fields in the VM bytecode.
(defconstant vm-length-token          1)
(defconstant vm-length-form-type      2)
(defconstant vm-length-type           3)
(defconstant vm-length-embedded       4)
(defconstant vm-length-expr           5)
(defconstant vm-length-short-expr-id  4)
(defconstant vm-length-expr-id       12)

(defconstant vm-string-limit        256)
(defconstant vm-symbol-limit      16384)

;; The depth in the expression tree is kept for each compiled expression,
;; so we know when to remove expressions folded into constants at the top
;; level of the tree.
(defvar *EXPR-LEVEL* -1)

(declaim (ftype function vm-translate-atom compile-expr))

;; Conditions.

(define-condition compiler-error (error)
  ((reason :initarg :reason :initform "unknown reason" :reader reason)
   (token :initarg :token :initform "" :reader token))
  (:report (lambda (condition stream)
	     (format stream "Compiler stopped! ~A: ~@(~A~)."
		     (token condition) (reason condition))))
  (:documentation "General compiler error."))

;; Adjust the Common Lisp reader to be able to read Scheme tokens.
(set-dispatch-macro-character #\# #\t
			      #'(lambda (stream char1 char2)
				  (declare (ignore stream char1 char2))
				  t))
(set-dispatch-macro-character #\# #\f
			      #'(lambda (stream char1 char2)
				  (declare (ignore stream char1 char2))
				  nil))

(defun generate-zeroes (result remaining-zeroes)
  (if (> remaining-zeroes 0)
      (generate-zeroes (cons #\0 result) (1- remaining-zeroes))
      result))

(defun get-element-index (lst counter elem)
  (if (null lst)
      nil
      (if (equal (car lst) elem)
	  counter
	  (get-element-index (cdr lst) (1+ counter) elem))))

(defun expand-ipv6-address (result orig-addr addr double-dots)
  (log-write +log-high-debug+ "expand ~a~%" addr)
  (if addr
    (if (equal (car addr) #\:)
	; We only take action if there is a subsequent dot in the address.
	(if (equal (cadr addr) #\:)
	    (if double-dots
		(error 'compiler-error :reason "Invalid IPv6 address"
				       :token orig-addr)
	      (progn
		(push #\Z result)
		(expand-ipv6-address result orig-addr (cddr addr) t)))
	  (expand-ipv6-address result orig-addr (cddr addr) double-dots))
      (progn
	(push (car addr) result)
	(expand-ipv6-address result orig-addr (cdr addr) double-dots)))
    (let ((expansion-place (get-element-index result 0 #\Z)))
      (if expansion-place
	(let ((needed-zeroes (- 16 (length result))))
	  (log-write +log-low-debug+ "current result: ~a~%" result)
	  (log-write +log-low-debug+ "first seq: ~a~%"
		     (subseq result 0 expansion-place))
	  (log-write +log-low-debug+ "second seq: ~a~%"
		     (generate-zeroes nil needed-zeroes))
	  (log-write +log-low-debug+ "third seq: ~a~%"
		     (nthcdr (1+ expansion-place) result))
	  (append (subseq result 0 expansion-place)
		  (generate-zeroes nil needed-zeroes)
		  (nthcdr (1+ expansion-place) result)))
	result))))

(defun hexchar-to-integer (ch)
  (let* ((hex-map '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3)
		  (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7)
		  (#\8 . 8) (#\9 . 9) (#\a . 10) (#\b . 11)
		  (#\c . 12) (#\d . 13) (#\e . 14) (#\f . 15)))
	 (code (cdr (assoc ch hex-map))))
    (log-write +log-high-debug+ "convert ~a to ~a~%" ch code)
    (if (null code)
	(if (not (equal ch #\:))
	    (error 'compiler-error
		   :reason "Invalid IP address character" :token ch)
	    ch)
        code)))

;; IPv6 address representation; e.g., #IP(fe80::1)
(defun ip-reader (stream char1 char2)
  (declare (ignore char2))
  (let ((addr-list (list)))
    (let ((ch (read-char stream nil)))
      (unless (equal ch #\()
	(error 'compiler-error
	       :reason "Invalid macro sequence" :token (cons char1 ch))))
    (loop for ch = (read-char stream nil)
	  while (and ch (char/= ch #\)))
          do (push (hexchar-to-integer ch) addr-list))
    (setq addr-list (expand-ipv6-address '() addr-list addr-list nil))
    (format t "read list: ~a~%" addr-list)
    `(VECTOR ,@addr-list)))

(set-dispatch-macro-character #\# #\I #'ip-reader)

;; <<< Compiler parameters >>>

;; Utility functions.

(defun set-atom (type &optional (info-field 0))
  (bit-stream-write vm-token-atom vm-length-token)
  (bit-stream-write info-field vm-length-embedded)
  (bit-stream-write type vm-length-type))

(defun write-form (form-info x y)
  (bit-stream-write x 1)
  (bit-stream-write form-info y))

(defmacro set-form (form-type form-info)
  `(progn
     (bit-stream-write vm-token-form vm-length-token)
     (bit-stream-write ,form-type vm-length-form-type)
     (case ,form-type
	   ;; The form is an inline form, so we write
	   ;; the number of elements in the form.
	   (,vm-form-inline
	    (bit-stream-write ,form-info vm-length-expr))
	   ;; The form is located elsewhere, so use its ID as a reference.
	   ((,vm-form-lambda ,vm-form-ref)
	    (if (<= (integer-length ,form-info) vm-length-short-expr-id)
		(write-form ,form-info 1 vm-length-short-expr-id)
	        (write-form ,form-info 0 vm-length-expr-id)))
	   (otherwise
	    (error 'compiler-error :reason "Invalid form" :token ,form-type)))))

(defun bool-bit (condition)
  (if condition 1 0))

(defun generate-dst-name (filename)
  (make-pathname :type "vm" :defaults filename))

(defun generate-debug-name (filename)
  (make-pathname :type "vmd" :defaults filename))

(defun generate-vm-symbols (series)
  (dolist (sym vm-symbol-list)
    (insert-series series (symbol-name sym)))
  (log-write +log-low-debug+ "Generated ~d symbol~:p~%"
	     (series-counter series)))

(defun symbol-existsp (symbol)
  "This predicate is true if the symbol exists either in the common table,
   or in the local table."
  (and (or (get-series-id 'symbols symbol)
	   (get-series-id 'local-symbols symbol))
       t))

(defun get-symbol-info (symbol)
  "Returns the scope and the numeric ID of a symbol. If the symbol does not
   exist, nil is returned."
  (let ((symbol-lang-id (get-series-id 'symbols symbol))
	(symbol-app-id (get-series-id 'local-symbols symbol)))
    (cond
     (symbol-lang-id
      (values 0 symbol-lang-id))
     (symbol-app-id
      (values 1 symbol-app-id))
     (t
      nil))))

(defun register-string (string)
  (unless (get-series-id 'strings string)
    (insert-series 'strings string))
  (let ((value (get-series-id 'strings string)))
    (when (> value vm-string-limit)
	(error 'compiler-error
	       :reason "Cannot store more than 256 strings"))
    (log-write +log-low-debug+ "String \"~a\" has ID ~a~%" string value)
    (bit-stream-write value 8)))

(defun integer-byte-length (value)
  "Returns the number of bytes needed to store an integer."
  (multiple-value-bind (q) (ceiling (/ (integer-length value) 8))
		       (if (zerop q) 1 q)))

;; Functions for translating source code to VM byte code.

(defun vm-translate-boolean (elem)
  (set-atom vm-type-boolean (bool-bit elem)))

(defun vm-translate-integer (elem)
  (let* ((value (abs elem))
	 (bytes-required (integer-byte-length value)))
    (set-atom vm-type-integer
	      (logior (ash (bool-bit (minusp elem)) 3)
		      bytes-required))
    (bit-stream-write value (* 8 bytes-required))))

(defun vm-translate-rational (elem)
  (set-atom vm-type-rational)
  (vm-translate-atom (numerator elem))
  (vm-translate-atom (denominator elem)))

(defun vm-translate-character (elem)
  (set-atom vm-type-character)
  (bit-stream-write (char-int elem) 8))

(defun vm-translate-string (elem)
  (set-atom vm-type-string)
  (register-string elem))

(defun check-symbol-names (syms)
  (mapcar #'(lambda (name)
	      (when (member name vm-symbol-list)
		(error 'compiler-error
		       :reason "Cannot override core symbol"
		       :token name)))
	  syms))

(defun add-symbol (name)
  (unless (symbol-existsp (symbol-name name))
    (insert-series 'local-symbols (symbol-name name))))

(defun vm-translate-symbol (elem)
  (add-symbol elem)
  (multiple-value-bind (scope code) (get-symbol-info (symbol-name elem))
		       (unless code
			 (error 'compiler-error
				:reason "Unable to use the symbol"
				:token elem))
		       (when (>= code vm-symbol-limit)
			 (error 'compiler-error
				:reason "Too high symbol ID"
				:token elem))
		       (set-atom vm-type-symbol)
		       (bit-stream-write scope 1)
		       (if (> code 63)
			   (progn
			     (bit-stream-write 1 1)
			     (bit-stream-write code 14))
			   (progn
			     (bit-stream-write 0 1)
			     (bit-stream-write code 6)))
		       (log-write +log-low-debug+
				  "Reference to symbol ~a with code ~a~%"
				  elem code)))

(defun vm-translate-atom (elem)
  (funcall
   (typecase elem
     (boolean #'vm-translate-boolean)
     (integer #'vm-translate-integer)
     (rational #'vm-translate-rational)
     (symbol #'vm-translate-symbol)
     (character #'vm-translate-character)
     (string #'vm-translate-string)
     (t (error 'compiler-error :reason "Unrecognized token" :token elem)))
   elem))

(defun vm-translate-sequence (elem &optional (form-type vm-form-ref))
  (let ((expr-bit-stream (make-bit-stream)))
    (with-bit-stream expr-bit-stream
      (compile-expr elem))
    ; Reuse an existing form if it the matches the one being compiled.
    (let ((expr-id (or (get-series-id 'expressions expr-bit-stream)
		       (insert-series 'expressions expr-bit-stream))))
      (set-form form-type expr-id)
      (when +output-app-debug+
        (insert-series 'debug-statements (format nil "~a" elem)))
      (log-write +log-low-debug+
		 "Expression ~a is ~a bit~:p long and has ID ~a and type ~a~%"
		 elem (bit-stream-length expr-bit-stream) expr-id form-type))))

(defun vm-translate-lambda (args form)
  (check-symbol-names args)
  (mapcar #'add-symbol args)
  ; Does the lambda form contain multiple sub-forms?
  (if (and (listp form) (> (length form) 1))
      (vm-translate-sequence `(bind ,@args (begin ,@form)) vm-form-lambda)
      (vm-translate-sequence `(bind ,@args ,@form) vm-form-lambda)))

(defmacro form-with-key-p (expr key)
  (let ((form (gensym)))
    `(let ((,form ,expr))
       (and (listp ,form) (eq (first ,form) ,key)))))

(defun lambdap (expr)
  (form-with-key-p expr 'LAMBDA))

(defun quotep (expr)
  (form-with-key-p expr 'QUOTE))

(defun funcdefp (expr)
  (and (form-with-key-p expr 'DEFINE)
       (= (length expr) 3)
       (symbolp (second expr))
       (lambdap (third expr))))

(defun vardefp (expr)
  (and (form-with-key-p expr 'DEFINE)
       (= (length expr) 3)
       (not (lambdap (third expr)))))

(defun named-letp (expr)
  (and (form-with-key-p expr 'LET)
       (symbolp (second expr))
       (>= (length expr) 4)))

(defun vm-translate-form (expr)
  (let ((elem (car expr))
	(more (cdr expr)))
    (if (lambdap expr)
	(vm-translate-lambda (first more) (rest more))
        (progn 
	  (cond
	   ((atom elem)
	    (vm-translate-atom elem))
	   ((lambdap elem)
	    (vm-translate-form elem))
	   ((listp elem)
	    (vm-translate-sequence elem))
	   (t
	    (error 'compiler-error :reason "Unrecognized token" :token elem)))
	  (when more
	    (vm-translate-form more))))))

(defun rewrite-expr (expr)
  (cond
    ((named-letp expr)
     (rewrite-named-let expr))
    ((quotep expr)
     (rewrite-quote expr))
    ((funcdefp expr)
     (rewrite-function expr))
    (t
     (rewrite-dispatcher expr))))

(defun check-semantics (expr)
  (when (and (form-with-key-p expr 'DEFINE)
             (= (length expr) 3)
	     (not (or (funcdefp expr) (vardefp expr))))
    (error 'compiler-error
	   :reason
	   "Definition must be of either an object or a lambda expression"
	   :token expr))
  (when (and (lambdap expr) (not (= 3 (length expr))))
    (error 'compiler-error
	   :reason
	   "Malformed lambda expression"
	   :token expr)))

;; compile-expr compiles top-level expressions in the compilation unit.
;; The VM will execute such expressions sequentially.

(defun compile-expr (expr)
  "Compile a Scheme expression and store the result in a bit stream."
  (check-semantics expr)
  (let* ((*EXPR-LEVEL* (1+ *EXPR-LEVEL*))
	 (new-expr (rewrite-expr expr))
	 (folded-expr (if +folding-enabled+
			  (fold-expr new-expr)
			new-expr)))
    (unless (equal expr new-expr)
      (log-write +log-low-debug+ "Rewrite ~a => ~a~%" expr new-expr))
    (unless (equal new-expr folded-expr)
      (log-write +log-low-debug+ "Optimize ~a => ~a~%" new-expr folded-expr))
    (log-write +log-low-debug+ "Compiling expression ~a at level ~a~%"
	       folded-expr *EXPR-LEVEL*)
    (if (atom folded-expr)
	(when (> *EXPR-LEVEL* 0)
	  (vm-translate-atom folded-expr))
	(progn 
	  (set-form vm-form-inline (length folded-expr))
	  (vm-translate-form folded-expr)))))

(defun save-expr (bs file)
  "Write a single bit stream to a file. The bit stream is prepended by
   a single byte, whose value is the length of the bit stream measured
   in full bytes."
  (let ((length (/ (bit-stream-length bs) 8)))
    (assert (integerp length))
    (write-byte length file)
    (with-bit-stream bs (bit-stream-save file))))

(defun generate-program (filename bit-stream series-set)
  "Generates a VM file from bytecode, a symbol table, and a string table."
  (with-open-file (dst-file (generate-dst-name filename)
			    :direction :output :if-exists :rename-and-delete
			    :element-type '(unsigned-byte 8))
    (loop for code in vm-file-magic
	  do (write-byte code dst-file))
    (write-byte vm-bytecode-version dst-file)
    (dolist (series series-set)
      (commit-series series dst-file))
    (commit-series 'expressions dst-file 'save-expr)
    (when +output-app-debug+
      (with-open-file (debug-file (generate-debug-name filename)
				  :direction :output
				  :if-exists :rename-and-delete
				  :element-type '(unsigned-byte 8))
		      (commit-series 'debug-statements debug-file)))
    (log-write +log-low-debug+
	       "Writing ~a byte~:p of program code at offset ~a~%"
	       (integer-byte-length (bit-stream-value bit-stream))
	       (file-length dst-file))
    (log-write +log-low-debug+
	       "Total program size: ~a byte~:p~%" (file-length dst-file))))

;; Main compilation function.

(defun run-compiler (filename)
  "Compiles Scheme source code into VM program code."
  (log-set-level +log-level+)
  (log-set-prefix "| ")
  (log-set-stream t)
  (mapcar #'create-series '(strings symbols local-symbols debug-statements))
  (create-series 'expressions
		 :comparator 'equalp
		 :element-type 'bit-stream)
  (generate-vm-symbols 'symbols)
  (setq *EXPR-LEVEL* -1)
  (let ((bs (make-bit-stream))
	(*read-eval* nil))
    (insert-series 'expressions bs)
    (bit-stream-set bs)
    (with-open-file (src-file filename)
		    (loop for expr = (read src-file nil)
			  while expr do (compile-expr expr))
		    (generate-program filename bs '(strings local-symbols)))))
