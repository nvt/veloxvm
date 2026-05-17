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

(provide :fold)

(defconstant math-symbol-list '(+ - * / gcd lcm numerator denominator
				  quotient remainder modulo = /= < <= > >=
				  sqrt sin cos tan))
(defconstant pred-symbol-list '(number? integer? rational? real?
					complex? string?))
(defconstant cmp-symbol-list '(zero? eq?))
(defconstant logic-symbol-list '(and or not))
(defconstant string-symbol-list '(string-append string-compare string-length
				  substring string->number number->string))
(defconstant name-map-assoc '((remainder . rem)
			     (modulo . mod)
			     (bit-shift . ash)
			     (bit-and . logand)
			     (bit-or . logior)
			     (bit-xor . logxor)
			     (number? . numberp)
			     (integer? . integerp)
			     (rational? . rationalp)
			     (real? . realp)
			     (complex? . complexp)
			     (string? . stringp)
			     (zero? . zerop)
			     (string-append . string-concat)
			     (string-length . length)
			     (substring . subseq)))

(defun scheme-to-lisp (proc)
  (let ((result (assoc proc name-map-assoc)))
    (if result
	(cdr result)
        proc)))

(defun fold-math-expr (proc args)
  ; Fold mathematical expressions that have number arguments.
  (if (and args (every #'numberp args))
      ; Do not fold division by zero.
      (if (and (equalp proc '/) (some #'zerop args))
	  (cons proc args)
	  (apply proc args))
      (cons proc args)))

(defun fold-pred-expr (proc args)
  ; Fold predicate expressions that have atomic arguments.
  (if (and (= (length args) 1)
	   (atom (first args))
	   (not (symbolp (first args))))
      (apply proc args)
      (cons proc args)))

; AND returns false (#f) if any argument is false. Otherwise, the last
; argument is returned.
(defun fold-and-expr (proc args)
  (if (some #'(lambda (arg) (null arg)) args)
      nil
      (cons proc args)))

; OR returns the first non-false argument; i.e., the truth value (#t) or a
; value of any other type.
(defun fold-or-expr (proc args)
  (if (null args)
      nil
      (if (null (car args))
	  (fold-or-expr proc (cdr args))
	  (cons proc args))))

(defun fold-logic-expr (proc args)
  (if (and (> (length args) 1)
	   (every #'(lambda (elem)
		      (or (atom elem)
			  (not (symbolp elem))
			  (eq elem t)
			  (eq elem nil)))
		  args))
	   ; We cannot apply the special functions AND and OR, so they have
           ; to be processed manually.
	   (cond
	    ((eq proc 'AND) (fold-and-expr proc args))
	    ((eq proc 'OR) (fold-or-expr proc args))
	     (t (apply proc args)))
	   (cons proc args)))

(defun fold-string-cmp-expr (proc args)
  (if (and (= (length args) 2)
	   (every #'stringp args))
      (let ((str1 (first args))
	    (str2 (second args)))
	(cond
	 ((string= str1 str2) 0)
	 ((string< str1 str2) -1)
	 (t 1)))
      (cons proc args)))

(defun fold-string->number (proc args)
  (if (and (>= (length args) 1)
	   (stringp (first args)))
      (let ((str (first args))
	    (radix (second args)))
	(if (null radix)
	    (parse-integer str)
	    (if (numberp radix)
		(parse-integer str :radix radix)
	        (cons proc args))))
      (cons proc args)))

(defun fold-number->string (proc args)
  (if (and (>= (length args) 1)
	   (every #'numberp args))
      (let ((num (first args))
	    (radix (second args)))
	(if (null radix)
	    (write-to-string num)
	    (write-to-string num :base radix)))
      (cons proc args)))

(defun fold-string-expr (proc args)
  (cond
   ((eq proc 'STRING-COMPARE)
    (fold-string-cmp-expr proc args))
   ((eq proc 'STRING->NUMBER)
    (fold-string->number proc args))
   ((eq proc 'NUMBER->STRING)
    (fold-number->string proc args))
   (t
    (if (and (> (length args) 0)
	     (every #'(lambda (elem)
			(and (atom elem)
			     (not (symbolp elem)))) args))
	(apply proc args)
        (cons proc args)))))

(defun fold-expr (expr)
  (if (listp expr)
      (let* ((processed-expr (mapcar #'fold-expr expr))
	     (proc (scheme-to-lisp (car processed-expr)))
	     (args (cdr processed-expr)))
      	(cond
         ((and (eq proc 'BEGIN) (= (length args) 1))
	  (car args))
	 ((and (eq proc 'IF)
	       (= (length args) 3)
	  (if (eq (first args) nil)
	      (third args)
	      (if (equalp (first args) t)
		  (second args)
		  processed-expr))))
	 ((member proc math-symbol-list)
	  (fold-math-expr proc args))
	 ((member proc pred-symbol-list)
	  (fold-pred-expr proc args))
	 ((member proc logic-symbol-list)
	  (fold-logic-expr proc args))
	 ((member proc string-symbol-list)
	  (fold-string-expr proc args))
	 (t processed-expr)))
      expr))
