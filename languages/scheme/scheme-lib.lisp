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

(provide :scheme-lib)

(declaim (ftype function lambdap rewrite-let))

(defconstant rewriters '((ABS . rewrite-abs)
                         (BEGIN . rewrite-begin)
			 (CAAR . rewrite-caar)
			 (CADR . rewrite-cadr)
			 (CADDR . rewrite-caddr)
			 (CALL/CC . rewrite-call/cc)
			 (CASE . rewrite-case)
			 (CHAR=? . rewrite-char=?)
			 (CHAR<? . rewrite-char<?)
			 (CHAR>? . rewrite-char>?)
			 (CHAR<=? . rewrite-char<=?)
			 (CHAR>=? . rewrite-char>=?)
			 (CHAR-CI=? . rewrite-char-ci=?)
			 (CHAR-CI<? . rewrite-char-ci<?)
			 (CHAR-CI>? . rewrite-char-ci>?)
			 (CHAR-CI<=? . rewrite-char-ci<=?)
			 (CHAR-CI>=? . rewrite-char-ci>=?)
			 (CHAR-ALPHABETIC? . rewrite-char-alphabetic?)
			 (CHAR-NUMERIC? . rewrite-char-numeric?)
			 (CHAR-WHITESPACE? . rewrite-char-whitespace?)
			 (CHAR-UPPER-CASE? . rewrite-char-upper-case?)
			 (CHAR-LOWER-CASE? . rewrite-char-lower-case?)
			 (COND . rewrite-cond)
			 (DO . rewrite-do)
			 (DISPLAY . rewrite-display)
			 (EVEN? . rewrite-evenp)
			 (GUARD . rewrite-guard)
			 (LET . rewrite-let)
			 (LET* . rewrite-let*)
			 (LETREC . rewrite-letrec)
			 (MAX . rewrite-max)
			 (MIN . rewrite-min)
			 (NEWLINE . rewrite-newline)
			 (NEGATIVE?  . rewrite-negativep)
			 (ODD? . rewrite-oddp)
			 (POSITIVE? . rewrite-positivep)
			 (PRINTLN . rewrite-println)
			 (STRING=? . rewrite-string=?)
			 (STRING<? . rewrite-string<?)
			 (STRING<=? . rewrite-string<=?)
			 (STRING>? . rewrite-string>?)
			 (STRING>=? . rewrite-string>=?)
			 (UNLESS . rewrite-unless)
			 (WHEN . rewrite-when)
			 (ZERO? . rewrite-zerop)))

(let ((internal-id 0))
  (defun next-internal-symbol ()
    (intern (format nil "$~a" (incf internal-id)))))

(defun rewrite-dispatcher (expr)
  (cond ((stringp expr)
	 expr)
	((vectorp expr)
	 (let ((result (list)))
	   (loop for i across expr
		 do (push i result))
	   (setq result (nreverse result))
	   (push 'VECTOR result)
	   result))
	((listp expr)
	 (let* ((name (car expr))
		(pair (assoc name rewriters)))
	   (if pair
	       (let ((rewriter (cdr pair)))
		 (funcall rewriter expr))
	     expr)))
	(t expr)))

;; Rewrite rules for Scheme primitive procedures.

(defun rewrite-begin (expr)
  (if (= (length (cdr expr)) 1)
      (rewrite-dispatcher (cadr expr))
      expr))

(defun rewrite-do (expr)
  (let ((thunk (next-internal-symbol))
	(variables (cadr expr))
	(test-expr (caddr expr))
	(commands (cdddr expr))
	(var-names '())
	(var-inits '())
	(var-steps '()))
    (dolist (var variables)
      (destructuring-bind (name init &optional step) var
	(push name var-names)
	(push init var-inits)
	(push (if (null step) name step) var-steps)))
    `(BEGIN (DEFUN ,thunk
	      (LAMBDA (,@var-names)
	       (IF ,@test-expr
		   (BEGIN ,@commands (,thunk ,@var-steps)))))
       (,thunk ,@var-inits))))

(defun rewrite-quote (expr)
  (if (listp (cadr expr))
      `(LIST ,@(cadr expr))
      expr))

(defun rewrite-named-let (expr)
  (let* ((name (second expr))
	(args (third expr))
	(arg-names (mapcar #'(lambda (a) (car a)) args))
	(arg-values (mapcar #'(lambda (a) (cadr a)) args))
	(body (fourth expr)))
    `(BEGIN (DEFINE ,name (LAMBDA (,@arg-names) ,body))
	    (,name ,@arg-values))))

(defun rewrite-function (expr)
  (let* ((name (second expr))
	 (lambda-form (third expr)))
    (list 'DEFINE name
	  (if (lambdap lambda-form)
	      lambda-form
	    (list 'LAMBDA lambda-form lambda-form)))))

;; Rewrite rules for Scheme library procedures.
(defun rewrite-rule (rule)
  (when (< (length rule) 2)
    (error 'compiler-error :reason "Invalid COND rule" :token rule))
  (destructuring-bind (condition action)
		      (if (> (length rule) 2)
			  `(,(car rule) (BEGIN ,@(cdr rule)))
			rule)
		      (if (equalp condition 'else)
			  action
			`(IF ,condition ,action))))

(defun merge-rules (if-left if-right)
  (append if-left (list if-right)))

(defun rewrite-cond (expr)
  (let* ((rules (cdr expr))
	 (if-expressions (mapcar #'rewrite-rule rules)))
    (reduce #'merge-rules if-expressions :from-end t)))

(defun rewrite-case (expr)
  (let ((thunk (next-internal-symbol))
	(eval-obj (second expr))
	(rules (cddr expr))
	(cond-rules '(COND)))
    (dolist (rule rules)
      (if (equalp (car rule) 'ELSE)
	(push `(ELSE ,(cadr rule)) cond-rules)
	(push `((MEMV ,thunk (LIST ,@(car rule))) ,(cadr rule)) cond-rules)))
    (setq cond-rules (nreverse cond-rules))
    (rewrite-let `(LET ((,thunk ,eval-obj)) ,(rewrite-cond cond-rules)))))

(defun rewrite-let (expr)
  (let ((binds (second expr)) (form (cddr expr)))
    (let (symbols values)
      (mapcar #'(lambda (bind)
		  (push (first bind) symbols)
		  (push (second bind) values)) binds)
      (setq symbols (nreverse symbols))
      (setq values (nreverse values))
      (let ((lambda-expr (list)))
	(mapcar #'(lambda (sub-form) (push sub-form lambda-expr)) form)
	(setq lambda-expr (nreverse lambda-expr))
	(push symbols lambda-expr)
	(push 'LAMBDA lambda-expr)
	(cons lambda-expr values)))))

(defun rewrite-println (expr)
  `(PRINT ,@(cdr expr) #\Newline))

(defun rewrite-zerop (expr)
  `(= ,(cadr expr) 0))

(defun rewrite-positivep (expr)
  `(> ,(cadr expr) 0))

(defun rewrite-negativep (expr)
  `(< ,(cadr expr) 0))

(defun rewrite-oddp (expr)
  `(= (REMAINDER ,(cadr expr) 2) 1))

(defun rewrite-evenp (expr)
  `(= (REMAINDER ,(cadr expr) 2) 0))

(defun rewrite-guard (expr)
  (let ((cond-obj (caadr expr))
	(conditions `(COND ,@(cdadr expr)))
	(expressions `(BEGIN ,@(cddr expr))))
    `(GUARD ,cond-obj ,conditions ,expressions)))

(defun rewrite-max (expr)
  (LET ((a (second expr))
	(b (third expr)))
       `(IF (< ,a ,b) ,b ,a)))

(defun rewrite-min (expr)
  (LET ((a (second expr))
	(b (third expr)))
       `(IF (< ,a ,b) ,a ,b)))

(defun rewrite-abs (expr)
  `(IF (< ,(cadr expr) 0) (- ,(cadr expr)) ,(cadr expr)))

(defun rewrite-caar (expr)
  `(CAR (CAR ,(cadr expr))))

(defun rewrite-cadr (expr)
  `(CAR (CDR ,(cadr expr))))

(defun rewrite-caddr (expr)
  `(CAR (CDR (CDR ,(cadr expr)))))

(defun rewrite-call/cc (expr)
  `(CALL-WITH-CURRENT-CONTINUATION ,@(cdr expr)))

(defun rewrite-let* (expr)
  (let* ((defs (second expr))
	 (body-length (length (cddr expr)))
	 (body (if (> body-length 1)
		   `(begin ,@(cddr expr))
		   (cddr expr)))
	 (expr-list (reverse (append defs (list body))))
	 (new-expr (reduce #'(lambda (x y) `(LET (,y) ,x)) expr-list)))
    (rewrite-let new-expr)))

(defun rewrite-letrec (expr)
  (rewrite-let `(LET ,@(cdr expr))))

(defun rewrite-newline (expr)
  `(WRITE #\Newline ,@(cdr expr)))

(defun rewrite-display (expr)
  `(PRINT ,@(cdr expr)))

(defun rewrite-string=? (expr)
  `(= (STRING-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-string<? (expr)
  `(< (STRING-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-string>? (expr)
  `(> (STRING-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-string<=? (expr)
  `(<= (STRING-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-string>=? (expr)
  `(>= (STRING-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-char=? (expr)
  `(= (CHAR-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-char<? (expr)
  `(< (CHAR-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-char>? (expr)
  `(> (CHAR-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-char<=? (expr)
  `(<= (CHAR-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-char>=? (expr)
  `(>= (CHAR-COMPARE ,(second expr) ,(third expr)) 0))

(defun rewrite-char-ci=? (expr)
  (rewrite-char=?
   `(CHAR=? (CHAR-UPCASE ,(second expr)) (CHAR-UPCASE ,(third expr)))))

(defun rewrite-char-ci<? (expr)
  (rewrite-char<?
   `(CHAR<? (CHAR-UPCASE ,(second expr)) (CHAR-UPCASE ,(third expr)))))

(defun rewrite-char-ci>? (expr)
  (rewrite-char>?
   `(CHAR>? (CHAR-UPCASE ,(second expr)) (CHAR-UPCASE ,(third expr)))))

(defun rewrite-char-ci<=? (expr)
  (rewrite-char<=?
   `(CHAR<=? (CHAR-UPCASE ,(second expr)) (CHAR-UPCASE ,(third expr)))))

(defun rewrite-char-ci>=? (expr)
  (rewrite-char>=?
   `(CHAR>=? (CHAR-UPCASE ,(second expr)) (CHAR-UPCASE ,(third expr)))))

(defun rewrite-char-alphabetic? (expr)
  `(< (CHAR-CLASS ,@expr) 0))

(defun rewrite-char-numeric? (expr)
  `(= (CHAR-CLASS ,@expr) 0))

(defun rewrite-char-whitespace? (expr)
  `(= (CHAR-CLASS ,@expr) 1))

(defun rewrite-char-upper-case? (expr)
  `(= (CHAR-CLASS ,@expr) -1))

(defun rewrite-char-lower-case? (expr)
  `(= (CHAR-CLASS ,@expr) -2))

(defun rewrite-unless (expr)
  (LET ((condition (second expr))
	(body (cddr expr)))
       `(IF (NOT ,condition) (BEGIN ,@body))))

(defun rewrite-when (expr)
  (LET ((condition (second expr))
	(body (cddr expr)))
       `(IF ,condition (BEGIN ,@body))))
