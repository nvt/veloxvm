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

(let ((freq (make-array 256 :element-type 'integer :initial-element 0))
      (total 0))
  (defun entropy-reset ()
    (setf total 0)
    (fill freq 0))
  (defun entropy-count (byte-value &optional (amount 1))
    (assert (and (>= byte-value 0) (< byte-value 256)))
    (incf total amount)
    (incf (aref freq byte-value) amount))
  (defun entropy-calc ()
    (if (zerop total)
	0
	(- (loop for byte-freq across freq sum
		(let ((p (/ byte-freq total)))
		  (if (zerop p)
		      0 ; lim_{p->0+} p*log(p) = 0.
		      (* p (log p 2)))))))))

(defun file-entropy (name)
  (entropy-reset)
  (with-open-file (file name :element-type 'unsigned-byte)
    (loop for value = (read-byte file nil)
	 while value do (entropy-count value)))
  (entropy-calc))
