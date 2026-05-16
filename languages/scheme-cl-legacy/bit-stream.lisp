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

(provide :bit-stream)

(defstruct bit-stream (length 0) (value 0))

(defun bit-stream-equal (bs1 bs2)
  (equalp bs1 bs2))

(defmacro with-bit-stream (bs &body body)
  (let ((old-bs (gensym)))
    `(let ((,old-bs (bit-stream-get)))
       (unwind-protect
	 (progn
	   (bit-stream-set ,bs)
	   ,@body)
	 (bit-stream-set ,old-bs)))))

(let ((bs nil))
  (defun bit-stream-set (new-bs)
    (setq bs new-bs))
  (defun bit-stream-get ()
    bs)
  (defun bit-stream-write (value &optional (bits (integer-length value)))
    (assert (>= bits 0))
    (incf (bit-stream-length bs) bits)
    (setf (bit-stream-value bs)
	  (logior (ash (bit-stream-value bs) bits) value)))
  (defun bit-stream-clear ()
    (setf (bit-stream-length bs) 0)
    (setf (bit-stream-value bs) 0))
  (defun bit-stream-save (stream)
    (assert (zerop (mod (bit-stream-length bs) 8)))
    (loop for i from (- (bit-stream-length bs) 8) downto 0 by 8
       do (write-byte (ldb (byte 8 i) (bit-stream-value bs)) stream)))
  (defun bit-stream-append (other-bs)
    (bit-stream-write (bit-stream-value other-bs)
		      (bit-stream-length other-bs))))

