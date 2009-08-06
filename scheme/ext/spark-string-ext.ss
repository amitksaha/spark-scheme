;; Some useful string functions.
;; Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.

;; Please contact Vijay Mathew Pandyalakal if you need additional 
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

(module spark-string-ext mzscheme

	(require spark-util-ext)

	;; Returns a new string which is the reverse of self.
	(define (string-reverse self)
	  (list->string (reverse (string->list self))))

	;; (string-find) and helpers.

	(define string-find
	  (case-lambda
	   ((self needle) 
	    (string-find self 0 (string-length self) 
			 needle 0 (string-length needle)))
	   ((self self-offset needle)
	    (string-find self self-offset (string-length self) 
			 needle 0 (string-length needle)))
	   ((self self-offset needle needle-offset)
	    (string-find self self-offset (string-length self) 
			 needle needle-offset (string-length needle)))
	   ((self self-offset self-len needle needle-offset)
	    (string-find self self-offset self-len
			 needle needle-offset (string-length needle)))
	   ((self self-offset self-len needle needle-offset needle-len)
	    (if (< self-offset 0)
		(set! self-offset 0))
	    (if (< needle-offset 0)
		(set! needle-offset 0))
	    (cond
	     ((and (= self-len 0)
		   (= needle-len 0))
	      0)
	     ((is-eligible-for-search self-offset needle-offset
				      self-len needle-len)
	      (let ((first (string-ref needle needle-offset))
		    (max-index (+ self-offset (- self-len needle-len)))
		    (ret -1))
		(if (>= max-index self-len)
		    (set! max-index (sub1 self-len)))
		(let loop ((i (find-first-character self self-offset 
						    first max-index)))
		  (if (>= i 0)
		      (let ((j (add1 i)))
			(if (match-rest self needle
					j
					(add1 needle-offset)
					(+ j (- needle-len 1)))
			    (set! ret (- i self-offset))
			    (loop (find-first-character 
				   self (add1 i)
				   first max-index))))))
		(if (>= ret 0)
		    (+ ret self-offset)
		    ret)))
	     (else -1)))))

	(define (is-eligible-for-search self-offset
					needle-offset
					self-len
					needle-len)
	  (cond 
	   ((or (= self-len 0)
		(= needle-len 0))
	    #f)
	   ((> self-offset self-len)
	    #f)
	   ((> needle-offset needle-len)
	    #f)
	   (else #t)))
	
	(define (find-first-character self index 
				      first-char
				      max-index)
	  (let ((ret -1))
	    (let loop ((i index))
	      (if (<= i max-index)
		  (if (not (char=? (string-ref self i) first-char))
		      (loop (add1 i))
		      (set! ret i))))
	    ret))

	(define (match-rest self needle 
			    first-char-index
			    needle-index
			    end)
	  (let ((ret #f))
	    (let loop ((j first-char-index)
		       (k needle-index))
	      (if (and (< j end)
		       (char=? (string-ref self j)
			       (string-ref needle k)))
		  (loop (add1 j) (add1 k))
		  (if (= j end)
		      (set! ret #t))))
	    ret))

	;; :~ (string-find)

	;; Reverse searches self for the occurence of needle.
	;; Returns the highest index of needle in self or -1
	;; if needle is not found.
	(define string-rfind
	  (case-lambda
	   ((self needle) 
	    (string-rfind self 0 (string-length self) 
			  needle 0 (string-length needle)))
	   ((self self-offset needle)
	    (string-rfind self self-offset (string-length self) 
			  needle 0 (string-length needle)))
	   ((self self-offset needle needle-offset)
	    (string-rfind self self-offset (string-length self) 
			  needle needle-offset (string-length needle)))
	   ((self self-offset self-len needle needle-offset)
	    (string-rfind self self-offset self-len
			  needle needle-offset (string-length needle)))
	   ((self self-offset self-len needle needle-offset needle-len)
	    (let ((rev-self (string-reverse self))
		  (rev-needle (string-reverse needle))
		  (i 0))
	      (set! i (string-find rev-self self-offset self-len
				   rev-needle needle-offset needle-len))
	      (if (>= i 0)
		  (begin
		    (set! i (- self-len i))
		    (set! i (- i needle-len))))		    
	      i))))
	;; :~ (string-rfind)

	;; Checks if a string has the given suffix.
	(define (string-ends-with? self suffix)
	  (let ((i (string-rfind self suffix)))
	    (if (>= i 0)
		(begin
		  (let ((slen (string-length suffix))
			(self-len (string-length self)))
		    (if (= (+ i slen) self-len)
			#t
			#f)))
		#f)))

	;; Checks if a string has the given prefix.
	(define (string-starts-with? self prefix)
	  (let ((i (string-find self prefix)))
	    (if (= i 0)
		#t
		#f)))

	;; Returns a new string with trailing whitespaces 
	;; removed from it's left, or self itself if there
	;; are no trailing whitespaces.
	(define (string-ltrim self)
	  (let ((len (string-length self)))
	    (cond
	     ((<= len 0) self)
	     (else
	      (let loop ((i 0) (found #f))
		(if (and (< i len)
			 (not found))
		    (if (not (char-whitespace? (string-ref self i)))
			(loop i #t)
			(loop (add1 i) #f))
		    (if found
			(substring self i)
			(cond ((>= i len) "")
			      (else self)))))))))

	;; Returns a new string with trailing whitespaces 
	;; removed from it's right, or self itself if there
	;; are no trailing whitespaces.
	(define (string-rtrim self)
	  (let ((len (string-length self)))		
	    (cond
	     ((<= len 0) self)
	     (else
	      (let loop ((i (sub1 len)) 
			 (found #f))
		(if (and (>= i 0)
			 (not found))
		    (if (not (char-whitespace? (string-ref self i)))
			(loop i #t)
			(loop (sub1 i) #f))
		    (if found
			(substring self 0 (add1 i))
			(cond ((<= i 0) "")
			      (else self)))))))))

	;; Removes trailing whitespaces.
	(define (string-trim self)
	  (let ((ret (string-ltrim self)))
	    (string-rtrim ret)))

	;; Splits the string at sep. Returns a list of tokens.
	;; sep can be a character or a list of characters.
	(define string-split
	  (case-lambda
	   ((str)
	    (string-split str null))
	   ((self sep)
	    (if (null? sep)
		(set! sep (list #\space)))
	    (let ((len (string-length self)))		
	      (let loop ((i 0) (ret '()) 
			 (idx 0))
		(if (< i len)
		    (begin
		      (if (contains? sep (string-ref self i))
			  (loop (add1 i)
				(cons (substring self idx i) ret)
				(add1 i))
			  (loop (add1 i) ret idx)))
		    (begin
		      (if (< idx len)
			  (reverse! (cons (substring self idx len) ret))
			  (reverse! ret)))))))))

	(define string-tokenizer
	  (case-lambda
	   ((str cb)
	    (string-tokenizer str cb null))
	   ((str cb sep)
	    (let ((i 0) 
		  (len (string-length str)) 
		  (idx 0))
	      (if (null? sep)
		  (set! sep (list #\space)))
	      (lambda ()
		(cond
		 ((< i len)
		  (let loop ()
		    (if (< i len)
			(begin
			  (if (contains? sep (string-ref str i))
			      (begin 
				(cb (substring str idx i))
				(set! i (add1 i))
				(set! idx i)
				#t)
			      (begin
				(set! i (add1 i))
				(loop))))
			#t)))
		 ((= i len)
		  (cb (substring str idx len))
		  (set! i (add1 len))
		  #t)
		 (else
		  #f)))))))

	;; Returns a string representation of args.
	(define (str . args)
	  (let ((out (open-output-string)))
	    (let loop ((a args))
	      (if (not (null? a))
		  (begin
		    (fprintf out "~a" (car a))
		    (loop (cdr a)))))
	    (get-output-string out)))

	(provide string-reverse
		 string-find
		 string-rfind
		 string-starts-with?
		 string-ends-with?
		 string-ltrim
		 string-rtrim
		 string-trim
		 string-split
		 string-tokenizer
		 str))