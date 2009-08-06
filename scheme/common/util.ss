;; Some common utilities.
;; Copyright (C) 2008 Vijay Mathew Pandyalakal
 
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

(library util

	 (import ((prefix spark.sysinfo:: #%spark-sysinfo)))

	(export remove-from-list 
		vm-executable
		spark-version
		parse-query-string
		url-decode)

	(define (remove-from-list a n)
	  (if (not (eqv? a null))
	      (begin
		(let* ((tmp a) (tmp-n 0))
		  (let loop ((new-a ()))
		    (set! tmp-n (car tmp))
		    (if (not (eqv? tmp-n n))
			(set! new-a (append new-a (list tmp-n))))
		    (set! tmp (cdr tmp))
		    (if (not (eqv? tmp null))
			(loop new-a)
			new-a))))
	      a))


	(define (vm-executable)
	  (spark.sysinfo::vm-executable))

	(define (spark-version)
	  (spark.sysinfo::spark-version))
	
	(define (parse-query-string query)
	   (let ((tokens (string-split query (list #\&)))
		 (ret (list))
		 (q null))
	     (while (not (null? tokens))
		    (set! q (string-split (car tokens) (list #\=)))
		    (set! ret (append ret 
				      (list (cons (car q) (url-decode (car (cdr q)))))))
		    (set! tokens (cdr tokens)))
	     ret))

	 
	 (define (url-decode s)
	   (let ((s (string->list s)))
	     (list->string
	      (let loop ((s s))
		(if (null? s) '()
		    (let ((a (car s)) (d (cdr s)))
		      (case a
			((#\+) (cons #\space (loop d)))
			((#\%) (cons (hex->char (car d) (cadr d)) (loop (cddr d))))
			(else (cons a (loop d))))))))))
	 
	 (define (hex->char x y)
	   (integer->char
	    (string->number (string x y) 16))))


	


		 
