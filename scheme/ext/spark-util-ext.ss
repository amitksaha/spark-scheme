;; Some useful extention procedures.
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

(module spark-util-ext mzscheme

	(require (prefix spark.sysinfo:: #%spark-sysinfo))
	(require spark-lang-ext)
	(require (lib "file.ss"))
	
	(define (prompt msg)
	  (printf "~a" msg)
	  (read))

	(define (argv)
	  (let ((v (spark.sysinfo::argv))
		(ret null) (i 0))
	    (if (not (eqv? v null))
		(begin
		  (set! ret (make-vector (length v)))
		  (let loop ()
		    (if (not (eqv? v null))
			(begin
			  (vector-set! ret i (car v))
			  (set! i (add1 i))
			  (set! v (cdr v))
			  (loop))))
		  ret)
		null)))

	(define (argv-list)
	  (spark.sysinfo::argv))

	;; Tests if a container contains the value v.
	;; The default comparison predicate is eq?.
	;; The optional funcs can contain three procedures
	;; which are the user-defined equivalents for eq?, 
	;; list-ref and length. These can be avoided if
	;; self is a list, a vector or a string.
	(define (contains? self v . funcs)
	  (let* ((p eq?) 
		 (ref null) 
		 (lf null)
		 (len 0) (i 0)
		 (found #f))
	    (cond
	     ((list? self)
	      (set! ref list-ref)
	      (set! lf length))
	     ((vector? self)
	      (set! ref vector-ref)
	      (set! lf vector-length))
	     ((string? self)
	      (set! ref string-ref)
	      (set! lf string-length)))
	    (if (not (null? funcs))
		(begin
		  (set! p (car funcs))
		  (set! funcs (cdr funcs))))
	    (if (not (null? funcs))
		(begin
		  (set! ref (car funcs))
		  (set! funcs (cdr funcs))))
	    (if (not (null? funcs))
		(set! lf (car funcs)))
	    (set! len (lf self))
	    (let loop ()
	      (if (and (< i len)
		       (not found))
		  (begin
		    (set! found (p (ref self i) v))
		    (set! i (add1 i))
		    (loop))))
	    found))

	;; Parses an arguments list into a hashtable of
	;; keyword-arguments. The keywords should be symbols.
	(define (kw/args args . keywords)
	  (if (null? args)
	      null
	      (begin
		(let ((ret (make-hash-table))
		      (kw null))
		  (let loop ()
		    (if (not (null? args))
			(begin
			  (set! kw (car args))
			  (set! args (cdr args))
			  (if (not (null? keywords))
			      (begin
				(if (not (contains? keywords kw))
				    (error "Invalid keyword argument."))))
			  (hash-table-put! ret kw (car args))
			  (set! args (cdr args))
			  (loop))))
		  ret))))

	;; range function similar to the one in Python
	;; Thanks to Michele Simionato 
	;; (http://www.artima.com/weblogs/viewpost.jsp?thread=240781)	
	(define range
	  (case-lambda
	   ((n); one-argument syntax
	    (range 0 n 1))
	   ((n0 n); two-argument syntax
	    (range n0 n 1))
	   ((n0 n s); three-argument syntax
	    ;;(assert (and (for-all number? (list n0 n s)) (not (zero? s))))
	    (let ((cmp (if (positive? s) >= <=)))
	      (let loop ((i n0) (acc '()))
		(if (cmp i n) (reverse acc)
		    (loop (+ i s) (cons i acc))))))))

	(define (generic-for-each f to-list-func . v)
	  (let ((args (list)))
	    (set! v (car v))
	    (set! args (append (list f)))
	    (let loop ()
	      (if (not (null? v))
		  (begin
		    (set! args (append args (list (to-list-func (car v)))))
		    (set! v (cdr v))
		    (loop))))
	    (apply for-each args)))

	(define (vector-for-each f . v)
	  (generic-for-each f vector->list v))

	(define (string-for-each f . v)
	  (generic-for-each f string->list v))

	;; (symbol=? symbol1 symbol2 symbol3 ...) -> Boolean
	(define (symbol=? s1 s2 . args)
	  (let loop ((e (eq? s1 s2))
		     (syms args))
	    (cond
	     ((null? syms) e)
	     ((<= (length syms) 1) (and e (eq? (car syms) (car args))))
	     (else
	      (loop (and e (eq? (car syms) (cadr syms))) (cddr syms))))))

	(provide prompt
		 argv
		 argv-list
		 contains?
		 kw/args
		 range
		 vector-for-each
		 string-for-each
		 ;; from file.ss
		 copy-directory/files
		 delete-directory/files
		 explode-path
		 file-name-from-path
		 filename-extension
		 find-files
		 find-relative-path
		 make-directory*
		 make-temporary-file
		 normalize-path
		 path-only
		 symbol=?))
