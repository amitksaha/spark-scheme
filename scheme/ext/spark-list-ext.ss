;; Some list and vector utilities. 
;; Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal

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

(module spark-list-ext mzscheme

	(require (prefix l:: (lib "list.ss")))

	(define list-flatten null)
	(define vector-flatten null)
	
	(define (flatten args)
	  (cond 
	   ((list? args)
	    (list-flatten args))
	   ((vector? args)
	    (vector-flatten args))
	   (else
	    (error "Could not flatten this object."))))

	(define list-remove-if null)
	(define vector-remove-if null)

	(define (remove-if v . predic)
	  (let ((p eq?))
	    (if (not (null? predic))
		(set! p (car predic)))
	    (cond 
	     ((list? v)
	      (list-remove-if v p))
	     ((vector? v)
	      (vector-remove-if v p))
	     (else
	      (error "(remove-if) cannot be applied on this object.")))))

	(define (remove-if-not v predic)
	  (cond 
	   ((list? v)
	    (list-remove-if v predic #t))
	   ((vector? v)
	    (vector-remove-if v predic #t))
	   (else
	    (error "(remove-if-not) cannot be applied on this object."))))

	(define (find s v . args)
	  (cond
	   ((null? s) -1)
	   (else
	    (if (not (list? s))
		(begin
		  (cond 
		   ((vector? s) (set! s (vector->list s)))
		   ((string? s) (set! s (string->list s)))
		   (else (error "Cannot do a find on this object.")))))
	    (let ((p eq?) (b 0) (c null) (i 0)
		  (len (length s)) (ret -1))
	      (if (not (null? args))
		  (begin
		    (set! b (car args))
		    (set! args (cdr args))))
	      (if (not (null? args))
		  (set! p (car args)))
	      (set! i b)
	      (let loop ()
		(if (< i len)	    
		    (begin
		      (set! c (list-ref s i))
		      (if (p v c)
			  (set! ret i)
			  (begin
			    (set! i (add1 i))
			    (loop))))))
	      ret))))

	(define (rfind s v . args)
	  (cond
	   ((null? s) -1)
	   (else
	    (if (not (list? s))
		(begin
		  (cond 
		   ((vector? s) (set! s (vector->list s)))
		   ((string? s) (set! s (string->list s)))
		   (else (error "Cannot do a rfind on this object.")))))
	    (let ((r -1) (p eq?) (b 0))
	      (if (not (null? args))
		  (begin
		    (set! b (car args))
		    (set! args (cdr args))))
	      (if (not (null? args))
		  (set! p (car args)))
	      (set! r (find (reverse s) v b p))
	      (if (not (= r -1))
		  (begin
		    (set! r (add1 r))
		    (set! r (- (length s) r))))
	      r))))

	(define (merge . s)
	  (let ((ret (list))
		(c null))
	    (let loop ()
	      (if (not (null? s))
		  (begin
		    (set! c (car s))
		    (cond
		     ((list?) (set! ret (append ret c)))
		     ((vector?) (set! ret (append ret (vector->list c))))
		     (else (set! ret (append ret (list c)))))
		    (set! s (cdr s))
		    (loop))))
	    ret))

	(define (sort self . args)
	  (let ((lt <) (type 'sort) (f null))
	    (if (not (null? args))
		(begin
		  (set! lt (car args))
		  (set! args (cdr args))))
	    (if (not (null? args))
		(set! type (car args)))
	    (case type
	      ((sort) (set! f l::sort))
	      ((quick) (set! f l::quicksort))
	      ((merge) (set! f l::mergesort))
	      (else (error "Invalid sort type.")))		
	    (cond
	     ((list? self) (f self lt))
	     ((vector? self) (list->vector (f (vector->list self) lt)))
	     (else (error "Cannot sort this type.")))))

	(define (merge-sorted list1 list2 . args)
	  (let ((lt <))
	    (if (not (null? args))
		(set! lt (car args)))
	    (cond
	     ((and (list? list1) (list? list2))
	      (l::merge-sorted-lists list1 list2 lt))
	     ((and (vector? list1) (vector? list2))
	      (list->vector (l::merge-sorted-lists (vector->list list1) (vector->list list2) lt)))
	     (else (error "Cannot merge-sort this type.")))))

	(define (empty? self)
	  (cond
	   ((list? self) (l::empty? self))
	   ((vector? self) (l::empty? (vector->list self)))
	   (else (error "Invalid type."))))

	(define (filter self f)
	  (cond
	   ((list? self) (l::filter f self))
	   ((vector? self) (list->vector (l::filter f (vector->list self))))
	   (else (error "Invalid type."))))

	(define (find-if self f)
	  (cond
	   ((list? self) (l::findf f self))
	   ((vector? self) (list->vector (l::findf f (vector->list self))))
	   (else (error "Invalid type."))))

	(define (unique self . args)
	  (let ((ret (list)) (i null) (cmpr =) (v #f))
	    (if (not (null? args))
		(set! cmpr (car args)))
	    (if (vector? self)
		(begin
		  (set! self (vector->list self))
		  (set! v #t)))
	    (let loop ()
	      (if (not (null? self))
		  (begin
		    (set! i (car self))
		    (if (= (find ret i 0 cmpr) -1)
			(set! ret (append ret (list i))))
		    (set! self (cdr self))
		    (loop))))
	    (if v
		(list->vector ret)
		ret)))	

	(define (unique? self . args)
	  (let ((cmpr =) (len-f length))
	    (if (not (null? args))
		(set! cmpr (car args)))
	    (if (vector? self)
		(set! len-f vector-length))
	    (= (len-f self) (len-f (unique self cmpr)))))

	(define (remove self item . args)
	  (if (null? args)
	      (l::remove item self)
	      (l::remove item self (car args))))

	;; Returns a new copy of lst after droping the first n elements in lst.
	(define (drop n lst)
	  (if (or (<= n 0) (null? lst))
	      lst
	      (drop (- n 1) (cdr lst))))

	;; Returns a new copy of lst that contains only the first n elements.
	(define (take n lst)
	  (if (or (<= n 0) (null? lst))
	      null
	      (cons (car lst) (take (- n 1) (cdr lst)))))

	;; Returns #t if predicate is true for all elements in elems.
	(define (every? predicate? elems)
	  (if (not (list? elems))
	      (cond 
	       ((vector? elems) (set! elems (vector->list elems)))
	       ((string? elems) (set! elems (string->list elems)))
	       (else (error "(every? cannot be applied to this type."))))
	  (let/ec break
		  (let loop ((e elems))
		    (if (not (null? e))
			(begin
			  (if (not (predicate? (car e)))
			      (break #f))
			  (loop (cdr e)))))
		  #t))

	(set! list-flatten 
	      (lambda (args)
		(let ((ret (list))
		      (c null))
		  (let loop ()
		    (if (not (null? args))
			(begin
			  (set! c (car args))
			  (cond 
			   ((list? c)
			    (set! ret (append ret (list-flatten c))))
			   (else
			    (set! ret (append ret (list c)))))
			  (set! args (cdr args))
			  (loop))))
		  ret)))

	(set! vector-flatten
	      (lambda (args)
		(let ((ret (list))
		      (c null)
		      (len (vector-length args))
		      (i 0))
		  (let loop ()
		    (if (< i len)
			(begin
			  (set! c (vector-ref args i))
			  (if (vector? c)
			      (set! ret (append ret (vector->list (vector-flatten c))))
			      (set! ret (append ret (list c))))
			  (set! i (add1 i))
			  (loop))))
		  (list->vector ret))))

	(set! list-remove-if
	      (lambda (v p . rev)
		(let ((ret (list)) (c null))
		  (let loop ()
		    (if (not (null? v))
			(begin
			  (set! c (car v))
			  (if (not (null? rev))
			      (begin
				(if (p c)
				    (set! ret (append ret (list c)))))
			      (begin
				(if (not (p c))
				    (set! ret (append ret (list c))))))
			  (set! v (cdr v))
			  (loop))))
		  ret)))

	(set! vector-remove-if
	      (lambda (v p . rev)
		(if (not (null? rev))
		    (list->vector (list-remove-if (vector->list v) p (car rev)))
		    (list->vector (list-remove-if (vector->list v) p)))))

	(provide flatten find rfind
		 remove-if remove-if-not remove
		 sort merge-sorted empty?
		 find-if filter unique
		 unique? drop take
		 every?))