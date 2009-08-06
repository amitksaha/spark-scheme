;; Some language extention trials. 
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

;; try-catch-finally

(module spark-lang-ext mzscheme

	(require library-manager)

	(define-syntax try
	  (syntax-rules (catch finally)
	    ((_ try-body ... (catch catch-proc))
	     (with-handlers ((void (lambda (ex) (catch-proc ex))))
			    (begin
			      try-body ...)))
	    ((_ try-body ... (catch catch-proc) (finally fin-proc))
	     (with-handlers ((void (lambda (ex) 
				     (with-handlers ((void (lambda (inner-ex)
							     (fin-proc)
							     (raise inner-ex))))
						    (catch-proc ex) 
						    (fin-proc)))))
			    (begin
			      try-body ...
			      (fin-proc))))))

	;; :~

	;; while loop

	(define-syntax while
	  (syntax-rules ()
	    ((while condition body ...)
	     (let __while_loop__ ()
	       (if (eq? condition #t)
		   (begin
		     body ...
		     (__while_loop__)))))))

	;; for loop on top of while loop

	(define-syntax for
	  (syntax-rules (in times)
	    ((for __v__ in __lst__ body ...)
	     (let* ((__v__ null)
		    (__list__ __lst__)
		    (__cond__ (lambda ()
				(if (eqv? __list__ null)
				    #f
				    (begin
				      (set! __v__ (car __list__))
				      (set! __list__ (cdr __list__))
				      #t)))))
	       (while (__cond__) body ...)))
	    ((for __x__ times body ...)
	     (let ((i 0))
	       (while (< i __x__)
		      body ...
		      (set! i (+ i 1)))))))


	;; A simplified module syntax

	(define-syntax library
	  (syntax-rules ()
	    ((library package-name package-body ...)
	     (module package-name spark
		     package-body ...))))

	(define-syntax export
	  (syntax-rules ()
	    ((export v ...)
	     (provide v ...))))

	
	(define-syntax import
	  (syntax-rules ()
	    ((_ (v1 ...) (v2 ...) ...)
	     (begin
	       (load-library 'v1 ...)
	       (load-library 'v2 ...)
	       ...
	       (require v1 ...)
	       (require v2 ...)
	       ...))))

	(define __sema__ (make-semaphore 1))
	(define __sema_map__ (make-hash-table 'equal))
	(define __sema_map_lock__ (make-semaphore 1))

	(define-syntax atomic
	  (syntax-rules ()
	    ((atomic b ...)
	     (begin
	       (semaphore-wait __sema__)
	       b ...
	       (semaphore-post __sema__)))
	    ((atomic name b ...)
	     (let ((sema (hash-table-get __sema_map__ name null)))
	       (cond
		((null? sema)
		 (set! sema (make-semaphore 1))
		 (semaphore-wait __sema_map_lock__)
		 (hash-table-put! __sema_map__ name sema)
		 (semaphore-post __sema_map_lock__)))
	       (semaphore-wait sema)
	       b ...
	       (semaphore-post sema)))))	

	(provide try
		 while
		 for
		 atomic
		 library
		 export
		 import))

