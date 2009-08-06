;; Mzscheme interface to system threads.
;; Copyright (C) 2008  Vijay Mathew Pandyalakal
 
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

(module sys-thread mzscheme

	(require exception)
	(require utils)
	(require (prefix spark.pthread:: #%spark-pthread))

	(define-struct thread-s (handle
				 callback
				 callback-arg))

	(define (sys-thread . args)
	  (let ((self (make-thread-s null null null)))
	    (if (not (eqv? args null))
		(begin
		  (set-thread-s-callback! self (car args))
		  (set! args (cdr args))))
	    (if (not (eqv? args null))
		(set-thread-s-callback-arg! self (car args)))
	    (if (not (eqv? (thread-s-callback self) null))
		(begin
		  (assert-callback self)
		  (if (sys-thread-start self)
		      self
		      null))
		self)))	

	(define (sys-thread-start self)
	  (let ((thread-handle (spark.pthread::pthread-create 
				null
				(thread-s-callback self)
				(thread-s-callback-arg self))))
	    (if (eqv? thread-handle null)
		#f
		(begin
		  (set-thread-s-handle! self thread-handle)
		  #t))))

	(define (sys-thread-sleep secs)
	  (spark.pthread::pthread-sleep secs))

	(define (sys-thread-join self)
	  (let ((thread-handle (thread-s-handle self)))
	    (if (not (eqv? thread-handle null))
		(spark.pthread::pthread-join thread-handle)
		#f)))

	(define (assert-callback self)
	  (let ((callback (thread-s-callback self)))
	    (if (not (eqv? callback null))
		(begin
		  (if (not (procedure? callback))
		      (raise-exception "assert-callback"
				       "Not a procedure"
				       null))
		  (if (not (= (procedure-arity callback) 1))
		      (raise-exception "assert-callback"
				       "Callback should take exactly one argument."
				       null)))
		(raise-exception "assert-callback"
				 "Callback cannot be null."
				 null))))

	(define (sys-thread-timer millisecs callback)
	  (spark.pthread::pthread-timer millisecs callback))

	(provide sys-thread
		 sys-thread-start
		 sys-thread-join
		 sys-thread-sleep
		 sys-thread-timer))
		 
