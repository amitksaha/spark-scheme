;; A reactor implemented using the select() API.
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

(library select-reactor
	 
	 (import (util) (exception) (reactor-handler)
		 (net) ((prefix spark.select:: #%spark-select)))

	 (export reactor reactor-run reactor-timeout!
		  reactor-add-handler reactor-suspend-handler 
		  reactor-remove-handler reactor-add-watch
		  reactor-remove-watch)

	 ;; A select-reactor object waits for multiple file
	 ;; descriptors using the select() system call.
	 ;; On the event of a read, write, exception or time-out, it will
	 ;; invoke the corresponding callback of it's handler objects
	 ;; with the notified file descriptor (handle) as argument.
	 (define-struct select-reactor-s (event-handlers
					  suspended-event-handlers
					  read-handles
					  write-handles
					  error-handles
					  timeout))

	 ;; Creates and returns a new select-reactor.
	 (define (reactor)
	   (let ((self null))
	     (set! self (make-select-reactor-s null null 
					       null null 
					       null null))
	     self))

	 ;; Runs select on the given handles and invokes the appropriate
	 ;; handler callbacks.
	 (define (reactor-run self)
	   (let ((ret 0))
	     (set! ret (spark.select::select (select-reactor-s-read-handles self)
					     (select-reactor-s-write-handles self)
					     (select-reactor-s-error-handles self)
					     (select-reactor-s-timeout self)))
	     (if (eqv? ret null)
		 (raise-sys-exception "select-reactor::run"))
	     (if (eqv? ret #t)
		 (call-timeouts self)
		 (begin
		   (if (> (length ret) 0)
		       (begin
			 (let ((read-ready ()) 
			       (write-ready ()) 
			       (error-ready ()))
			   (set! read-ready (car ret))
			   (set! write-ready (car (cdr ret)))
			   (set! error-ready (car (cdr (cdr ret))))
			   (call-reads self read-ready)
			   (call-writes self write-ready)
			   (call-errors self error-ready))))))))
	 
	 ;; Sets the time-out value. The argument should be
	 ;; a list of second and microsecond values that represent
	 ;; the time-out delay.
	 (define (reactor-timeout! self timeout)
	   (set-select-reactor-s-timeout! self timeout))

	 ;; Adds a handler. The design of the reactor permits
	 ;; more than one handler, but in practice only one is enough.
	 (define (reactor-add-handler self handler)
	   (let ((handlers (select-reactor-s-event-handlers self)))
	     (set! handlers (cons handler handlers))
	     (set-select-reactor-s-event-handlers! self handlers)))
	 
	 ;; Removes a handler.
	 (define (reactor-remove-handler self handler)
	   (set-select-reactor-s-event-handlers! self 
						 (remove-from-list 
						  (select-reactor-s-event-handlers self)
						  handler)))
	 
	 ;; Puts the handler in a suspended state. No callbacks will
	 ;; be invoked on a handler as long as it is in the suspended state.
	 (define (reactor-suspend-handler self handler flag)
	   (if flag
	       (begin
		 (reactor-remove-handler self handler)
		 (let ((handlers (select-reactor-s-suspended-event-handlers self)))
		   (set! handlers (cons handler handlers))
		   (set-select-reactor-s-suspended-event-handlers! self handlers)))
	       (begin
		 (set-select-reactor-s-suspended-event-handlers! self
								 (remove-from-list (select-reactor-s-suspended-event-handlers)
										   handler))
		 (reactor-add-handler self handler))))				  
	 
	 ;; Adds a file descriptor for event watching.
	 ;; type can be:
	 ;; for-read, for-write or for-error.
	 (define (reactor-add-watch self handle type)
	   (case type
	     ((for-read) (watch-for-read self handle))
	     ((for-write) (watch-for-write self handle))
	     ((for-error) (watch-for-error self handle))
	     (else (raise-exception "select-reactor::watch"
				    "Type should be 'on-read, 'on-write or 'on-error."
				    'contract))))

	 (define (watch-for-read self handle)
	   (let ((read-handles (select-reactor-s-read-handles self)))
	     (set! read-handles (cons handle read-handles))
	     (set-select-reactor-s-read-handles! self read-handles)))

	 (define (watch-for-write self handle)
	   (let ((write-handles (select-reactor-s-write-handles self)))
	     (set! write-handles (cons handle write-handles))
	     (set-select-reactor-s-write-handles! self write-handles)))
	 
	 (define (watch-for-error self handle)
	   (let ((error-handles (select-reactor-s-error-handles self)))
	     (set! error-handles (cons handle error-handles))
	     (set-select-reactor-s-error-handles! self error-handles)))

	 ;; Removes a handle from being watched.
	 (define (reactor-remove-watch self handle type)
	   (let ((handles null))
	     (case type
	       ((for-read) 
		(begin
		  (set! handles (remove-from-list (select-reactor-s-read-handles self)
						  handle))
		  (set-select-reactor-s-read-handles! self handles)))
	       ((for-write)
		(begin
		  (set! handles (remove-from-list (select-reactor-s-write-handles self)
						  handle))
		  (set-select-reactor-s-write-handles! self handles)))
	       ((for-error)
		(begin
		  (set! handles (remove-from-list (select-reactor-s-error-handles self)
						  handle))
		  (set-select-reactor-s-error-handles! self handles)))
	       ((all)
		(begin
		  (reactor-remove-watch self handle 'for-read)
		  (reactor-remove-watch self handle 'for-write)
		  (reactor-remove-watch self handle 'for-error)))
	       (else (raise-exception "remove-from-watch" 
				      "Invalid type.('on-read, 'on-write or 'on-error)"
				      'contract)))))
	 
	 (define (call-timeouts self)
	   (call-callback self null 'on-timeout))
	 
	 (define (call-reads self handles)
	   (call-callback self handles 'on-read))

	 (define (call-writes self handles)
	   (call-callback self handles 'on-write))

	 (define (call-errors self handles)
	   (call-callback self handles 'on-error))

	 (define (call-callback self handles type)
	   (let ((handlers (select-reactor-s-event-handlers self)) 
		 (handler null))
	     (let loop ()
	       (set! handler (car handlers))
	       (apply-callback handler handles type)
	       (set! handlers (cdr handlers))
	       (if (not (eqv? handlers null))
		   (loop)))))
	 
	 (define (apply-callback handler handles type)
	   (if (eq? type 'on-timeout)
	       (handler-on-timeout handler)
	       (begin
		 (let ((handle null))	     
		   (let loop ()	       
		     (if (not (eqv? handles null))
			 (begin
			   (set! handle (car handles))
			   (case type		
			     ((on-read) 
			      (handler-on-read handler handle))
			     ((on-write) 
			      (handler-on-write handler handle))
			     ((on-error) 
			      (handler-on-error handler handle))
			     (else (raise-exception "select-reactor::apply-callback"
						    "Not a valid callback type."
						    'contract)))
			   (set! handles (cdr handles))
			   (loop)))))))))
	 

