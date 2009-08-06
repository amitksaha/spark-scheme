;; Interface of a reactor event handler.
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

(library reactor-handler

	(export handler handler-on-read handler-on-write 
		 handler-on-error handler-on-timeout)
	
	;; A reactor event handler holds 4 callback procedures.
	;; It also has a member called caller. This is of an
	;; arbitrary type. When the reactor calls a callback in the
	;; handler, the handler in turn will call it's corresponding
	;; callback with the caller object as first argument.
	(define-struct handler-s (on-read
				on-write
				on-error
				on-timeout
				caller))
				
	(define (handler on-read on-write on-exception on-timeout caller)
	  (make-handler-s on-read on-write on-exception on-timeout caller))

	;; This procedure is called by the reactor when a handle is
	;; ready for reading. The handle can be any valid file
	;; descriptor. The handler in turn will call the procedure that
	;; is pointed to by it's on-read member slot.
	(define (handler-on-read self handle)
	  (let ((callback (handler-s-on-read self)))
	    (if (not (eqv? callback null))
		(callback (handler-s-caller self) handle))))

	;; This procedure is called by the reactor when a handle is
	;; ready for writing. The handle can be any valid file
	;; descriptor. The handler in turn will call the procedure that
	;; is pointed to by it's on-write member slot.
	(define (handler-on-write self handle)
	  (let ((callback (handler-s-on-write self)))
	    (if (not (eqv? callback null))
	       (callback (handler-s-caller self) handle))))
		
	;; This procedure is called by the reactor when a handle
	;; generates an error. The handle can be any valid file
	;; descriptor. The handler in turn will call the procedure that
	;; is pointed to by it's on-error member slot.
	(define (handler-on-error self handle)
	  (let ((callback (handler-s-on-error self)))
	    (if (not (eqv? callback null))
		(callback (handler-s-caller self) handle))))

	;; This procedure is called by the reactor when it times-out.
	;; The handler in turn will call the procedure that
	;; is pointed to by it's on-timeout member slot.
	(define (handler-on-timeout self)
	  (let ((callback (handler-s-on-timeout self)))
	    (if (not (null? callback))
		(callback (handler-s-caller self))))))
	    

