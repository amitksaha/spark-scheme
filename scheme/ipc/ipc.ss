;; Simple mechanism to run a script using a separate instance of
;; spark and communicate with it.
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

(module ipc mzscheme
	
	(require (lib "process.ss"))
	(require utils)

	(define-struct ipc-s (name
			      process-id
			      input-port
			      output-port
			      error-port
			      control-procedure))

	(define (ipc script . args)
	  (let ((exe (vm-executable))
		(cmd (open-output-string))
		(p null)
		(self null))
	    (fprintf cmd "~a ~a " exe script)
	    (let loop ()
	      (if (not (eqv? args null))
		  (begin
		    (fprintf cmd "~a " (car args))
		    (set! args (cdr args))
		    (loop))))
	    (set! p (process (get-output-string cmd)))
	    (if (not (eqv? p null))
		(begin
		  (set! self (make-ipc-s script 0
					  null null 
					  null null))
		  (let ((i 0)
			(v null))
		    (let loop ()
		      (if (not (eqv? p null))
			  (begin
			    (set! v (car p))
			    (set! p (cdr p))
			    (cond
			     ((= i 0)
			      (set-ipc-s-input-port! self v))
			     ((= i 1)
			      (set-ipc-s-output-port! self v))
			     ((= i 2)
			      (set-ipc-s-process-id! self v))
			     ((= i 3)
			      (set-ipc-s-error-port! self v))
			     ((= i 4)
			      (set-ipc-s-control-procedure! self v)))
			    (set! i (add1 i))
			    (loop)))))))
	    self))

	(define (ipc-wait script . args)
	  (let ((self (ipc script args))
		(f null))
	    (if (not (eqv? self null))
		(begin
		  (set! f (ipc-s-control-procedure self))
		  (f 'wait)
		  self))))
	
	(define (ipc-close self)
	  (let ((f (ipc-s-control-procedure self)))
	    (close-output-port (ipc-s-output-port self))
	    (close-input-port (ipc-s-input-port self))
	    (close-input-port (ipc-s-error-port self))
	    (f 'kill)))

	(define (ipc-status self)
	  (let ((f (ipc-s-control-procedure self)))
	    (f 'status)))

	(define (ipc-exit-code self)
	  (let ((f (ipc-s-control-procedure self)))
	    (f 'exit-code)))

	(define (ipc-interrupt self)
	  (let ((f (ipc-s-control-procedure self)))
	    (f 'interrupt)))

	(define (ipc-send self command)
	  (let ((out (ipc-s-output-port self)))
	    (fprintf out "~a~n" command)
	    (flush-output out))
	  #t)

	(define (ipc-recv self)
	  (let ((in (ipc-s-input-port self)))
	    (read-line in)))

	(define (ipc-client-send command)
	  (let ((p (current-output-port)))
	    (fprintf p "~a~n" command)
	    (flush-output p)
	    #t))

	(define (ipc-client-recv)
	  (let ((p (current-input-port)))
	    (read-line p)))
	  
	(define (ipc-name self)
	  (ipc-s-name self))

	(define (ipc-process-id self)
	  (ipc-s-process-id self))

	(provide ipc
		 ipc-wait
		 ipc-close
		 ipc-exit-code
		 ipc-status
		 ipc-interrupt
		 ipc-send
		 ipc-recv
		 ipc-name
		 ipc-process-id
		 ipc-client-send
		 ipc-client-recv))
