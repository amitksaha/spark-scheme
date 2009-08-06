;; A socket server based on the select-reactor.
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

(library reactor-acceptor
	 
	 (import (net) (select-reactor)
		 (reactor-handler) (exception)
		 (asserts))

	 (export socket-acceptor socket-acceptor?
		 acceptor-port! acceptor-open
		 acceptor-watch acceptor-close
		 acceptor-server-socket acceptor-address!
		 acceptor-backlog! acceptor-callbacks!
		 acceptor-on-client-connect! acceptor-on-client-read!
		 acceptor-on-server-write! acceptor-on-client-write!
		 acceptor-on-server-error! acceptor-on-client-error!
		 acceptor-on-server-timeout! acceptor-add-watch
		 acceptor-remove-watch acceptor-watch-for-error? 
		 acceptor-watch-for-error!)

	 
	 ;; socket-acceptor binds together a select-reactor
	 ;; and a server socket to create a synchronous demultiplexing
	 ;; server. For sample usage, see time-server.ss in the tests directory.
	 (define-struct socket-acceptor-s (server-socket 
					   address 
					   backlog
					   ;; callbacks
					   on-client-connect
					   on-client-read
					   on-server-write
					   on-client-write
					   on-server-error
					   on-client-error
					   on-server-timeout
					   ;; end of callbacks.
					   reactor
					   watch-for-error))					

	 ;; Creates and initializes a new socket-acceptor object.
	 ;; Takes two optional arguments:
	 ;; 1. A custom socket object or null.
	 ;; 2. Address to bind to or null.
	 ;; Returns the new socket-acceptor.
	 (define (socket-acceptor . args)
	   (let ((self (make-socket-acceptor-s (socket) 
					       (address) 
					       10
					       null null null null 
					       null null null null
					       #f)))
	     (if (not (eqv? args null))
		 (begin
		   (let ((len (length args)) (tmp null) (rest ()))
		     (if (> len 2)
			 (raise-exception "socket-acceptor"
					  "Expects only 2 arguments."
					  'contract))
		     (if (>= len 1)
			 (begin
			   (set! tmp (car args))
			   (if (not (eqv? tmp null))
			       (begin
				 (if (not (socket? tmp))
				     (raise-exception "socket-acceptor"
						      "Expected socket object."
						      null))))
			   (set-socket-acceptor-s-server-socket! self tmp)
			   (set! rest (cdr args))))
		     (if (not (eqv? rest null))
			 (begin
			   (set! tmp (car rest))
			   (if (not (eqv? tmp null))
			       (begin
				 (if (not (address? tmp))
				     (raise-exception "socket-acceptor"
						      "Expected address object."
						      null))
				 (set-socket-acceptor-s-address! self tmp)))
			   (set! rest (cdr args)))))))
	     self))
	 
	 ;; A convenience method to set the server port.
	 (define (acceptor-port! self port)
	   (assert-integer port)
	   (address-port! (socket-acceptor-s-address self) port))

	 ;; Opens the acceptor. Opens and binds the server socket.
	 ;; Initializes the reactor. Adds the server socket to the
	 ;; reactors watch-for-read list. If the watch-for-error
	 ;; flag is #t, also adds the socket to the reactors error watch
	 ;; list.
	 (define (acceptor-open self . args)
	   (let ((server-socket (socket-acceptor-s-server-socket self))
		 (r (socket-acceptor-s-reactor self))
		 (h (handler on-read
			     on-write
			     on-error
			     on-timeout
			     self))
		 (reuse-addr #f)
		 (timeout (list 0 0)))
	     (if (eqv? r null)
		 (begin
		   (set! r (reactor))
		   (set-socket-acceptor-s-reactor! self r)))
	     (if (not (eqv? args null))
		 (begin
		   (if (> (length args) 2)
		       (raise-exception "acceptor-open"
					"Expected only 3 arguments."
					null))
		   (set! reuse-addr (car args))
		   (set! timeout (car (cdr args)))
		   (assert-list timeout)))
	     (socket-open server-socket)
	     (socket-bind server-socket 
			  (socket-acceptor-s-address self) 
			  reuse-addr)
	     (socket-listen server-socket (socket-acceptor-s-backlog self))
	     (reactor-add-handler r h)
	     (let ((handle (socket-handle server-socket)))
	       (reactor-add-watch r handle 'for-read)
	       (if (socket-acceptor-s-watch-for-error self)
		   (reactor-add-watch r handle 'for-error)))
	     (reactor-timeout! r timeout)))

	 ;; Runs the reactor.
	 (define (acceptor-watch self)
	   (let ((r (socket-acceptor-s-reactor self)))
	     (if (eqv? r null)
		 (raise-exception "acceptor-watch"
				  "Null reactor."
				  null))
	     (reactor-run r)))

	 ;; Closes the server socket. Releases the reactor.
	 (define (acceptor-close self)
	   (let ((server-socket (socket-acceptor-s-server-socket self)))
	     (if (not (eqv? server-socket null))
		 (begin
		   (socket-close server-socket)
		   (set-socket-acceptor-s-server-socket! self null)))
	     (set-socket-acceptor-s-reactor! self null)))

	 ;; The handler in the reactor will call this method on
	 ;; a read event. The handler's caller is this socket-acceptor
	 ;; object (self). handle will be a socket descriptor.
	 (define (on-read self handle)
	   (let ((server-socket (socket-acceptor-s-server-socket self))
		 (on-client-connect (socket-acceptor-s-on-client-connect self))
		 (on-client-read (socket-acceptor-s-on-client-read self)))
	     ;; if handle is the server socket, we need to accept a client
	     ;; connection and call the on-client-connect callback.
	     (if (eqv? handle (socket-handle server-socket))
		 (begin
		   (let ((client-conn null))
		     (set! client-conn (socket-accept server-socket))
		     (if (not (eqv? on-client-connect null))
			 (on-client-connect self client-conn))))
		 (begin
		   ;; for a client socket, we just call the on-client-read
		   ;; callback.
		   (if (not (eqv? on-client-read null))
		       (begin
			 (let ((client-socket (socket-instance handle)))
			   (on-client-read self client-socket))))))))

	 ;; The handler in the reactor will call this method on
	 ;; a write event. The handler's caller is this socket-acceptor
	 ;; object (self). handle will be a socket descriptor.
	 (define (on-write self handle)
	   (let ((server-socket (socket-acceptor-s-server-socket self))
		 (on-server-write (socket-acceptor-s-on-server-write self))
		 (on-client-write (socket-acceptor-s-on-client-write self)))
	     (if (eqv? handle (socket-handle server-socket))
		 (begin
		   (if (not (eqv? on-server-write null))
		       (on-server-write self server-socket)))
		 (begin
		   (let ((socket-object (socket-instance handle)))
		     (if (not (eqv? on-client-write null))
			 (on-client-write self socket-object)))))))

	 ;; The handler in the reactor will call this method on
	 ;; a error event. The handler's caller is this socket-acceptor
	 ;; object (self). handle will be a socket descriptor.
	 (define (on-error self handle)
	   (let ((server-socket (socket-acceptor-s-server-socket self))
		 (on-server-error (socket-acceptor-s-on-server-error self))
		 (on-client-error (socket-acceptor-s-on-client-error self)))
	     (if (eqv? handle (socket-handle server-socket))
		 (begin
		   (if (not (eqv? on-server-error null))
		       (on-server-error self server-socket)))
		 (begin
		   (let ((socket-object (socket-instance handle)))
		     (if (not (eqv? on-client-error null))
			 (on-client-error self socket-object)))))))

	 ;; The handler in the reactor will call this method on
	 ;; a time-out. The handler's caller is this socket-acceptor
	 ;; object (self).
	 (define (on-timeout self)
	   (let ((on-server-timeout (socket-acceptor-s-on-server-timeout self)))
	     (on-server-timeout self)))

	 ;; A convenience method to set all callbacks on one go.
	 (define (acceptor-callbacks! self
				      on-client-connect
				      on-client-read
				      on-server-write
				      on-client-write
				      on-server-error
				      on-client-error
				      on-server-timeout)
	   (set-socket-acceptor-s-on-client-connect! self on-client-connect)
	   (set-socket-acceptor-s-on-client-read! self on-client-read)
	   (set-socket-acceptor-s-on-server-write! self on-server-write)
	   (set-socket-acceptor-s-on-client-write! self on-client-write)
	   (set-socket-acceptor-s-on-server-error! self on-server-error)
	   (set-socket-acceptor-s-on-client-error! self on-client-error)
	   (set-socket-acceptor-s-on-server-timeout! self on-server-timeout))
	 
	 ;; set the callbacks individually.

	 (define (acceptor-on-client-connect! self callback)
	   (set-socket-acceptor-s-on-client-connect! self callback))

	 (define (acceptor-on-client-read! self callback)
	   (set-socket-acceptor-s-on-client-read! self callback))

	 (define (acceptor-on-server-write! self callback)
	   (set-socket-acceptor-s-on-server-write! self callback))

	 (define (acceptor-on-client-write! self callback)
	   (set-socket-acceptor-s-on-client-write! self callback))

	 (define (acceptor-on-client-error! self callback)
	   (set-socket-acceptor-s-on-client-error! self callback))

	 (define (acceptor-on-server-error! self callback)
	   (set-socket-acceptor-s-on-server-error! self callback))

	 (define (acceptor-on-server-timeout! self callback)
	   (set-socket-acceptor-s-on-server-timeout! self callback))

	 ;; Adds a socket object for watching by the internal reactor. Note that
	 ;; socket is a socket object and not a socket handle.
	 (define (acceptor-add-watch self socket type)
	   (reactor-add-watch (socket-acceptor-s-reactor self)
			      (socket-handle socket)
			      type))

	 ;; Removes a socket object from being watched by the internal reactor.
	 (define (acceptor-remove-watch self socket type)
	   (reactor-remove-watch (socket-acceptor-s-reactor self)
				 (socket-handle socket)
				 type))

	 (define (acceptor-watch-for-error? self)
	   (socket-acceptor-s-watch-for-error self))
	 
	 (define (acceptor-watch-for-error! self flag)
	   (set-socket-acceptor-s-watch-for-error! self flag))

	 (define (socket-acceptor? self)
	   (socket-acceptor-s? self))

	 (define (acceptor-server-socket self)
	   (socket-acceptor-s-server-socket self))

	 (define (acceptor-backlog! self b)
	   (set-socket-acceptor-s-backlog! self b))

	 (define (acceptor-address! self a)
	   (set-socket-acceptor-s-address! self a)))


