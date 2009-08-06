;; A socket client based on the select-reactor.
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

(library reactor-connector

	 (import (net) (select-reactor) (reactor-handler)
		 (exception) (asserts))

	 (export socket-connector connector-open
		 connector-watch connector-close
		 connector-add-watch connector-remove-watch
		 connector-callbacks! connector-on-connect!
		 connector-on-read! connector-on-write!
		 connector-on-error! connector-on-timeout!
		 connector-watch-for-error! connector-watch-for-error?)

	 ;; Combines together the select-reactor and socket objects
	 ;; to create a synchronous socket client. For sample usage,
	 ;; see time-client.ss in the tests directory.
	 (define-struct socket-connector-s (client-sockets
					    addresses
					    on-connect
					    on-read
					    on-write
					    on-error
					    on-timeout
					    reactor
					    watch-for-error))

	 ;; Creates and initializes a socket-connector object.
	 ;; Takes an optional argument. This should be a list
	 ;; of addresses for the connector to connect to.
	 (define (socket-connector . args)
	   (let ((self (make-socket-connector-s null () null null
						null null null null
						#f)))
	     (if (not (eqv? args null))
		 (begin
		   (let ((len (length args)) (tmp null) (rest ()))
		     (if (> len 1)
			 (raise-exception "socket-connector"
					  "Expects only 1 argument."
					  'contract))
		     (set! tmp (car args))
		     (if (not (eqv? tmp null))
			 (begin
			   (set-socket-connector-s-addresses! self tmp))))))
	     self))

	 ;; Opens the connector. Initializes the reactor.
	 ;; Creates one socket each for the addresses given in
	 ;; (socket-connector). Adds them to the write watch list
	 ;; of the reactor. 
	 (define (connector-open self timeout)
	   (let ((addresses (socket-connector-s-addresses self)))
	     (if (not (eqv? addresses null))
		 (begin
		   (assert-list timeout)
		   (let ((client-sockets ())
			 (client-socket null)
			 (r null)
			 (h (handler on-read
				     on-write
				     on-error
				     on-timeout
				     self)))
		     (if (eqv? r null)
			 (begin
			   (set! r (reactor))
			   (set-socket-connector-s-reactor! self r)))
		     (reactor-timeout! r timeout)
		     (reactor-add-handler r h)
		     ;; loop through the list of addresses
		     (let loop ((rest addresses))
		       ;; create one socket for each address
		       (set! client-socket (socket))
		       (socket-open client-socket)		      
		       (socket-non-blocking! client-socket #t)
		       ;; append the new socket to the sockets list
		       (set! client-sockets (cons client-socket client-sockets))
		       (socket-connect client-socket (car rest))
		       ;; when the socket connects succefully, 
		       ;; it will be ready for write.
		       (let ((handle (socket-handle client-socket)))
			 (reactor-add-watch r handle 'for-write)
			 (if (socket-connector-s-watch-for-error self)
			     (reactor-add-watch r handle 'for-error)))
		       (set! rest (cdr rest))
		       (if (not (eqv? rest null))
			   (loop rest)))
		     (set-socket-connector-s-client-sockets! self client-sockets))))))

	 ;; Runs the reactor.
	 (define (connector-watch self)
	   (let ((r (socket-connector-s-reactor self)))
	     (if (eqv? r null)
		 (raise-exception "watch"
				  "Null reactor."
				  null))	    
	     (reactor-run r)))
	 
	 ;; Releases the reactor. Closes all socket objects.
	 (define (connector-close self)
	   (let* ((client-sockets (socket-connector-s-client-sockets self)))
	     (if (> (length client-sockets) 0)
		 (begin
		   (let ((sock (car client-sockets))
			 (rest (cdr client-sockets)))
		     (let loop ()
		       (socket-close sock)
		       (if (not (eqv? rest null))
			   (begin
			     (set! sock (car rest))
			     (set! rest (cdr rest))
			     (loop)))))
		   (set-socket-connector-s-client-sockets! self null))))
	   (set-socket-connector-s-reactor! self null))

	 ;; Reactor's handler will call this when a write event occurs.
	 (define (on-write self handle)
	   (let* ((on-sock-connect (socket-connector-s-on-connect self))
		  (on-sock-write (socket-connector-s-on-write self))
		  (sock (get-socket-with-handle self handle))
		  (non-blocking #f))
	     (if (not (eqv? sock null))
		 (begin
		   (set! non-blocking (socket-non-blocking? sock))
		   ;; If the socket is non-blocking, then it is freshly
		   ;; connected.
		   (if non-blocking
		       (begin
			 ;; If the error options is set, then the connect failed.
			 (let ((sockopt (socket-option 
					 sock 
					 'error)))
			   (if (not (eqv? sockopt 0))
			       (raise-exception "socket-connector::on-write"
						"Connection error."
						null)
			       (begin
				 ;; If the connect succeeded, set the socket
				 ;; to blocking and call the on-sock-connect
				 ;; callback.
				 (socket-non-blocking! sock #f)
				 (if (not (eqv? on-sock-connect null))
				     (on-sock-connect self sock))))))
		       ;; If the socket is already blocking, then it was
		       ;; previously connected. So just call on-sock-write
		       ;; callback.
		       (begin
			 (if (not (eqv? on-sock-write null))
			     (on-sock-write self sock))))))))
	 
	 (define (on-read self handle)
	   (let* ((on-sock-read (socket-connector-s-on-read self))
		  (sock (get-socket-with-handle self handle)))
	     (if (and (not (eqv? sock null))
		      (not (eqv? on-sock-read null)))
		 (on-sock-read self sock))))

	 (define (on-error self handle)
	   (let* ((on-sock-error (socket-connector-s-on-error self))
		  (sock (get-socket-with-handle self handle)))
	     (if (not (eqv? sock null))
		 (on-sock-error self sock))))

	 (define (on-timeout self)
	   (let ((on-conn-timeout (socket-connector-s-on-timeout self)))
	     (on-conn-timeout self)))

	 (define (connector-callbacks! self
				       on-connect
				       on-read
				       on-write
				       on-error
				       on-timeout)
	   (set-socket-connector-s-on-connect! self on-connect)
	   (set-socket-connector-s-on-read! self on-read)
	   (set-socket-connector-s-on-error! self on-error)
	   (set-socket-connector-s-on-timeout! self on-timeout))
	 
	 (define (connector-on-connect! self callback)
	   (set-socket-connector-s-on-connect! self callback))

	 (define (connector-on-read! self callback)
	   (set-socket-connector-s-on-read! self callback))

	 (define (connector-on-write! self callback)
	   (set-socket-connector-s-on-write! self callback))

	 (define (connector-on-error! self callback)
	   (set-socket-connector-s-on-error! self callback))

	 (define (connector-on-timeout! self callback)
	   (set-socket-connector-s-on-timeout! self callback))

	 (define (connector-add-watch self socket type)
	   (reactor-add-watch (socket-connector-s-reactor self)
			      (socket-handle socket)
			      type))
	 
	 (define (connector-remove-watch self socket type)
	   (reactor-remove-watch (socket-connector-s-reactor self)
				 (socket-handle socket)
				 type))
	 
	 (define (connector-watch-for-error? self)
	   (socket-connector-s-watch-for-error self))

	 (define (connector-watch-for-error! self flag)
	   (set-socket-connector-s-watch-for-error! self flag))

	 (define (get-socket-with-handle self handle)
	   (let ((client-sockets (socket-connector-s-client-sockets self)))
	     (if (not (eqv? client-sockets null))
		 (begin
		   (let ((sock (car client-sockets))
			 (rest (cdr client-sockets))
			 (found #f))
		     (let loop ()
		       (if (eqv? (socket-handle sock)
				 handle)
			   (set! found #t)
			   (begin
			     (set! sock (car rest))
			     (set! rest (cdr rest))
			     (if (not (eqv? rest null))
				 (loop)))))
		     (if found
			 sock
			 null)))
		 null))))
