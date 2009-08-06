;; An HTTP server.
;; Copyright (C) 2007, 2008, 2009 Vijay Mathew Pandyalakal

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

;; TODO: 
;;  * Implement actual request handling.
;;  * Fix file loading, search paths, etc
;;  * Test and fix session management, its GC etc.
;;  * Implement logging. No logging if 'log-port is null.

(library web-server

	 (import (net) (reactor)
		 ((prefix parser:: http-request-parser))
		 ((prefix loader:: http-resource-loader))
		 ((prefix response:: http-response))
		 ((prefix session:: http-session)))

	 (export web-server
		 web-server-start
		 web-server-stop
		 web-server-socket
		 web-server-configuration
		 web-server-configuration!)

	 (define-struct web-server-s (configuration
				      sessions
				      sessions-gc-thread
				      server-socket))
	 

	 ;; (list) -> web-server
	 ;; Creates a new web-server object. The list argument
	 ;; contains key-values that make up the web-server configuration.
	 ;; Valid key-values are
	 ;; 'port integer - The port to listen on. Defaults to 80.
	 ;; 'script-exts string - Script file extention. Defaults to "ss".
	 ;; 'session-timeout integer - Session timeout in seconds. 
         ;;                            Defaults to 5 seconds.
	 ;; 'log-port - Output port where logging will happen.
	 ;;             Defaults to standard output port.
	 ;; 'max-header-length - Maximum number of bytes that the request
	 ;;                      header can contain. Defaults to 512 Kb
	 ;; 'max-body-length - Maximum number of bytes the body can contain.
	 ;;                    Defaults to 5Mb.
	 ;; E.g.: (web-server (list 'port 8080 'session-timeout 10))
	 (define (web-server conf)
	   (let ((self (make-web-server-s (make-default-conf)
					  (make-hash-table 'equal)
					  null null)))
	     (while (not (null? conf))
		    (web-server-configuration! self
					       (car conf)
					       (cadr conf))
		    (set! conf (cddr conf)))
	     (let ((server-socket (socket))
		   (addr (address)))
	       (address-port! addr (web-server-configuration self 'port))
	       (socket-open server-socket)
	       (socket-bind server-socket addr #t)
	       (set-web-server-s-server-socket! self server-socket))
	     self))

	 ;; (web-server procedure) -> bool
	 ;; Starts the web-server and enters a listen loop.
	 ;; The loop will continue to execute as long as
	 ;; condition-check-proc returns #t.
	 (define web-server-start
	   (case-lambda
	    ((self)
	     (web-server-start self (lambda () #t)))
	    ((self condition-check-proc)
	     (let ((server-socket (web-server-s-server-socket self))
		   (timeout-secs (web-server-configuration self 'timeout))
		   (session-gc-thread 
		    (thread (lambda () 
			      (let loop ()
				(sessions-gc-proc self)
				(loop))))))
	       (set-web-server-s-sessions-gc-thread! self session-gc-thread)
	       (while (condition-check-proc)
		      (socket-listen server-socket)
		      (let ((conn (socket-accept server-socket)))
			(async on-client-connect (list self conn))
			;; Just to context switch.
			(sleep 0)))))))
					


	 (define (web-server-stop self)
	   (socket-close (web-server-s-server-socket self))
	   (if (not (null? (web-server-s-sessions-gc-thread self)))
	       (kill-thread (web-server-s-sessions-gc-thread self))))

	 (define (web-server-socket self)
	   (web-server-s-server-socket self))

	 (define (web-server-configuration self conf-key)
	   (hash-table-get (web-server-s-configuration self) 
			   conf-key null))

	 (define (web-server-configuration! self 
					    conf-key
					    conf-value)
	   (let ((conf (web-server-s-configuration self)))
	     (hash-table-put! conf conf-key conf-value)))

	 
	 (define (make-default-conf)
	   (let ((conf (make-hash-table)))
	     (hash-table-put! conf 'port 80)
	     (hash-table-put! conf 'script-ext #"ss")
	     (hash-table-put! conf 'session-timeout 5) ;; 5 seconds
	     (hash-table-put! conf 'log-port (current-output-port))
	     (hash-table-put! conf 'max-header-length (* 1024 512)) ;; 512Kb
	     (hash-table-put! conf 'max-body-length (* 1024 5120)) ;; 5Mb
	     conf))

	 ;; Called when a new client connection is established.
	 (define (on-client-connect self client-conn)
	   (printf "client connected...~n") (flush-output)
	   (let ((client-socket (car client-conn))
		 (conf (web-server-s-configuration self)))
	     (try
	      (let* ((http-request (read-header conf client-socket))
		     (body-str (read-body conf client-socket http-request)))
		(parser::http-request-data! http-request body-str)
		(handle-request self 
				http-request))
	      (catch (lambda (error)
		       (cond
			((parser::http-parser-error? error)
			 (return-error client-socket 
				       (parser::http-parser-error-message error)
				       conf))
			(else
			 (fprintf (hash-table-get conf 'log-port)
				  "Error: ~a in connection ~a."
				  error
				  (address->string (car (cdr client-conn)))))))))
	     (try
	      (socket-close client-socket)
	      (catch (lambda (error)
		       (fprintf (hash-table-get conf 'log-port)
				"Error: (socket-close): ~a~n"
				error))))))

	 (define (read-header conf client-socket)
	   (let ((max-header-length (hash-table-get conf 'max-header-length))
		 (http-request (parser::http-request))
		 (request-parsed #f))
	     (let loop ((line (socket-recv-line client-socket max-header-length))
			(running-header-length 0))
	       (cond 
		((not (null? line))
		 (cond 
		  ((> running-header-length max-header-length)
		   (raise "Header content is too long."))
		  (else
		   (let ((line-length (string-length line)))
		     (cond 
		      ((> line-length 0)
		       (if (not request-parsed)
			   (begin
			     (parser::http-request-request! http-request line)
			     (set! request-parsed #t))
			   (parser::http-request-header! http-request line))
		       (loop (socket-recv-line client-socket max-header-length)
			     (+ running-header-length line-length))))))))))
	     http-request))

	 (define (read-body conf client-socket http-request)
	   (let ((max-body-length (hash-table-get conf 'max-body-length))
		 (content-length (parser::http-request-header 
				  http-request "content-length" 0)))
	     (if (> content-length 0)
		 (cond
		  ((> content-length max-body-length)
		   (raise "Content is too long."))
		  (else
		   (socket-recv client-socket content-length)))
		 "")))

	 (define (handle-request self http-request)
	   (printf "~a~n" (parser::http-request->string http-request))
	   (flush-output))

	 (define (return-error client-socket error-message
			       conf)
	   (try
	    (socket-send client-socket 
			 (response::make-error-response
			  error-message
			  500 
			  "HTTP/1.0"))
	    (catch (lambda (error)
		     (fprintf (hash-table-get conf 'log-port)
			      "(return-error): ~a~n" error)))))

	 ;; Makes timedout sessions available for garbage collection.
	 (define (sessions-gc-proc self)
	   (let ((session-timeout-secs
		  (web-server-configuration self 'session-timeout)))
	     (if (null? session-timeout-secs)
		 (set! session-timeout-secs (* 5 60)))
	     (sleep session-timeout-secs)
	     (let ((sessions (web-server-s-sessions self))
		   (gc-session-ids (list)))
	       (hash-table-for-each 
		sessions
		(lambda (id session)
		  (let ((diff (- (current-seconds)
				 (session::session-last-access session))))
		    (if (>= diff session-timeout-secs)
			(set! gc-session-ids (cons id gc-session-ids))))))
	       (for id in gc-session-ids
		    (session::session-destroy id sessions))))))
	       