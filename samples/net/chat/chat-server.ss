(import (net) (reactor))

(define users (make-hash-table))
(define running #t)

(define (send-message msg from-socket)
  (let ((next-element (hash-table-iterate-first users))
	(sock null))
    (let loop ()
      (if (integer? next-element)
	  (begin
	    (set! sock (hash-table-iterate-key users next-element))
	    (if (not (eqv? sock from-socket))
		(socket-send-line sock msg))
	    (set! next-element (hash-table-iterate-next users next-element))
	    (loop))))))

(define (on-client-connect acceptor client-conn)
  (let ((client-socket (car client-conn))
	(user ""))    
    (acceptor-add-watch acceptor client-socket 'for-read)
    (set! user (socket-recv-line client-socket 10))
    (hash-table-put! users client-socket user)
    (printf "user: ~a~n" user) (flush-output)
    (let ((out (open-output-string)))
      (fprintf out "~a joined the chat" user)
      (send-message (get-output-string out) client-socket))))
    
(define (on-client-read acceptor client-socket)
  (let ((msg "") (user (hash-table-get users client-socket "")))
    (set! msg (socket-recv-line client-socket 10))
    (if (not (null? msg))
	(begin
	  (printf "~a~n" msg) (flush-output)
	  (if (not (eqv? msg null))
	      (begin
		(cond
		 ((string=? msg "QUIT")
		  (set! running #f))
		 ((string=? msg "BYE")
		  (hash-table-remove! users client-socket)
		  (acceptor-remove-watch acceptor client-socket 'for-read)
		  (let ((out (open-output-string)))
		    (fprintf out "~a has left the chat." user)
		    (send-message (get-output-string out) client-socket)))
		 (else
		  (let ((out (open-output-string)))
		    (fprintf out "~a: ~a" user msg)
		    (send-message (get-output-string out) 
				  client-socket))))))))))

(define (on-server-timeout acceptor)
  (printf "Waiting for client, timedout.~n")
  (flush-output))

(define port 7070)
(printf "Chat server listening on port ~a ~n" port)

(define chat-server (socket-acceptor))
(acceptor-port! chat-server 7070)
(acceptor-on-client-connect! chat-server on-client-connect)
(acceptor-on-client-read! chat-server on-client-read)
(acceptor-on-server-timeout! chat-server on-server-timeout)
(acceptor-open chat-server #t (list 10 0))
(let loop ()
  (if running
      (begin
	(acceptor-watch chat-server)
	(loop))))
(acceptor-close chat-server)
