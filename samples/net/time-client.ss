(import (net) (reactor))

(define finished #f)
(define cs null)

(define (on-connect connector client-socket)
  (socket-send client-socket "GET TIME"))
;  (print (socket-recv client-socket 10)) (newline)
 ; (set! cs client-socket))

(define (on-write connector client-socket)
  (print (socket-recv client-socket 10)) (newline)
  (connector-remove-watch connector client-socket 'for-write)
  (set! finished #t))

(define (on-timeout connector)
  (print "timeout.")
  (newline)
  (set! finished #t))

(define time-client (socket-connector (list 
				       (address "127.0.0.1" 7070))))
(connector-on-connect! time-client on-connect)
(connector-on-write! time-client on-write)
;(connector-on-read! time-client on-read)
(connector-on-timeout! time-client on-timeout)
(connector-open time-client (list 10 0))
(let loop ()
  (connector-watch time-client)
  (if (not finished)
      (loop)))
(connector-close time-client)

