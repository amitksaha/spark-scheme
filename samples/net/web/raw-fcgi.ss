;; FastCGI using low-level C++ calls.

(import (net) (http-util) ((prefix spark.fcgi:: #%spark-fcgi)))

(define server-socket (socket))
(define addr (address))
(address-port! addr 8888)
(socket-open server-socket)
(socket-bind server-socket addr #t)
(socket-listen server-socket)
(define s (socket-accept server-socket))
(define client-socket (car s))

(define req (spark.fcgi::recv (socket-handle client-socket)))
(printf "request-id: ~a~n" (spark.fcgi::fcgi-request-id req))
(printf "request-role: ~a~n" (spark.fcgi::fcgi-request-role req))
(printf "request-keep-connection: ~a~n" (spark.fcgi::fcgi-request-keep-connection req))
(printf "request-aborted: ~a~n" (spark.fcgi::fcgi-request-aborted req))
(define keys (spark.fcgi::fcgi-request-param-keys req))
(printf "request-keys: ~a~n" keys)
(while (not (null? keys))
       (printf "~a: ~a~n" (car keys) (spark.fcgi::fcgi-request-param-value req (car keys)))
       (if (string=? (car keys) "QUERY_STRING")
	   (printf "~a~n" (parse-query-string (spark.fcgi::fcgi-request-param-value req (car keys)))))
       (set! keys (cdr keys)))
(flush-output)

(printf "sending response: ~a~n" (spark.fcgi::send req "<html><head><title>Hello</title></head><body>hello, fcgi world</body></html>"))
(printf "deleting request: ~a~n" (spark.fcgi::delete-fcgi-request req))

(socket-close client-socket)
(socket-close server-socket)
