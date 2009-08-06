(import (net))

(define client-socket (socket))
(define host-address (address "127.0.0.1" 8090))

(socket-open client-socket)
(socket-connect client-socket host-address)
(socket-send client-socket #"GET / HTTP/1.0")
(socket-send client-socket #"\r\n\r\n") ;; send \r\n as bytes
(print (socket-recv client-socket 1024)) (newline)
(socket-close client-socket)