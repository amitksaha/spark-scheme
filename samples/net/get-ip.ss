(import (net))

(define ip-addr (ip-address (car (argv-list))))
(printf "Name: ~a~n" (ip-address-name ip-addr))
(printf "Aliases: ~a~n" (ip-address-aliases ip-addr))
(printf "Type: ~a~n" (ip-address-type ip-addr))
(printf "Length: ~a~n" (ip-address-length ip-addr))
(printf "Addresses: ~a~n" (ip-address-list ip-addr))
