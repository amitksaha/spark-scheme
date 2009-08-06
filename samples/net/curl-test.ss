(import (util) (net-curl))

(define args (argv-list))

(if (not (= (length args) 2))
    (begin
      (display "Usage: spark curl-test.ss url out-file")
      (newline)
      (exit)))

(define url (car args))
(define file (car (cdr args)))

(printf "Downloading ~a to ~a ... ~n" url file)

(curl-env)

(define out (open-output-file file))

(define (write-cb s d)
  (fprintf out "~a" s))


(define c (curl url write-cb))
(printf "curl-perform: ~a~n" (curl-perform c))
(close-output-port out)
(printf "curl-dispose: ~a~n" (curl-dispose c))

(curl-env-dispose)

