;; exception handling

(define (check-string s)
  (if (not (string? s))
      (raise "Expects a string argument")
      #t))

(define (check-null n)
  (if (eqv? n null)
      (raise 0)
      #t))

;; this is the exception handler.
;; this can also be defined as a lambda with the
;; catch statement itself.
(define (exception-handler ex)
  (cond
   ((string? ex) 
    (printf "Got a string exception~n"))
   ((number? ex)
    (printf "Got a number exception~n"))
   (else
    (print "Got un unknown object as exception~n")))
  (printf "error: ~a~n" ex))

;; this is for the finally part,
;; which is optional.
(define (after-all)
  (printf "do all cleanup here~n"))

;; this is how we handle exceptions in Spark
(try
 (check-string "hello, world")
 (check-null null) ;; an exception
 (catch exception-handler))

(try
 (check-string "ok") ;; another exception
 (catch exception-handler)
 (finally after-all))