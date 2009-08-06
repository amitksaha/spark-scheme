;; exception handling

(define (a val)
  (if (eqv? val null)
      (raise "null")
      (printf "~a~n" val)))

(define (b val)
  (if (eqv? val null)
      (raise 'null)
      (printf "~a~n" val)))

(define (handle-exception x)
  (cond
   ((string? x) (printf "caught string ~a~n" x))
   ((number? x) (printf "caught number ~a~n" x))
   ((symbol? x) (printf "caught symbol ~a~n" x))
   (else (printf "caught unknown ~a~n" x))))

(define (do-this-always)
  (display "Finally...")
  (newline)
  (try
   (raise "hello")
   (catch handle-exception)))

(try
 (a null)
 (b null)
 (catch handle-exception)
 (finally do-this-always))

(try
 (a null)
 (catch (lambda (x) (raise "catch")))
 (finally (lambda () (display "any way ...!") (newline))))
	

(print "OK") (newline)
