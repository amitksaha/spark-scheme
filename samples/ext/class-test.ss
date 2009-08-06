(import (class))

(define person
  (class object%
	 (public to-string
		 set-age)
	 
	 (init-field (name ""))
	 (init-field (age 1))
	 
	 (define (to-string)
	   (print name)
	   (newline)
	   (print age)
	   (newline))
	 
	 (define (set-age a)
	   (if (<= a 0)
	       (raise (make-exn:fail "Invalid age." 
				     (current-continuation-marks)))
	       (set! age a)))
	 
	 (super-new)))

(define p (new person (name "Vijay") (age 29)))
(send p to-string)
(try
 (send p set-age 30)
 (catch (lambda (ex) 
	  (send p set-age 10))))
(send p to-string)
