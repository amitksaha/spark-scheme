;; A remote process for doing arithmetic.

(define (math-server pid)
  (let loop ((msg (receive pid))) 
    (if (not (eq? msg 'exit))
	(let ((client-pid (list-ref msg 0))
	      (opr-sym (list-ref msg 1))
	      (opr null)
	      (a (list-ref msg 2))
	      (b (list-ref msg 3)))
	  (case opr-sym
	    ((+) (set! opr +))
	    ((-) (set! opr -))
	    ((*) (set! opr *))
	    ((/) (set! opr /)))
	  (send client-pid (list opr-sym a b '= (opr a b)))
	  (loop (receive pid))))))

(define pid (spawn math-server))
(register pid "math-server")

(remoting!)

(read)