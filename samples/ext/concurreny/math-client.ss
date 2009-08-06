;; A client process for the math-server.

(define (math-client pid)
  (let loop ((msg (receive pid)))
    (if (not (eq? msg 'exit))
	(begin
	  (printf "Result: ~a~n" msg)
	  (flush-output)
	  (loop (receive pid))))))

(define pid (spawn math-client))
(register pid "math-client")

(remoting! 1445)
;; Give some time for the remoting server to start.
(sleep 2)

(send "math-server@localhost" (list "math-client@localhost:1445" '+ 2 3))
(send "math-server@localhost" (list "math-client@localhost:1445" '* 2 3))

(read)
