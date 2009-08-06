(import (match))

(define (server pid)
  (printf "server pid: ~a~n" pid) (flush-output)
  (let ((v (receive pid)))
    (while (not (eq? v 'exit))
	   (match v
		  ((from-pid 'add a b) (send from-pid (+ a b)))
		  (_ (error "Invalid message.")))
	   (set! v (receive pid)))))

(define (client pid)
  (printf "client pid: ~a~n" pid) (flush-output)
  (let ((r-pid (spawn server null)))
    (send r-pid (list pid 'add 10 20))
    (printf "~a~n" (receive pid)) (flush-output)
    (send r-pid 'exit)))

(define cpid (spawn client))

(define (watcher pid)
  (printf "watcher pid: ~a~n" pid) (flush-output)
  (watch cpid pid)
  (match (receive pid)
	 (('dead wpid) (if (= wpid cpid) (begin (printf "thread ~a is dead~n" wpid) (flush-output))))
	 (_ (printf "waiting ...~n") (flush-output))))

(spawn watcher)
(spawn watcher)
(spawn watcher)
(read)
