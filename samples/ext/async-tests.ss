;; test

(define done 0)

(define (cb a)
  (printf "done with ~a~n" a)
  (flush-output)
  (set! done (add1 done)))

(define (hello)
;  (sleep 1)
  (printf "hello, world~n")
  (flush-output)
  #t)

(async hello () cb)
(async hello () cb)
(async hello () cb)
(async hello () cb)

(let loop ()
  (if (not (= done 3))
      (loop)))

(printf "~a~n" (async-workers))
(flush-output)  

(async-release)

(printf "~a~n" (async-workers))
(flush-output)  
