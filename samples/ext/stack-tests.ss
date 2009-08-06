(import (sunit))

(define s (stack))

(sunit-suite "stack-tests"
	     (list
	      "(stack-empty?)"
	      (list 
	       (make-test '(stack-empty? s) #t))
	      "(stack-push!)"
	      (list 
	       (make-test '(stack-push! s 10) 10)
	       (make-test '(stack-push! s 20) 20)
	       (make-test '(stack-push! s 30) 30)
	       (make-test '(stack-push! s 10) 10)
	       (make-test '(stack-push! s 50) 50))
	      "(stack-empty?)"
	      (list 
	       (make-test '(stack-empty? s) #f))
	      "(stack-pop!)"
	      (list 
	       (make-test '(stack-pop! s) 50)
	       (make-test '(stack-pop! s) 10)
	       (make-test '(stack-pop! s) 30)
	       (make-test '(stack-pop! s) 20)
	       (make-test '(stack-pop! s) 10))
	      "(stack-empty?)"
	      (list 
	       (make-test '(stack-empty? s) #t))
	      "(stack-pop!)"
	      (list 
	       (make-test '(stack-pop! s) 'error))
	      "(stack-top)"
	      (list 
	       (make-test '(stack-top s) 'error))
	      "(stack-push!)"
	      (list 
	       (make-test '(stack-push! s 10) 10)
	       (make-test '(stack-push! s 20) 20)
	       (make-test '(stack-push! s 30) 30)
	       (make-test '(stack-push! s 10) 10)
	       (make-test '(stack-push! s 50) 50))
	      "(stack-empty?)"
	      (list 
	       (make-test '(stack-empty? s) #f))
	      "(stack-pop!) and (stack-top)"
	      (list 
	       (make-test '(stack-pop! s) 50)
	       (make-test '(stack-top s) 10)
	       (make-test '(stack-pop! s) 10)
	       (make-test '(stack-top s) 30)
	       (make-test '(stack-pop! s) 30))
	      "(stack-empty?)"
	      (list 
	       (make-test '(stack-empty? s) #f)))
	     #f)