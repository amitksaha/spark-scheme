;; language extention tests

(import ((lib "list.ss")))

(define (ex e)
 (let ((ret 0))
   (cond ((string? e)
          (set! ret 1))
         ((integer? e)
          (set! ret 2))
         (else
          (set! ret -1)))
   ret))

(define (raise-string)
 (raise "error"))

(define (raise-integer)
 (raise 1))

(define (raise-unknown)
 (raise #f))

(define (try-catch-test-1)
 (try
  (raise-string)
  (catch ex)))

(define (try-catch-test-2)
 (try
  (raise-integer)
  (catch ex)))

(define (try-catch-test-3)
 (try
  (raise-unknown)
  (catch ex)))

(define (try-catch-finally-test)
 (try
  (raise-unknown)
  (catch ex)
  (finally (lambda () 4))))

(define (for-test-1)
 (let ((ret (list)))
   (for v in '('a 'e 'i 'o 'u)
        (set! ret (append ret (list v))))
   ret))

(define (for-test-2)
 (let ((ret (list)))
   (for i in (range 6)
        (set! ret (append ret (list i))))
   ret))

(define (for-test-3)
 (let ((ret (list)))
 (for 3 times
      (set! ret (append ret (list 0)))
      (set! ret (append ret (list 1))))
   ret))

(define (run-test s res exp-res)
 (let ((cmp-opr equal?))
   (if (integer? exp-res)
       (set! cmp-opr =))
   (if (not (cmp-opr res exp-res))
       (begin
         (let ((out (open-output-string)))
           (fprintf out "~a failed with result ~a. expected ~a" s res exp-res)
           (raise (get-output-string out))))
       (printf "test ~s OK -> ~a~n" s res))))

(define (run-tests)
 (run-test "(try-catch-test-1)" (try-catch-test-1) 1)
 (run-test "(try-catch-test-2)" (try-catch-test-2) 2)
 (run-test "(try-catch-test-3)" (try-catch-test-3) -1)
 (run-test "(try-catch-finally-test)" (try-catch-finally-test) 4)

 (run-test "(range 10)" (range 10) (list 0 1 2 3 4 5 6 7 8 9))
 (run-test "(range -10)" (range -10) ())
 (run-test "(range 5 12)" (range 5 12) '(5 6 7 8 9 10 11))
 (run-test "(range -5 12)" (range -5 12) '(-5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11))
 (run-test "(range -5 -12)" (range -5 -12) ())
 (run-test "(range 1 10 2)" (range 1 10 2) '(1 3 5 7 9))

 (run-test "(for-test-1)" (for-test-1) '('a 'e 'i 'o 'u))
 (run-test "(for-test-2)" (for-test-2) '(0 1 2 3 4 5))
 (run-test "(for-test-3)" (for-test-3) '(0 1 0 1 0 1))

 "All tests passed.")

(run-tests)
