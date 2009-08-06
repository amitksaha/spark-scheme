(import (sunit))

(define verbose #f)

(sunit-tests "(flatten)"
	     (list 
	      (make-test '(flatten (list 1 2 (list 3 4))) (list 1 2 3 4))
	      (make-test '(flatten (list 1 2 (list 3 4 (list "sunny") (list 'a 'b 'c)))) 
			 (list 1 2 3 4 "sunny" 'a 'b 'c))
	      (make-test '(flatten (list 1 2 3 4)) (list 1 2 3 4))
	      (make-test '(flatten (list)) (list))
	      (make-test '(flatten (vector 1 2 (vector 3 4))) (vector 1 2 3 4))
	      (make-test '(flatten (vector 1 2 (vector 3 4 (vector "sunny")) (vector 'a 'b 'c))) 
			 (vector 1 2 3 4 "sunny" 'a 'b 'c))
	      (make-test '(flatten (vector 1 2 3 4)) (vector 1 2 3 4))
	      (make-test '(flatten (vector)) (vector)))
	     verbose)

(define (cmp a)
  (= a 20))

(sunit-tests "(remove-if)"
	     (list
	      (make-test '(remove-if (list 10 20 30 40 50) cmp) (list 10 30 40 50))
	      (make-test '(remove-if (list 10 20 30 20 40 50) cmp) (list 10 30 40 50))
	      (make-test '(remove-if (list 10 30 40 50) cmp) (list 10 30 40 50))
	      (make-test '(remove-if (list) cmp) (list))
	      (make-test '(remove-if (vector 10 20 30 40 50) cmp) (vector 10 30 40 50))
	      (make-test '(remove-if (vector 10 20 30 20 40 50) cmp) (vector 10 30 40 50))
	      (make-test '(remove-if (vector 10 30 40 50) cmp) (vector 10 30 40 50))
	      (make-test '(remove-if (vector) cmp) (vector)))
	     verbose)

(sunit-tests "(remove-if-not)"
	     (list
	      (make-test '(remove-if-not (list 10 20 30 40 50) cmp) (list 20))
	      (make-test '(remove-if-not (list 10 20 30 20 40 50) cmp) (list 20 20))
	      (make-test '(remove-if-not (list 10 30 40 50) cmp) (list))
	      (make-test '(remove-if-not (list) cmp) (list))
	      (make-test '(remove-if-not (vector 10 20 30 40 50) cmp) (vector 20))
	      (make-test '(remove-if-not (vector 10 20 30 20 40 50) cmp) (vector 20 20))
	      (make-test '(remove-if-not (vector 10 30 40 50) cmp) (vector))
	      (make-test '(remove-if-not (vector) cmp) (vector)))
	     verbose)
	     

(sunit-tests "(find)"
	     (list
	      (make-test '(find (list 10 20 30 100 50) 30) 2)
 	      (make-test '(find (list 10 20 30 100 50) 30 2) 2)
 	      (make-test '(find (list 10 20 30 100 50) 30 5) -1)
 	      (make-test '(find (list 10 20 30 100 50) 100) 3)
 	      (make-test '(find (list 10 20 30 100 50) 10) 0)
 	      (make-test '(find (list 10 20 30 100 50) 50) 4)
 	      (make-test '(find (list 10 20 30 100 50) 300) -1)
 	      (make-test '(find (list) 30) -1)
 	      (make-test '(find (list "sunny" "jenny" "crystel") "jenny" 0 equal?) 1)
 	      (make-test '(find (list "sunny" "jenny" "crystel" "kenny") "crystel" 1 equal?) 2)
 	      (make-test '(find (list "sunny" "jenny" "crystel" "kenny") "sam" 1 equal?) -1)
 	      (make-test '(find (list) "sam") -1)
	      (make-test '(find (list "sam") "sam" 0 equal?) 0)
	      (make-test '(find (list "sam") "sam" 1 equal?) -1)
	      (make-test '(find (vector 10 20 30 100 50) 30) 2)
 	      (make-test '(find (vector 10 20 30 100 50) 30 2) 2)
 	      (make-test '(find (vector 10 20 30 100 50) 30 5) -1)
 	      (make-test '(find (vector 10 20 30 100 50) 100) 3)
 	      (make-test '(find (vector 10 20 30 100 50) 10) 0)
 	      (make-test '(find (vector 10 20 30 100 50) 50) 4)
 	      (make-test '(find (vector 10 20 30 100 50) 300) -1)
 	      (make-test '(find (vector) 30) -1)
 	      (make-test '(find (vector "sunny" "jenny" "crystel") "jenny" 0 equal?) 1)
 	      (make-test '(find (vector "sunny" "jenny" "crystel" "kenny") "crystel" 1 equal?) 2)
 	      (make-test '(find (vector "sunny" "jenny" "crystel" "kenny") "sam" 1 equal?) -1)
 	      (make-test '(find (vector) "sam" 1 equal?) -1)
 	      (make-test '(find (vector "sam") "sam" 0 equal?) 0)
	      (make-test '(find "hello" #\e) 1)
	      (make-test '(find 'something 'a) 'error))
	     verbose)

(sunit-tests "(rfind)"
	     (list
	      (make-test '(rfind (list 10 20 30 100 50) 30) 2)
 	      (make-test '(rfind (list 10 20 30 100 50) 30 2) 2)
 	      (make-test '(rfind (list 10 20 30 100 50) 30 5) -1)
 	      (make-test '(rfind (list 10 20 30 100 50) 100) 3)
 	      (make-test '(rfind (list 10 20 30 100 50) 10) 0)
 	      (make-test '(rfind (list 10 20 30 100 50) 50) 4)
 	      (make-test '(rfind (list 10 20 30 100 50) 300) -1)
 	      (make-test '(rfind (list) 30) -1)
 	      (make-test '(rfind (list "sunny" "jenny" "crystel") "jenny" 0 equal?) 1)
 	      (make-test '(rfind (list "sunny" "jenny" "crystel" "kenny") "crystel" 1 equal?) 2)
 	      (make-test '(rfind (list "sunny" "jenny" "crystel" "kenny") "sam" 1 equal?) -1)
 	      (make-test '(rfind (list) "sam") -1)
	      (make-test '(rfind (list "sam") "sam" 0 equal?) 0)
	      (make-test '(rfind (list "sam") "sam" 1 equal?) -1)
	      (make-test '(rfind (vector 10 20 30 100 50) 30) 2)
 	      (make-test '(rfind (vector 10 20 30 100 50) 30 2) 2)
 	      (make-test '(rfind (vector 10 20 30 100 50) 30 5) -1)
 	      (make-test '(rfind (vector 10 20 30 100 50) 100) 3)
 	      (make-test '(rfind (vector 10 20 30 100 50) 10) 0)
 	      (make-test '(rfind (vector 10 20 30 100 50) 50) 4)
 	      (make-test '(rfind (vector 10 20 30 100 50) 300) -1)
 	      (make-test '(rfind (vector) 30) -1)
 	      (make-test '(rfind (vector "sunny" "jenny" "crystel") "jenny" 0 equal?) 1)
 	      (make-test '(rfind (vector "sunny" "jenny" "crystel" "kenny") "crystel" 1 equal?) 2)
 	      (make-test '(rfind (vector "sunny" "jenny" "crystel" "kenny") "sam" 1 equal?) -1)
 	      (make-test '(rfind (vector) "sam" 1 equal?) -1)
 	      (make-test '(rfind (vector "sam") "sam" 0 equal?) 0)
	      (make-test '(rfind "hello" #\e) 1)
	      (make-test '(rfind 'something 'a) 'error))
	     verbose)

(sunit-tests "(empty?)"
	     (list
	      (make-test '(empty? (list 10 20 30 100 50)) #f)
	      (make-test '(empty? (list)) #t)
	      (make-test '(empty? (vector 10 20 30 100 50)) #f)
	      (make-test '(empty? (vector)) #t))
	     verbose)

(sunit-tests "(sort)"
	     (list 
	      (make-test '(sort (list 20 30 10 100 50)) (list 10 20 30 50 100))
	      (make-test '(sort (list "abc" "zzse" "aaa" "qas") string<?) (list "aaa" "abc" "qas" "zzse"))
	      (make-test '(sort (vector 20 30 10 100 50)) (vector 10 20 30 50 100))
	      (make-test '(sort (vector "abc" "zzse" "aaa" "qas") string<?) (vector "aaa" "abc" "qas" "zzse")))
	     verbose)

(define (f x)
  (if (= x 100)
      #f
      #t))

(sunit-tests "(filter)"
	     (list 
	      (make-test '(filter '(20 30 40 100 10 50 100) f) '(20 30 40 10 50))
	      (make-test '(filter (vector 20 30 40 100 10 50 100) f) (vector 20 30 40 10 50)))
	     verbose)

(sunit-tests "(unique)"
	     (list
	      (make-test '(unique '(10 20 30 20 100 4 5 20 5)) '(10 20 30 100 4 5))
	      (make-test '(unique '("hello" "bye" "is" "bye" "true") string=?) '("hello" "bye" "is" "true"))
	      (make-test '(unique '()) '())
	      (make-test '(unique (vector 10 20 30 20 100 4 5 20 5)) (vector 10 20 30 100 4 5))
	      (make-test '(unique (vector "hello" "bye" "is" "bye" "true") string=?) 
			 (vector "hello" "bye" "is" "true")))
	     #f)

(sunit-tests "(unique?)"
	     (list
	      (make-test '(unique? '(10 20 30 20 100 4 5 20 5)) #f)
	      (make-test '(unique? '(10 20 30 100 4 5)) #t)
	      (make-test '(unique? ()) #t)
	      (make-test '(unique? '("hello" "bye" "is" "bye" "true") string=?) #f)
	      (make-test '(unique? '("hello" "bye" "is" "true") string=?) #t)
	      (make-test '(unique? (vector 10 20 30 20 100 4 5 20 5)) #f)
	      (make-test '(unique? (vector 10 20 30 100 4 5)) #t)
	      (make-test '(unique? (vector "hello" "bye" "is" "bye" "true") string=?) #f)
	      (make-test '(unique? (vector "hello" "bye" "is" "true") string=?) #t))
	     #f)

(sunit-tests "(drop)"
	     (list
	      (make-test '(drop 3 '(1 2 3 4 5 6)) '(4 5 6))
	      (make-test '(drop 1 '(1 2 3 4 5 6)) '(2 3 4 5 6))
	      (make-test '(drop 0 '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
	      (make-test '(drop -2 '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
	      (make-test '(drop 3 null) null)
	      (make-test '(drop -3 null) null)
	      (make-test '(drop 3 '(1)) null)
	      (make-test '(drop 3 '(1 2 3)) null)
	      (make-test '(drop 1 '(1)) null))
	     #f)

(sunit-tests "(take)"
	     (list
	      (make-test '(take 3 '(1 2 3 4 5 6)) '(1 2 3))
	      (make-test '(take 1 '(1 2 3 4 5 6)) '(1))
	      (make-test '(take 0 '(1 2 3 4 5 6)) null)
	      (make-test '(take -2 '(1 2 3 4 5 6)) null)
	      (make-test '(take 3 null) null)
	      (make-test '(take -3 null) null)
	      (make-test '(take 3 '(1)) '(1))
	      (make-test '(take 3 '(1 2 3)) '(1 2 3))
	      (make-test '(take 1 '(1)) '(1)))
	     #f)
