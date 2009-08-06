(import (sunit))

(define verbose #t)

(sunit-suite "string-tests"
	     (list
	      "(string-find)"
	      (list 
	       (make-test '(string-find "" "") 0)
	       (make-test '(string-find "hello, world" "o") 4)
	       (make-test '(string-find "hello, world" 5 "o") 8)
	       (make-test '(string-find "hello, world" 5 11 "o" 0) 8)
	       (make-test '(string-find "hello, world" 9 "o") -1)
	       (make-test '(string-find "hello, world" "world") 7)
	       (make-test '(string-find "hello, world" "o, wo") 4)
	       (make-test '(string-find "hello, world" "hello") 0)
	       (make-test '(string-find "hello, world" "") -1)
	       (make-test '(string-find "hello, world" "e") 1)
	       (make-test '(string-find "hello, world" "hell") 0)
	       (make-test '(string-find "hello, world" "ell") 1)
	       (make-test '(string-find "hello, world" "ll") 2)
	       (make-test '(string-find "HERE IS A SIMPLE EXAMPLE" "EXAMPLE") 17)
	       (make-test '(string-find "THIS EXAMPLE IS SIMPLE" "EXAMPLE") 5)
	       (make-test '(string-find 
			    "This is a test of the string search algorithm." 
			    20 
			    "algorithm") 
			  36)
	       (make-test '(string-find 
			    "This is a test of the string search algorithm." 
			    30 
			    "algorithm") 
			  36)
	       (make-test '(string-find 
			    "This is a test of the string search algorithm." 
			    100 
			    "algorithm") 
			  -1)
	       (make-test '(string-find 
			    "This is a test of the string search algorithm." 
			    37
			    "algorithm") 
			  -1)
	       (make-test '(string-find "ANPANPANANPANMANANPANMAN" "ANPANMAN") 8))
	      "(string-rfind)"
	      (list
	       (make-test '(string-rfind "hello" "lo") 3)
	       (make-test '(string-rfind "hello, world" "world") 7)
	       (make-test '(string-rfind "hello, world world " "world") 13)
	       (make-test '(string-rfind "hello, world" "") -1)
	       (make-test '(string-rfind "hello, world" "bye") -1)
	       (make-test '(string-rfind "hello, world" "ell") 1)
	       (make-test '(string-rfind "hello, world" "h") 0)
	       (make-test '(string-rfind "hello, world" "d") 11)
	       (make-test '(string-rfind "hello, world" 1 "worl") 7)
	       	       (make-test '(string-rfind 
			    "This is a test of the string search algorithm." 
			    "algorithm") 
			  36)
	       (make-test '(string-rfind 
			    "This is a test of the string search algorithm." 
			    30 
			    "algorithm") 
			  -1)
	       (make-test '(string-rfind 
			    "This is a test of the string search algorithm." 
			    100 
			    "algorithm") 
			  -1)
	       (make-test '(string-rfind 
			    "This is a test of the string search algorithm." 
			    37
			    "algorithm") 
			  -1))
	      "(contains)"
	      (list
	       (make-test '(contains? (list 100 200 300 10) 200) #t)
	       (make-test '(contains? (list 100 200 300 10) 20) #f)
	       (make-test '(contains? "hello, world" #\w) #t)
	       (make-test '(contains? #("sunny" "ellie" "johny") "ellie") #f)
	       (make-test '(contains? #("sunny" "ellie" "johny") "ellie" equal?) #t))
	      "(string-ends-with?)"
	      (list
	       (make-test '(string-ends-with? "hello, world" "world") #t)
	       (make-test '(string-ends-with? "hello, world" "hello") #f)
	       (make-test '(string-ends-with? "hello, world" "") #f)
	       (make-test '(string-ends-with? "hello, world" "rld") #t)
	       (make-test '(string-ends-with? "hello, world" "ld") #t)
	       (make-test '(string-ends-with? "hello, world" "d") #t)
	       (make-test '(string-ends-with? "hello, world" "w") #f)
	       (make-test '(string-ends-with? "hello, world" "worl") #f)
	       (make-test '(string-ends-with? "hello, world" "bye") #f))
	      "(string-starts-with?)"
	      (list
	       (make-test '(string-starts-with? "hello, world" "hello") #t)
	       (make-test '(string-starts-with? "hello, world" "world") #f)
	       (make-test '(string-starts-with? "hello, world" "") #f)
	       (make-test '(string-starts-with? "hello, world" "hell") #t)
	       (make-test '(string-starts-with? "hello, world" "he") #t)
	       (make-test '(string-starts-with? "hello, world" "h") #t)
	       (make-test '(string-starts-with? "hello, world" "o") #f)
	       (make-test '(string-starts-with? "hello, world" "ello") #f)
	       (make-test '(string-starts-with? "hello, world" "bye") #f))
	      "(string-ltrim)"
	      (list
	       (make-test '(string-ltrim " hello") "hello")
	       (make-test '(string-ltrim "hello") "hello")
	       (make-test '(string-ltrim "    hello  ") "hello  "))
	      "(string-rtrim)"
	      (list
	       (make-test '(string-rtrim "hello ") "hello")
	       (make-test '(string-rtrim "hello") "hello")
	       (make-test '(string-rtrim "    hello  ") "    hello"))
	      "(string-trim)"
	      (list
	       (make-test '(string-trim " hello") "hello")
	       (make-test '(string-trim "hello") "hello")
	       (make-test '(string-trim "hello    world   ") "hello    world")
	       (make-test '(string-trim "    hello  ") "hello"))
	      "(str)"
	      (list
	       (make-test '(str "I am " 3 " years old.") "I am 3 years old."))
	      "(string-split)"
	      (list
	       (make-test '(string-split "hello, world") '("hello," "world"))
	       (make-test '(string-split "hello, world" (list #\space #\,)) '("hello" "" "world"))
	       (make-test '(string-split "This is a#long sentence." (list #\space #\# #\.)) 
			  '("This" "is" "a" "long" "sentence"))))
	     verbose)