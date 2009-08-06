(import (aura))

(define (get-num1 new-uri state)
  (let ((html (sgml `(html
		      (body
		       (form ((action ,new-uri))
			     "Enter number 1: "
			     (input ((type "text")
				     (name "num1")))))))))
    ((html 'text))))

(define (get-num2 new-uri state)
  (let ((html (sgml `(html
		      (body
		       (form ((action ,new-uri))
			     "Enter number 2: "
			     (input ((type "text")
				     (name "num2")))))))))
    ((html 'text))))

(define (add new-uri state)
  (let* ((s1 (hash-table-get state "num1" null))
	 (s2 (hash-table-get state "num2" null))
	 (res (number->string (+ (string->number s1)
				 (string->number s2))))
	 (html (sgml `(html
		       (body
			(b ,res))))))
    ((html 'text))))

(list get-num1 get-num2 add)