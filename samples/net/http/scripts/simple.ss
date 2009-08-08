(define (hello new-url state)
    (let ((html (sgml `(html
		      (body
		       "hello, world")))))
      ((html 'text))))

(list hello)