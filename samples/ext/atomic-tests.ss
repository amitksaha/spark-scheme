(define count 0)

(define (counter)
  (atomic
   (begin
     (set! count (add1 count))
     (printf "~a " count))))

(define (counter-with-named-atomic)
  (atomic "counter"
	  (begin
	    (set! count (add1 count))
	    (printf "~a " count))))

(for i in (range 1000)
     (thread counter))

(while (< count 1000)
       (sleep 1))

(printf "~nFinal value: ~a~n" count)

(set! count 0)

(for i in (range 1000)
     (thread counter-with-named-atomic))

(while (< count 1000)
       (sleep 1))

(printf "~nFinal value with named-atomic: ~a~n" count)