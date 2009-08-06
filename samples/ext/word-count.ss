;; Reads a text file, counts each word and returns the word-count
;; map in descending order of the count.

(define (read-file file-name)
  (call-with-input-file file-name
    (lambda (file)
      (let ((s (file-size file-name)))
	(read-string s file)))))

(define (print-usage)
  (printf "Usage: spark wc.ss file-name~n")
  (exit))

(if (null? (argv))
    (print-usage))

(define text (read-file (vector-ref (argv) 0)))
(define start (current-milliseconds))
(define tokens (make-hash-table 'equal))
(define tokenizer (string-tokenizer text
				    (lambda (s)
				      (if (not (equal? s ""))
					  (let ((count (hash-table-get tokens s 0)))
					    (hash-table-put! tokens s (add1 count)))))))

(while (tokenizer))
(define res '())
(hash-table-for-each tokens 
		     (lambda (k v) (set! res (cons (cons k v) res))))
(sort res (lambda (a b) (> (cdr a) (cdr b))))
(define end (current-milliseconds))
(printf "~a milliseconds.~n" (- end start))
res       
	
	
