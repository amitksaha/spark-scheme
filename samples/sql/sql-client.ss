(import (sql-sqlite))
;;(import (sql-odbc))

(define mydb (db))
(db-open mydb "mydb")
;;(db-open mydb "dsn=MySQL;user=vijay;password=pswd")

(display "Welcome to the interactive SQL console!")
(newline)
(display "Enter a . to quit: ")
(newline)

(define sql null)
(define col-count 0)
(define i 0)

(let repl ()
  (display "> ")
  (set! sql (read-line (current-input-port) 'any))
  (cond
   ((string=? sql ".")
    (db-close mydb)
    (exit))
   ((char=? (char-downcase (string-ref sql 0)) #\s)
    (let ((stmt (db-create-statement mydb sql)))
      (if (not (eqv? stmt null))
	  (begin
	    (statement-execute stmt)
	    (set! col-count (result-column-count stmt))
	    (for i in (range col-count)
		 (printf "~a," (result-column-name stmt i)))		 
	    (newline)
	    ;(printf "~a~n" (result-columns->list stmt))))))
	    (while (result-next stmt)
		   (for i in (range col-count)
			(printf "~a," (result-string stmt i)))
		   (newline))))))	   
   (else 
    (db-execute mydb sql)))
  (repl))
  