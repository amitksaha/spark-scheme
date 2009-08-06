(import (sql-odbc))

(define mydb (db))
(db-open mydb "dsn=MySQL;user=vijay;password=pswd")
(define sql "select * from animal")
(define stmt (db-create-statement mydb sql))
(statement-execute stmt)
(define cols (result-column-count stmt))
(printf "Cols: ~a~n" cols)
(for i in (range cols)
     (printf "~a        " (result-column-name stmt i)))
(newline)
(while (result-next stmt)
       (printf "~a      " 
	       (result-integer stmt 0))
       (printf "~a      " 
	       (result-value stmt 1))
       (printf "~a      " 
	       (result-integer stmt 2))
       (newline))
(db-close mydb)