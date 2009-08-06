(import ((prefix odbc:: #%spark-odbc)))

(define h (odbc::sql-alloc-handle odbc::SQL-HANDLE-ENV null))
(printf "Env: ~a~n" h)
(if (symbol? h)
    (exit))
(odbc::sql-set-env-attr h odbc::SQL-ATTR-ODBC-VERSION odbc::SQL-OV-ODBC3)
(define d (odbc::sql-alloc-handle odbc::SQL-HANDLE-DBC h))
(printf "Db: ~a~n" d)
(if (symbol? d)
    (begin
      (printf "Error in DB: ~a~n" (odbc::sql-get-diag-rec odbc::SQL-HANDLE-ENV h))
      (odbc::sql-free-handle odbc::SQL-HANDLE-ENV h) 
      (exit)))
(odbc::sql-set-connect-attr d odbc::SQL-ATTR-LOGIN-TIMEOUT 5)
(if (symbol? (odbc::sql-connect d "MySQL" "vijay" "password"))
    (begin
      (printf "Error while connecting: ~a~n" 
	      (odbc::sql-get-diag-rec odbc::SQL-HANDLE-DBC d))
      (odbc::sql-free-handle odbc::SQL-HANDLE-DBC d)
      (odbc::sql-free-handle odbc::SQL-HANDLE-ENV h)
      (exit)))
(define s (odbc::sql-alloc-handle odbc::SQL-HANDLE-STMT d))
(printf "Stmt: ~a~n" s)
(if (symbol? s)
    (begin
      (printf "Error in stmt: ~a~n" (odbc::sql-get-diag-rec odbc::SQL-HANDLE-DBC d))
      (odbc::sql-free-handle odbc::SQL-HANDLE-DBC d)
      (odbc::sql-free-handle odbc::SQL-HANDLE-ENV h)
      (exit)))

;; Test a DDL statement
(define res (odbc::sql-exec-direct s "create table t(id integer)"))
(if (symbol? res)
    (begin
      (printf "Error in stmt: ~a~n" (odbc::sql-get-diag-rec odbc::SQL-HANDLE-STMT s))
      (odbc::sql-free-handle odbc::SQL-HANDLE-DBC d)
      (odbc::sql-free-handle odbc::SQL-HANDLE-ENV h)
      (exit)))
(printf "Exec: ~a~n" (odbc::sql-exec-direct s "drop table t"))

;; Test a SELECT query.
(printf "Exec: ~a~n" (odbc::sql-exec-direct s "select * from animal"))
(define cols (odbc::sql-num-result-cols s))
(define rows (odbc::sql-row-count s))
(printf "Num cols: ~a~n" cols)
(printf "Num rows: ~a~n" rows)
(for i in (range 1 (+ cols 1))
     (printf "Label: ~a, Type: ~a, Length: ~a ~n" 
	     (odbc::sql-col-attribute s i odbc::SQL-DESC-NAME)
	     (odbc::sql-col-attribute s i odbc::SQL-DESC-TYPE)
	     (odbc::sql-col-attribute s i odbc::SQL-DESC-LENGTH)))

(while (odbc::sql-fetch s)
       (printf "~a ~a ~a~n" 
	       (odbc::sql-get-data s 1 odbc::SQL-C-CHAR 4)
	       (odbc::sql-get-data s 2 odbc::SQL-C-CHAR 80)
	       (odbc::sql-get-data s 3 odbc::SQL-C-CHAR 4)))

(printf "Free stmt: ~a~n" (odbc::sql-free-handle odbc::SQL-HANDLE-STMT s))
(printf "Disconnect: ~a~n" (odbc::sql-disconnect d))
(printf "Free db: ~a~n" (odbc::sql-free-handle odbc::SQL-HANDLE-DBC d))
(printf "Free env: ~a~n" (odbc::sql-free-handle odbc::SQL-HANDLE-ENV h))
(flush-output)