(import (sql-sqlite))

(define mydb (db))
(db-open mydb "test.db")
(db-execute mydb "create table test(id, name)")
(db-execute mydb "insert into test values(1, 'aaaa')")
(db-execute mydb "insert into test values(2, 'bbbb')")
(define stmt (db-create-statement mydb "select * from test where id=?"))
(statement-bind stmt 0 1)
(statement-execute stmt)
(define col-count (result-column-count stmt))
(while (result-next stmt)
       (for i in (range col-count)
	    (printf "~a," (result-string stmt i))))
(newline)
(statement-dispose stmt)

(set! stmt (db-create-statement mydb "select * from test where id=? and name=?"))
(statement-bind stmt 0 2)
(statement-bind stmt 1 "bbbb")
(statement-execute stmt)
(define col-count (result-column-count stmt))
(while (result-next stmt)
       (for i in (range col-count)
	    (printf "~a," (result-string stmt i))))
(newline)
(statement-dispose stmt)

(db-execute mydb "drop table test")
(db-close mydb)