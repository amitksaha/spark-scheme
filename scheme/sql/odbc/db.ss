;; Scheme interface to ODBC.
;; Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.

;; Please contact Vijay Mathew Pandyalakal if you need additional
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

(library sql-odbc

	 (import (exception) (asserts) (util)
		 ((prefix odbc:: #%spark-odbc)))

	 (export 
	  ;; db interface
	  db db-open db-close db-name!
	  db-user-name! db-password! db-authentication!
	  db-option! db-autocommit! db-autocommit?
	  db-commit db-rollback db-begin-transaction 
	  db-end-transaction db-native-sql db-execute
	  db-create-statement db-save-point! 
	  db-release-save-point
	  ;; statement interface
	  statement-bind statement-execute
	  result-with-columns->list
	  result->list
	  result-next result-value
	  result-blob result-integer
	  result-real result-string
	  result-symbol
	  result-column-count result-column-name
	  result-column-database-name result-column-length 
	  result-column-type statement-reset 
	  statement-dispose)

	 ;; Represents a connection to an sqlite database and it's
	 ;; related configurations.
	 (define-struct db-s (env-handle
			      dbc-handle
			      stmt-handle
			      server-name
			      user-name
			      password
			      options
			      statements))
	 
	 ;; Creates and initializes an odbc connection object.
	 ;; Returns the new db object on success.
	 (define db
	   (case-lambda
	    (()
	     (db null null null null))
	    ((server-name user-name password)
	     (db server-name user-name password null))
	    ((server-name user-name password options)
	     (make-db-s null null null
			server-name user-name
			password options
			null))))
	 
	 (define (db-name! self db-name)
	   (assert-string db-name)
	   (set-db-s-server-name! self db-name))

	 (define (db-user-name! self user-name)
	   (assert-string user-name)
	   (set-db-s-user-name! self user-name))

	 (define (db-password! self pswd)
	   (assert-string pswd)
	   (set-db-s-password! self pswd))

	 (define (db-authentication! self user-name pswd)
	   (db-user-name! self user-name)
	   (db-password! self pswd))

	 (define (db-option! self option-name option-value)
	   (let ((options (db-s-options self)))
	     (if (eqv? options null)
		 (set! options (make-hash-table)))
	     (hash-table-put! options option-name option-value)
	     (set-db-s-options! self options)))

	 (define (handle-error ret type handle)
	   (if (symbol? ret)
	       (let ((out (open-output-string)))
		 (fprintf out "~a" (odbc::sql-get-diag-rec type handle))
		 (raise-exception "handle-error"
				  (get-output-string out)
				  null))
	       #t))

	 (define (symbol->odbc-version s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((odbc3) odbc::SQL-OV-ODBC3)
		   (else odbc::SQL-OV-ODBC2)))))

	 (define (symbol->cp s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((off) odbc::SQL-CP-OFF)
		   ((one-per-driver) odbc::SQL-CP-ONE-PER-DRIVER)
		   ((one-per-henv) odbc::SQL-CP-ONE-PER-HENV)
		   ((one-strict-match) odbc::SQL-CP-ONE-STRICT-MATCH)
		   ((one-relaxed-match) odbc::SQL-CP-ONE-RELAXED-MATCH)))))

	 (define (set-env-options! self)
	   (let ((env-handle (db-s-env-handle self))
		 (options (db-s-options self))
		 (val null))
	     (if (and (not (null? env-handle)) 
		      (not (null? options)))
		 (begin
		   (set! val (hash-table-get options 'connection-pooling null))
		   (if (not (null? val))
		       (odbc::sql-set-env-attr 
			env-handle 
			odbc::SQL-ATTR-CONNECTION-POOLING 
			val))
		   (set! val (hash-table-get options 'output-nts null))
		   (if (not (null? val))
		       (odbc::sql-set-env-attr 
			env-handle 
			odbc::SQL-ATTR-OUTPUT-NTS
			val))
		   (set! val (hash-table-get options 'odbc-version null))
		   (if (not (null? val))
		       (odbc::sql-set-env-attr 
			env-handle 
			odbc::SQL-ATTR-ODBC-VERSION
			(symbol->odbc-version val)))
		   (set! val (hash-table-get options 'cp-match null))
		   (if (not (null? val))
		       (odbc::sql-set-env-attr 
			env-handle 
			odbc::SQL-ATTR-ODBC-VERSION
			(symbol->cp val)))))))

	 (define (symbol->access-mode s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((read-only) odbc::SQL-MODE-READ-ONLY)
		   (else odbc::SQL-MODE-READ-WRITE)))))

	 (define (symbol->async-enable s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((off) odbc::SQL-ASYNC-ENABLE-OFF)
		   (else odbc::SQL-ASYNC-ENABLE-ON)))))

	 (define (symbol->auto-commit s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((off) odbc::SQL-AUTOCOMMIT-OFF)
		   (else odbc::SQL-AUTOCOMMIT-ON)))))

	 (define (symbol->trace s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((off) odbc::SQL-OPT-TRACE-OFF)
		   (else odbc::SQL-OPT-TRACE-ON)))))

	 (define (symbol->cd s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((true) odbc::SQL-CD-TRUE)
		   (else odbc::SQL-CD-FALSE)))))

	 (define (symbol->bool s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((true) odbc::SQL-TRUE)
		   (else odbc::SQL-FALSE)))))

	 (define (symbol->odbc-cursors s)
	   (if (not (symbol? s))
	       s
	       (begin
		 (case s
		   ((use-if-needed) odbc::SQL-CUR-USE-IF-NEEDED)
		   ((use-odbc) odbc::SQL-CUR-USE-ODBC)
		   (else odbc::SQL-CUR-USE-DRIVER)))))

	 (define (set-dbc-options! self)
	   (let ((dbc-handle (db-s-dbc-handle self))
		 (options (db-s-options self))
		 (val null))
	     (if (and (not (null? dbc-handle))
		      (not (null? options)))
		 (begin
		   (set! val (hash-table-get options 'access-mode null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-ACCESS-MODE 
			(symbol->access-mode val)))
		   (set! val (hash-table-get options 'async-enable null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-ASYNC-ENABLE 
			(symbol->async-enable val)))
		   (set! val (hash-table-get options 'auto-ipd null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-AUTO-IPD 
			(symbol->bool val)))
		   (set! val (hash-table-get options 'auto-commit null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-AUTOCOMMIT 
			(symbol->auto-commit val)))
		   (set! val (hash-table-get options 'connection-dead null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-CONNECTION-DEAD 
			(symbol->cd val)))
		   (set! val (hash-table-get options 'connection-timeout null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-CONNECTION-TIMEOUT 
			val))
		   (set! val (hash-table-get options 'current-catalog null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-CURRENT-CATALOG 
			val))
		   (set! val (hash-table-get options 'login-timeout null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-LOGIN-TIMEOUT 
			val))
		   (set! val (hash-table-get options 'metadata-id null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-METADATA-ID 
			val))
		   (set! val (hash-table-get options 'odbc-cursors null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-ODBC-CURSORS 
			(symbol->odbc-cursors val)))
		   (set! val (hash-table-get options 'packet-size null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-PACKET-SIZE 
			val))
		   (set! val (hash-table-get options 'quiet-mode null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-QUIET-MODE 
			(symbol->bool val)))
		   (set! val (hash-table-get options 'trace null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-TRACE 
			(symbol->trace val)))
		   (set! val (hash-table-get options 'tracefile null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-TRACEFILE 
			val))
		   (set! val (hash-table-get options 'translate-lib null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-TRANSLATE-LIB
			val))
		   (set! val (hash-table-get options 'translate-option null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-TRANSLATE-OPTION
			val))
		   (set! val (hash-table-get options 'txn-isolation null))
		   (if (not (null? val))
		       (odbc::sql-set-connect-attr 
			dbc-handle 
			odbc::SQL-ATTR-TXN-ISOLATION
			val))))))

	 ;; Opens the database and sets the low-level db-handle.
	 ;; Accept 3 optional arguments:
	 ;; 1. Either DSN or a string in the format: dsn=dsn;user=username;password=pswd
	 ;; 2. User name
	 ;; 3. Password
	 ;; Returns #t on success, raises exception otherwise.
	 (define db-open
	   (case-lambda
	    ((self)
	     (db-open self null))
	    ((self s)
	     (let ((conn-options (parse-connection-string s)))
	       (if (null? conn-options)
		   (db-open self s null null)
		   (db-open self 
			    (hash-table-get conn-options "dsn")
			    (hash-table-get conn-options "user")
			    (hash-table-get conn-options "password")))))
	    ((self server-name user-name password)
	     (db-open self server-name user-name password null))
	    ((self server-name user-name password options)
	     (if (not (null? server-name))
		 (set-db-s-server-name! self server-name))
	     (if (not (null? user-name))
		 (set-db-s-user-name! self user-name))
	     (if (not (null? password))
		 (set-db-s-password! self password))
	     (if (not (null? options))
		 (set-db-s-options! self options))
	     (let ((h (odbc::sql-alloc-handle odbc::SQL-HANDLE-ENV null))
		   (d null) (c null))
	       (if (or (null? h) (symbol? h))
		   (raise-exception "db-open" h null))
	       (set-db-s-env-handle! self h)
	       ;; One option must be set on the env handle.
	       (odbc::sql-set-env-attr h 
				       odbc::SQL-ATTR-ODBC-VERSION 
				       odbc::SQL-OV-ODBC3)
	       (set-env-options! self)
	       (set! d (odbc::sql-alloc-handle odbc::SQL-HANDLE-DBC h))
	       (cond 
		((symbol? d)
		 (handle-error d odbc::SQL-HANDLE-ENV h))
		((null? d)
		 (db-close self)
		 (raise-exception "db-open" "Null DBC handle." null)))
	       (set-db-s-dbc-handle! self d)
	       (set-dbc-options! self)
	       (set! c (odbc::sql-connect d 
					  (db-s-server-name self)
					  (db-s-user-name self)
					  (db-s-password self)))
	       (cond
		((symbol? c)
		 (handle-error c odbc::SQL-HANDLE-DBC d))
		((null? c)
		 (db-close self)
		 (raise-exception "db-open" "Null connect handle." null)))
	       #t))))
	 
	 ;; Closes the low-level database and releases all
	 ;; associated resources.
	 ;; Returns true on success.
	 (define (db-close self)
	   (let ((env-handle (db-s-env-handle self))
		 (dbc-handle (db-s-dbc-handle self))
		 (stmt-handle (db-s-stmt-handle self))
		 (stmts (db-s-statements self)))
	     (for s in stmts
		  (statement-dispose s))
	     (set-db-s-statements! self null)
	     (if (not (null? stmt-handle))
		 (begin
		   (odbc::sql-free-handle odbc::SQL-HANDLE-STMT stmt-handle)
		   (set-db-s-stmt-handle! self null)))
	     (if (not (null? dbc-handle))
		 (begin
		   (odbc::sql-disconnect dbc-handle)
		   (odbc::sql-free-handle odbc::SQL-HANDLE-DBC dbc-handle)
		   (set-db-s-dbc-handle! self null)))
	     (if (not (null? env-handle))
		 (begin
		   (odbc::sql-free-handle odbc::SQL-HANDLE-ENV env-handle)
		   (set-db-s-env-handle! self null))))
	   #t)		 
	 
	 ;; Sets the autocommit mode.
	 (define (db-autocommit! self flag)
	   (let ((dbc-handle (db-s-dbc-handle self))
		 (mode odbc::SQL-AUTOCOMMIT-OFF))
	     (if (not (null? dbc-handle))
		 (begin
		   (if flag
		       (set! mode odbc::SQL-AUTOCOMMIT-ON))
		   (odbc::sql-set-connect-attr 
		    dbc-handle
		    odbc::SQL-ATTR-AUTOCOMMIT 
		    mode)
		   (hash-table-put! (db-s-options self) 'auto-commit mode)))))

	 ;; Returns the autocommit mode.
	 (define (db-autocommit? self)
	   (let ((options (db-s-options self))
		 (v #f))
	     (if (not (null? options))
		 (begin
		   (set! v (hash-table-get options 'auto-commit null))
		   (if (null? v)
		       (set! v #t)
		       (begin
			 (set! v (symbol->auto-commit v))
			 (if (= v odbc::SQL-AUTOCOMMIT-ON)
			     (set! v #t)
			     (set! v #f))))))
	     v))

	 ;; Begins a transaction.
	 ;; Takes two optional arguments:
	 ;; 1. Transaction flags. This can be either 'deferred, 
	 ;;    'immediate, or 'exclusive.
	 ;; 2. Transaction name. As of now, ignored.
	 (define (db-begin-transaction self . args)
	   #t)
	 
	 (define (symbol->handle-type s)
	   (if (integer? s)
	       s
	       (case s
		 ((dbc) odbc::SQL-HANDLE-DBC)
		 ((env) odbc::SQL-HANDLE-ENV)
		 ((stmt) odbc::SQL-HANDLE-STMT)
		 (else (error "Invalid handle type.")))))

	 (define (get-handle self ht)
	   (cond
	    ((= ht odbc::SQL-HANDLE-DBC) (db-s-dbc-handle self))
	    ((= ht odbc::SQL-HANDLE-ENV) (db-s-env-handle self))
	    ((= ht odbc::SQL-HANDLE-STMT) (db-s-stmt-handle self))
	    (else (error "Invalid handle type."))))

	 (define (symbol->completion-type s)
	   (if (integer? s)
	       s
	       (case s
		 ((commit) odbc::SQL-COMMIT)
		 ((rollback) odbc::SQL-ROLLBACK)
		 (else (error "Invalid completion type.")))))

	 ;; Ends a transaction. 
	 (define db-end-transaction
	   (case-lambda
	    ((self)
	     (db-end-transaction self 'dbc 'commit))
	    ((self completion-type)
	     (db-end-transaction self 'dbc completion-type))
	    ((self handle-type completion-type)
	     (let* ((ht (symbol->handle-type handle-type))
		    (h (get-handle self ht))
		    (r (odbc::sql-end-tran ht h
					   (symbol->completion-type completion-type))))
	       (handle-error r ht h)				      null)
	     #t))) 
	 
	 ;; Commits the database transaction.
	 (define (db-commit self)
	   (db-end-transaction self 'commit))

	 ;; Rollsback the transaction.
	 (define (db-rollback self . args)
	   (db-end-transaction self 'rollback))

	 ;; Returns sql converted to the database specific 
	 ;; grammar. No converion is supported for SQLite.
	 (define (db-native-sql self sql)
	   null)

	 (define (init-stmt-handle self)
	   (let* ((dbc (db-s-dbc-handle self))
		  (sh (odbc::sql-alloc-handle odbc::SQL-HANDLE-STMT dbc)))
	     (handle-error sh odbc::SQL-HANDLE-DBC dbc)
	     (set-db-s-stmt-handle! self sh)
	     sh))

	 ;; Executes a DML command. Returns the number of rows affected.
	 (define (db-execute self sql)
	   (let ((stmt-h (db-s-stmt-handle self))
		 (h null))
	     (if (null? stmt-h)		 
		 (set! stmt-h (init-stmt-handle self)))
	     (set! h (odbc::sql-exec-direct stmt-h sql))
	     (handle-error h odbc::SQL-HANDLE-STMT stmt-h)
	     (odbc::sql-row-count stmt-h)))

	 ;; Creates a statement object using which
	 ;; SQL commands can be executed on the database.
	 (define (db-create-statement self sql)
	   (let ((stmts (db-s-statements self))
		 (dbc (db-s-dbc-handle self))
		 (h null)
		 (ret null))
	     (if (null? stmts)
		 (set! stmts (list)))
	     (set! h (odbc::sql-alloc-handle odbc::SQL-HANDLE-STMT dbc))
	     (handle-error h odbc::SQL-HANDLE-STMT dbc)
	     (handle-error (odbc::sql-prepare h sql)
			   odbc::SQL-HANDLE-STMT
			   h)
	     (set! ret (statement h self sql))
	     (set! stmts (cons ret stmts))
	     (set-db-s-statements! self stmts)
	     ret))
	 
	 ;; Creates and returns a new savepoint object
	 ;; in the current transaction. Optional argument
	 ;; specifies a name for the savepoint.
	 (define (db-save-point! self . name)
	   null)

	 ;; Removes the specified and subsequent savepoint objects 
	 ;; from the transaction.
	 (define (db-release-save-point self save-point)
	   null)

	 ;; Directly executes an SQL on the database,
	 ;; without the help from a statement object.
	 (define (raw-execute self sql)
	   (db-execute self sql))

	 (define (raw-execute-command self command args)
	   (raise-exception "raw-execute-command" 
			    "Not implemented"
			    'contract))

	 (define (statement-finalize self stmt)
	   (let* ((handle (statement-s-handle stmt))
		  (r (odbc::sql-free-handle odbc::SQL-HANDLE-STMT handle)))
	     (handle-error r odbc::SQL-HANDLE-STMT handle)
	     (set-db-s-statements! self (remove-from-list 
					 (db-s-statements self) stmt))))

	 ;; The statement interface

	 (define-struct statement-s (handle db sql bindings metadata))
	 (define-struct metadata-s (num-cols num-rows cols))
	 (define-struct column-s (name type length))

	 (define (statement h d sql)
	   (make-statement-s h d sql null null))

	 (define (symbol->sqltype s)
	   (if (integer? s)
	       s
	       (case s
		 ((wchar) odbc::SQL-C-WCHAR)
		 ((string) odbc::SQL-C-WCHAR)
		 ((char) odbc::SQL-C-CHAR)
		 ((short) odbc::SQL-C-SHORT)
		 ((sshort) odbc::SQL-C-SSHORT)
		 ((ushort) odbc::SQL-C-USHORT)
		 ((long) odbc::SQL-C-LONG)
		 ((slong) odbc::SQL-C-SLONG)
		 ((integer) odbc::SQL-C-SLONG)
		 ((ulong) odbc::SQL-C-ULONG)
		 ((float) odbc::SQL-C-FLOAT)
		 ((double) odbc::SQL-C-DOUBLE)
		 ((real) odbc::SQL-C-DOUBLE)
		 ((bit) odbc::SQL-C-BIT)
		 ((boolean) odbc::SQL-C-BIT)
		 ((tinyint) odbc::SQL-C-TINYINT)
		 ((stinyint) odbc::SQL-C-STINYINT)
		 ((utinyint) odbc::SQL-C-UTINYINT)
		 ((sbigint) odbc::SQL-C-SBIGINT)
		 ((ubigint) odbc::SQL-C-UBIGINT)
		 ((binary) odbc::SQL-C-BINARY)
		 ((blob) odbc::SQL-C-BINARY)
		 ((date) odbc::SQL-C-TYPE-DATE)
		 ((time) odbc::SQL-C-TYPE-TIME)
		 ((timestamp) odbc::SQL-C-TYPE-TIMESTAMP)
		 ((numeric) odbc::SQL-C-NUMERIC)
		 ((guid) odbc::SQL-C-GUID)
		 (else odbc::SQL-C-CHAR))))		 
	 
	 (define (sqltype->symbol s)
	   (if (symbol? s)
	       s
	       (cond
		((eq? s odbc::SQL-C-WCHAR) 'string)
		((eq? s odbc::SQL-C-CHAR) 'char)
		((eq? s odbc::SQL-C-SHORT) 'short)
		((eq? s odbc::SQL-C-SSHORT) 'sshort)
		((eq? s odbc::SQL-C-USHORT) 'ushort)
		((eq? s odbc::SQL-C-LONG) 'long)
		((eq? s odbc::SQL-C-SLONG) 'slong)
		((eq? s odbc::SQL-C-ULONG) 'ulong)
		((eq? s odbc::SQL-C-FLOAT) 'float)
		((eq? s odbc::SQL-C-DOUBLE) 'double)
		((eq? s odbc::SQL-C-BIT) 'bit)
		((eq? s odbc::SQL-C-TINYINT) 'tinyint)
		((eq? s odbc::SQL-C-STINYINT) 'stinyint)
		((eq? s odbc::SQL-C-UTINYINT) 'utinyint)
		((eq? s odbc::SQL-C-SBIGINT) 'sbigint)
		((eq? s odbc::SQL-C-UBIGINT) 'ubigint)
		((eq? s odbc::SQL-C-BINARY) 'binary)
		((eq? s odbc::SQL-C-TYPE-DATE) 'date)
		((eq? s odbc::SQL-C-TYPE-TIME) 'time)
		((eq? s odbc::SQL-C-TYPE-TIMESTAMP) 'timestamp)
		((eq? s odbc::SQL-C-NUMERIC) 'numeric)
		((eq? s odbc::SQL-C-GUID) 'guid)
		(else 'string))))
	 
	 ;; Binds a value to a prepared statement's placeholder.
	 ;; The first parameter has an index of 0.
	 (define statement-bind	   
	   (case-lambda
	    ((self index value)
	     (statement-bind self index value 'string))
	    ((self index value type)
	     (set! index (add1 index))
 	     (let ((b (statement-s-bindings self)))
 	       (if (null? b)
 		   (set! b (list)))
 	       (set-statement-s-bindings! 
 		self
 		(cons (list index (symbol->sqltype type) value) b))))))

	 (define (statement-execute self)
	   (let ((bindings (statement-s-bindings self))
		 (handle (statement-s-handle self))
		 (res null))
	     (if (null? bindings)
		 (set! res (odbc::sql-exec-direct handle
						  (statement-s-sql self)))
		 (set! res (odbc::sql-execute-with-params
			    handle
			    bindings)))
	     (handle-error res odbc::SQL-HANDLE-STMT handle)
	     (init-metadata self)))

	 (define (init-metadata self)
	   (let* ((handle (statement-s-handle self))
		  (num-cols (odbc::sql-num-result-cols handle))
		  (num-rows (odbc::sql-row-count handle))
		  (cols null)
		  (mtdt (make-metadata-s num-cols num-rows null)))
	     (if (> num-cols 0)
		 (let ((col null))
		   (set! cols (make-vector num-cols))
		   (for i in (range 1 (+ num-cols 1))
			(set! col (make-column-s
				   (odbc::sql-col-attribute 
				    handle i odbc::SQL-DESC-NAME)
				   (odbc::sql-col-attribute 
				    handle i odbc::SQL-DESC-TYPE)
				   (odbc::sql-col-attribute 
				    handle i odbc::SQL-DESC-LENGTH)))
			(vector-set! cols (- i 1) col))))
	     (set-metadata-s-cols! mtdt cols)
	     (set-statement-s-metadata! self mtdt)))

	 ;; Moves to the next row.
	 ;; Returns #f if no more rows.
	 (define (result-next self)
	   (let* ((handle (statement-s-handle self))
		  (res (odbc::sql-fetch handle)))
	     (if (symbol? res)
		 (handle-error res odbc::SQL-HANDLE-STMT handle)
		 res)))

	 ;; Returns the resultset in the following format:
	 ;; ((column-names) (row1) (row2) ... (rowN)) 
	 (define (result-with-columns->list self)
	   (let ((col-list (list))
		 (col-count (result-column-count self)))		
	     (let loop ((cols (list)) (i 0))
	       (if (< i col-count)
		   (loop (cons (result-column-name self i) cols) (add1 i))
		   (set! col-list cols)))
	     (cons col-list (result->list_helper self col-count))))
	 
	 ;; Returns the resultset in the following format:
	 ;; ((row1) (row2) ... (rowN)) 
	 (define (result->list self)
	   (result->list_helper self (result-column-count self)))
	 
	 (define (result->list_helper self col-count)
	   (let loop ((c (result-next self))
		      (row-list (list)))
	     (cond 
	      (c
	       (let inner-loop ((row (list)) (i 0))
		 (if (< i col-count)
		     (inner-loop (cons (result-string self i) row) (add1 i))
		     (begin
		       (loop (result-next self)
			     (cons row row-list))))))
	      (else
	       row-list))))

	 ;; Index starts at 0.
	 (define (result-value self index)
	   (let* ((handle (statement-s-handle self))
		  (res (odbc::sql-get-data handle
					   (add1 index)
					   odbc::SQL-C-CHAR
					   (result-column-length self index))))
	     (if (symbol? res)
		 (handle-error res 
			       odbc::SQL-HANDLE-STMT
			       handle)
		 (car res))))

	 ;; Returns the blob value at the given index as a list 
	 ;; of bytes. Index starts at 0.
	 (define (result-blob self index)
	   (result-value self index))

	 ;; Returns the float value at the given index.
	 ;; Index starts at 1.
	 (define (result-real self index)
	   (string->number (result-value self index)))

	 ;; Returns the integer value at the given index.
	 ;; Index starts at 1.
	 (define (result-integer self index)
	   (string->number (result-value self index)))

	 ;; Returns the string value at the given index.
	 ;; Index starts at 1.
	 (define (result-string self index)
	   (result-value self index))

	 ;; Returns the string value at the given index.
	 ;; Index starts at 1.
	 (define (result-symbol self index)
	   (string->symbol (result-value self index)))

	 ;; Return the number of columns in resultset
	 (define (result-column-count self)
	   (let ((mtdt (statement-s-metadata self)))
	     (if (not (null? mtdt))
		 (metadata-s-num-cols mtdt)
		 0)))

	 (define (get-column self index)
	   (let ((mtdt (statement-s-metadata self)))
	     (if (not (null? mtdt))
		 (vector-ref (metadata-s-cols mtdt) index)
		 null)))

	 ;; For a string or blob field, returns the number of bytes
	 ;; in that field.
	 ;; Index starts at 0.
	 (define (result-column-length self index)
	   (let ((col (get-column self index)))
	     (if (null? col)
		 0
		 (column-s-length col))))

	 ;; Returns the name of the column
	 (define (result-column-name self index)
	   (let ((col (get-column self index)))
	     (if (null? col)
		 ""
		 (column-s-name col))))
	 
	 ;; Returns the name of the database that this column belongs
	 (define (result-column-database-name self index)
	   "")

	 ;; Returns the name of the table that this column belongs
	 (define (result-column-table-name self index)
	   "")

	 ;; Returns the type of data in the given column
	 (define (result-column-type self index)
	   (let ((col (get-column self index)))
	     (if (null? col)
		 'char
		 (sqltype->symbol (column-s-type col)))))

	 (define (result-column-raw-type self index)
	   (let ((col (get-column self index)))
	     (if (null? col)
		 odbc::SQL-C-CHAR
		 (column-s-type col))))

	 ;; Resets the statement for re-execution.
	 ;; The bindings are not cleared.
	 (define (statement-reset self)
	   #f)

	 ;; Deletes the statement handle.
	 ;; The statement object is unusable after it is
	 ;; finalized.
	 (define (statement-dispose self)
	   (odbc::sql-free-handle odbc::SQL-HANDLE-STMT 
				  (statement-s-handle self)))
	 
	 (define (parse-connection-string s)
	   (let ((conn-options null)
		 (tokens (string-split s (list #\;))))
	     (if (not (null? tokens))
		 (begin
		   (set! conn-options (make-hash-table 'equal))
		   (for t in tokens
			(let ((opts (string-split t (list #\=))))
			  (hash-table-put! conn-options
					   (car opts)
					   (car (cdr opts)))))))
	     conn-options)))

