;; Scheme interface to SQLite.
;; Copyright (C) 2008  Vijay Mathew Pandyalakal

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

(library sql-sqlite

	 (import (exception) (asserts) (util)
		 ((prefix spark.sqlite:: #%spark-sqlite)))

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
	  statement-execute
	  statement-bind result-next
	  result-with-columns->list
	  result->list
	  result-blob result-integer
	  result-real result-string
	  result-symbol result-value
	  result-column-count result-column-name
	  result-column-database-name result-column-length 
	  result-column-type statement-reset 
	  statement-dispose)

	 ;; Represents a connection to an sqlite database and it's
	 ;; related configurations.
	 (define-struct db-s (handle ;; low-level database connection handle
			      name ;; database name
			      user-name
			      password
			      options
			      statements)) ;; type of options is list
	 
	 ;; Creates and initializes a sqlite-db object.
	 ;; Returns the new sqlite-db object on success.
	 (define (db . args)
	   (let ((self null) (len 0) (rest ()) (tmp null))
	     (set! self (make-db-s null ""
				   null null
				   null 
				   ()))
	     self))
	 
	 (define (db-name! self db-name)
	   (assert-string db-name)
	   (set-db-s-name! self db-name))

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
	     (case option-name 
	       ((flags) (assert-integer option-value))
	       ((vfs-name) (assert-string option-value))
	       (else (raise-exception "db-option!"
				      "Invalid options. Should be 'flags or 'vfs-name."
				      'contract)))
	     (hash-table-put! options option-name option-value)
	     (set-db-s-options! self options)))

	 ;; Opens the database and sets the low-level db-handle.
	 ;; Accept 1 optional argument:
	 ;; 1. db-name. Defaults to "".
	 ;; Returns #t on success, raises exception otherwise.
	 (define (db-open self . args)
	   (let ((len 0))
	     (if (not (eqv? args null))
		 (begin
		   (set! len (length args))
		   (if (> len 1)
		       (raise-exception "db-open" 
					"Expects 2 arguments."
					'contract))
		   (set-db-s-name! self (car args)))))
	   (let ((flags 0) (vfs-name null) (options (db-s-options self))
		 (handle null))
	     (if (not (eqv? options null))
		 (begin
		   (set! flags (hash-table-get 'flags 0))
		   (set! vfs-name (hash-table-get 'vfs-name null))))
	     (set! handle (spark.sqlite::open (db-s-name self)
					      (db-s-user-name self)
					      (db-s-password self)
					      flags
					      vfs-name))
	     (if (eqv? handle null)
		 (raise-exception "open" "sqlite::open failed." null)
		 (begin
		   (if (string? handle)
		       (raise-exception "open" handle null)
		       (begin
			 (set-db-s-handle! self handle)
			 #t))))))
	 
	 ;; Closes the low-level database and releases all
	 ;; associated resources.
	 ;; Returns true on success.
	 (define (db-close self)
	   (let ((handle (db-s-handle self))
		 (ret null)
		 (stmts (db-s-statements self)))
	     (if (not (eqv? stmts null))
		 (begin
		   (let ((stmt null) (rest stmts))
		     (let loop ()
		       (set! stmt (car rest))
		       (statement-dispose stmt)
		       (set! rest (cdr rest))
		       (if (not (eqv? rest null))
			   (loop))))
		   (set-db-s-statements! self null)))
	     (set! ret (spark.sqlite::close handle))
	     (if (eqv? ret null)
		 (begin
		   (let ((errmsg (spark.sqlite::strerror handle)))
		     (raise-exception "close" errmsg null)))
		 (begin
		   (set-db-s-handle! self null)))))
	 
	 ;; Sets the autocommit mode.
	 (define (db-autocommit! self flag)
	   (if flag
	       (db-begin-transaction self)
	       (db-end-transaction self)))

	 ;; Returns the autocommit mode.
	 (define (db-autocommit? self)
	   (let ((handle (db-s-handle self)))
	     (spark.sqlite::get-autocommit handle)))	    

	 ;; Begins a transaction.
	 ;; Takes two optional arguments:
	 ;; 1. Transaction flags. This can be either 'deferred, 
	 ;;    'immediate, or 'exclusive.
	 ;; 2. Transaction name. As of now, ignored.
	 (define (db-begin-transaction self . args)
	   (let ((sql (open-output-string))
		 (flag null)
		 (len (length args))
		 (name null))
	     (if (> len 2)
		 (raise-exception "db-begin-transaction"
				  "Takes upto 3 arguments."
				  'contract))
	     (fprintf sql "BEGIN ")	    
	     (case (car args)
	       ((DEFERRED) (set! flag "DEFERRED"))
	       ((IMMEDIATE) (set! flag "IMMEDIATE"))
	       ((EXCLUSIVE) (set! flag "EXCLUSIVE")))
	     (if (not (eqv? flag null))
		 (fprintf sql "~a " flag))
	     (fprintf sql "TRANSACTION")
	     (set! name (cdr args))
	     (if (not (eqv? name null))
		 (set! name (car args)))
	     (if (not (eqv? name null))
		 (fprintf sql " ~a" name))
	     (raw-execute (db-s-handle self) (get-output-string sql))))
	 
	 ;; Ends a transaction.
	 ;; Takes an optional argument, ie, the name of the transaction.
	 (define (db-end-transaction self . args)
	   (if (> (length args) 1)
	       (raise-exception "db-end-transaction"
				"Takes upto 2 arguments."
				'contract))
	   (raw-execute-command self "END TRANSACTION" args))
	 
	 ;; Commits the database transaction.
	 (define (db-commit self . args)
	   (db-end-transaction self args))

	 ;; Rollsback the transaction.
	 ;; Takes two optional argument, 
	 ;; 1. Name of the transaction, ignored
	 ;; 2. Savepoint object, ignored.
	 ;; upto which rollback.
	 (define (db-rollback self . args)
	   (if (> (length args) 2)
	       (raise-exception "db-rollback"
				"Takes upto 3 arguments."
				'contract))
	   (raw-execute-command self "ROLLBACK" null))

	 ;; Returns sql converted to the database specific 
	 ;; grammar. No converion is supported for SQLite.
	 (define (db-native-sql self sql)
	   (raise-exception "db-native-sql"
			    "Not supported."
			    null))

	 ;; Executes a DML command. Returns the number of rows affected.
	 (define (db-execute self sql)
	   (let ((handle (db-s-handle self)))
	     (if (raw-execute handle sql)
		 (spark.sqlite::get-changes handle)))
	   0)
	 
	 ;; Creates a statement object using which
	 ;; SQL commands can be executed on the database.
	 (define (db-create-statement self sql)
	   (assert-string sql)
	   (let ((handle (db-s-handle self))
		 (stmt null)
		 (ret null)
		 (stmts (db-s-statements self)))
	     (set! stmt (spark.sqlite::prepare handle sql))
	     (if (eqv? stmt null)
		 (raise-exception "create-statement"
				  (spark.sqlite::strerror handle)
				  null)
		 (begin
		   (set! ret (statement stmt self))
		   (set! stmts (cons ret stmts))
		   (set-db-s-statements! self stmts)
		   ret))))
	 
	 ;; Creates and returns a new savepoint object
	 ;; in the current transaction. Optional argument
	 ;; specifies a name for the savepoint.
	 (define (db-save-point! self . name)
	   (raise-exception "db-save-point!"
			    "Not supported."
			    null))

	 ;; Removes the specified and subsequent savepoint objects 
	 ;; from the transaction.
	 (define (db-release-save-point self save-point)
	   (raise-exception "db-release-save-point"
			    "Not supported."
			    null))

	 (define (raw-execute handle sql)
	   (if (eqv? (spark.sqlite::execute handle sql)
		     null)
	       (raise-exception "raw-execute"
				(spark.sqlite::get-global-error)
				null)
	       #t))

	 (define (raw-execute-command self command args)
	   (let ((sql (open-output-string))
		 (rest args)
		 (value null))
	     (fprintf sql "~a" command)
	     (if (not (eqv? args null))
		 (begin
		   (let loop ()
		     (set! value (car rest))
		     (if (not (eqv? value null))
			 (fprintf sql " ~a" value))
		     (set! rest (cdr rest))
		     (if (not (eqv? rest null))
			 (loop)))))
	     (raw-execute (db-s-handle self) (get-output-string sql))))

	 (define (statement-finalize self stmt)
	   (if (eqv? (spark.sqlite::finalize (statement-s-handle stmt))
		     null)
	       (raise-exception "statement-finalize"
				(spark.sqlite::strerror (db-s-handle self))		
				null))
	   (set-db-s-statements! self (remove-from-list 
				       (db-s-statements self) stmt)))
	 
	 ;; The statement interface

	 (define-struct statement-s (handle db))

	 (define (statement h d)
	   (make-statement-s h d))

	 ;; Binds a value to a prepared statement's placeholder.
	 ;; The first parameter has an index of 0.
	 (define (statement-bind self index value)
	   (set! index (add1 index))
	   (if (< index 1)
	       (raise-exception "statement-bind"
				"Index should be >= 0"
				null))
	   (let ((handle (statement-s-handle self)))
	     (cond ((real? value)
		    (spark.sqlite::bind-double handle index value))
		   ((integer? value)
		    (spark.sqlite::bind-int64 handle index value))
		   ((string? value)
		    (spark.sqlite::bind-text handle index value))
		   ((null? value)
		    (spark.sqlite::bind-null handle index))
		   ((boolean? value)
		    (begin
		      (let ((v 0))
			(if value
			    (set! v #t))
			(spark.sqlite::bind-int handle index v))))
		   (else (raise-exception "statement-bind"
					  "Unsupported type."
					  null)))))

	 (define (statement-execute self)
	   #t)

	 ;; Moves to the next row.
	 ;; Returns #f if no more rows.
	 (define (result-next self)
	   (let ((ret (spark.sqlite::step (statement-s-handle self))))
	     (if (eqv? ret null)
		 (raise-exception "result-next"
				  (spark.sqlite::strerror 
				   (db-s-handle (statement-s-db self)))
				  null))
	     ret))

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

	 ;; Returns the blob value at the given index as a list 
	 ;; of bytes. Index starts at 0.
	 (define (result-blob self index)
	   (let ((ret (spark.sqlite::column-blob (statement-s-handle self)
						 index)))
	     (if (eqv? ret null)
		 (begin
		   (if (not (eqv? (spark.sqlite::column-type index)
				  spark.sqlite::SQLITE-NULL))
		       (raise-exception "result-blob"
					(spark.sqlite::strerror
					 (db-s-handle (statement-s-db self))
					 null)))))
	     ret))

	 ;; Returns the float value at the given index.
	 ;; Index starts at 0.
	 (define (result-real self index)
	   (spark.sqlite::column-double (statement-s-handle self) index))

	 ;; Returns the integer value at the given index.
	 ;; Index starts at 0.
	 (define (result-integer self index)
	   (spark.sqlite::column-int64 (statement-s-handle self) index))

	 ;; Returns the string value at the given index.
	 ;; Index starts at 0.
	 (define (result-string self index)
	   (spark.sqlite::column-text (statement-s-handle self) index))

	 (define (result-symbol self index)
	   (string->symbol (result-string self index)))

	 (define (result-value self index)
	   (result-string self index))

	 ;; Return the number of columns in resultset
	 (define (result-column-count self)
	   (spark.sqlite::column-count (statement-s-handle self)))

	 ;; For a string or blob field, returns the number of bytes
	 ;; in that field.
	 ;; Index starts at 0.
	 (define (result-column-length self index)
	   (spark.sqlite::column-bytes (statement-s-handle self) index))

	 ;; Returns the name of the column
	 (define (result-column-name self index)
	   (spark.sqlite::column-name (statement-s-handle self) index))

	 ;; Returns the name of the database that this column belongs
	 (define (result-column-database-name self index)
	   (spark.sqlite::column-database-name (statement-s-handle self) index))

	 ;; Returns the name of the table that this column belongs
	 (define (result-column-table-name self index)
	   (spark.sqlite::column-table-name (statement-s-handle self) index))

	 ;; Returns the type of data in the given column
	 (define (result-column-type self index)
	   (let ((type (spark.sqlite::column-type (statement-s-handle self) 
						  index)))
	     (case type
	       ((spark.sqlite::SQLITE-INTEGER) 'integer)
	       ((spark.sqlite::SQLITE-FLOAT) 'real)
	       ((spark.sqlite::SQLITE-TEXT) 'string)
	       ((spark.sqlite::SQLITE-NULL) 'null)
	       ((spark.sqlite::SQLITE-BLOB) 'blob)
	       (else 'variant))))

	 ;; Resets the statement for re-execution.
	 ;; The bindings are not cleared.
	 (define (statement-reset self)
	   (if (eqv? (spark.sqlite::reset (statement-s-handle self))
		     null)
	       (raise-exception "statement-reset"
				(spark.sqlite::strerror (db-s-handle 
							 (statement-s-db self)))
				null)))

	 ;; Deletes the statement handle.
	 ;; The statement object is unusable after it is
	 ;; finalized.
	 (define (statement-dispose self)
	   (statement-finalize (statement-s-db self) self)
	   (set-statement-s-handle! self null)))



