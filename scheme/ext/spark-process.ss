;; An Erlang like process system.
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

(module spark-process mzscheme

	 (require (lib "async-channel.ss"))
	 (require (lib "thread.ss"))
	 
	 (provide spawn send receive
		  register unregister 
		  watch alive? kill
		  remoting-port
		  remoting! remoting?
		  descriptor message-channel)
	 
	 (define process-id 0)
	 (define sem (make-semaphore 1))
	 (define-struct proc-s (thread-desc channel))
	 (define processes (make-hash-table))
	 (define name-registry (make-hash-table 'equal))
	 (define watchers (make-hash-table))
	 (define remoting-port 1383)

	 ;; Spawns a process and returns the process id.
	 ;; The argument is a callback function to the process.
	 ;; It takes one argument - the value received from a call
	 ;; to (send v).
	 (define spawn
	   (case-lambda 
	    ((callback) (spawn callback ()))
	    ((callback args)
	     (let ((pid 0) (channel null))		

	     ;; Modifies the package global state.
	     (set! pid (+ process-id 1))
	     (set! process-id pid)
	     (set! channel (make-async-channel))
	     ;; :~

	     (let ((proc-obj (make-proc-s null channel))
		   (thrd null))
	       (update-proc-table pid proc-obj)
	       (set! thrd (thread 
			   (lambda () 			   
			     (apply callback (cons pid args))
			     (cleanup-process pid 'dead))))

	       (set-proc-s-thread-desc! proc-obj thrd))	     

	     pid))))

	 (define (update-proc-table pid proc-obj)
	   (semaphore-wait sem)
	   (hash-table-put! processes pid proc-obj)
	   (semaphore-post sem))
	    
	 (define (cleanup-process pid exit-type)
	   (semaphore-wait sem)
	   (let ((p (hash-table-get processes pid null)))
	     (if (not (null? p)) 
		 (begin
		   (hash-table-remove! processes pid)
		   (set! p (hash-table-get watchers pid null))
		   (let loop ((w p))
		     (if (not (null? w))
			 (begin
			   (send (car w) (list exit-type pid))
			   (loop (cdr w)))))
		   (hash-table-remove! watchers pid))))
	   (semaphore-post sem))

	 ;; Recieives a message on the process message channel.
	 (define (receive pid)
	   (async-channel-get (proc-s-channel (hash-table-get processes pid))))

	 ;; Registers a name for the process.
	 ;; The process can be later identified using
	 ;; this name, whereever a pid is be used.
	 (define (register pid name)
	   (semaphore-wait sem)
	   (hash-table-put! name-registry name pid)
	   (semaphore-post sem))

	 ;; Removes the regustered name of the pid.
	 (define (unregister pid name)
	   (semaphore-wait sem)
	   (hash-table-remove! name-registry name)
	   (semaphore-post sem))

	 ;; Adds a watcher to the process.
	 ;; The watcher is notified when the 
	 ;; process exits.
	 (define (watch pid-to-watch
			pid-watcher)
	   (semaphore-wait sem)
	   (let ((w (hash-table-get watchers pid-to-watch (list))))
	     (set! w (append w (list pid-watcher)))
	     (hash-table-put! watchers pid-to-watch w))
	   (semaphore-post sem))	   

	 (define (alive? pid)
	   (set! pid (find-pid pid))
	   (let ((pobj (hash-table-get processes pid null)))
	     (if (not (null? pobj)) (not (thread-dead? (proc-s-thread-desc pobj)))
		 #f)))

	 (define (kill pid)
	   (set! pid (find-pid pid))
	   (let ((pobj (hash-table-get processes pid null)))
	     (if (not (null? pobj)) 
		 (begin
		   (kill-thread (proc-s-thread-desc pobj))
		   (cleanup-process pid 'killed)))))

	 (define (descriptor pid)
	   (let ((pobj (hash-table-get processes pid null)))
	     (if (not (null? pobj)) 
		 (proc-s-thread-desc pobj)
		 null)))		 

	 (define (message-channel pid)
	   (let ((pobj (hash-table-get processes pid null)))
	     (if (not (null? pobj)) 
		 (proc-s-channel pobj)
		 null))) 

	 (define (find-pid pid)
	   (if (string? pid) (set! pid (hash-table-get name-registry pid null)))
	   (if (null? pid) (error "Process name is not registered."))
	   pid)

	 (define (string-index-of str ch)
	   (let ((len (string-length str))
		 (idx -1))
	     (let loop ((i 0))
	       (if (< i len)
		   (begin
		     (if (char=? (string-ref str i) ch)
			 (begin
			   (set! idx i)
			   (set! i len)))
		     (loop (add1 i)))))
	     idx))

	 (define (tokenize-remote-pid pid idx)
	   (let ((remote-pid (substring pid 0 idx))
		 (host (substring pid (add1 idx) (string-length pid))))
	     (cons remote-pid host)))

	 (define (send-remote pid v idx)
	   (let* ((pid-tokens (tokenize-remote-pid pid idx))
		  (host-port (get-host-port (cdr pid-tokens)))
		  (ret #t))
	     (let-values (((in-port out-port)
			   (tcp-connect (car host-port)
					(cdr host-port))))
			 (file-stream-buffer-mode out-port 'line)
			 (write (cons (car pid-tokens) v) out-port)
			 (if (not (null? in-port))
			     (close-input-port in-port))
			 (if (not (null? out-port))
			     (close-output-port out-port))
			 ret)))

	 (define (get-host-port host)
	   (let ((idx (string-index-of host #\:)))
	     (if (> idx 0)
		 (cons
		  (substring host 0 idx)
		  (string->number (substring host (add1 idx))))
		 (cons
		  host
		  remoting-port))))

	 (define remoting-flag #f)

	 (define (remoting! . args)
	   (if (not remoting-flag)
	       (let ((port remoting-port))
		 (if (not (null? args))
		     (set! port (car args)))
		 (set! remoting-flag (not remoting-flag))
		 (if remoting-flag
		     (thread
		      (lambda ()
			(run-server port 
				    (lambda (in out)
				      (handle-remote-message (read in)))
				    #f)))))))

	 (define (remoting?) remoting-flag)

	 (define (handle-remote-message pid-message)
	   (send (car pid-message) (cdr pid-message)))
	   
	 ;; Sends a message to a process.
	 ;; pid can be either the process id or
	 ;; a registered process name.
	 ;; v can be any valid Scheme expression.
	 (define (send pid v)
	   (let ((idx -1))
	     (if (string? pid)
		 (set! idx (string-index-of pid #\@)))
	     (if (> idx 0)
		 (send-remote pid v idx)
		 (begin
		   (set! pid (find-pid pid))
		   (let ((pobj (hash-table-get processes pid null)))
		     (if (null? pobj) (error "Invalid pid."))
		     (let ((channel (proc-s-channel pobj)))
		       (async-channel-put channel v))))))))



	 