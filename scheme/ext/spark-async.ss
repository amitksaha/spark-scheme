;; Asynchronous functions.
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

(module spark-async mzscheme

	(require (lib "async-channel.ss"))

	(define pool-size 5)

	(define worker-index 0)

	(define-struct worker (thread channel))
	(define-struct async-s (func args done-callback))

	(define w-sema (make-semaphore 1))

	(define workers (list))

	(define async
	  (case-lambda
	   ((func)
	    (async func null null))
	   ((func fargs)
	    (async func fargs null))
	   ((func fargs done-callback)
	    (let ((worker null) 
		  (forked #f)
		  (len (length workers)))
	      (cond 
	       ((>= len pool-size)
		(if (>= worker-index len)
		    (set! worker-index 0))
		(set! worker (list-ref workers worker-index))
		(async-channel-put (worker-channel worker)
				   (make-async-s func fargs done-callback))
		(set! worker-index (add1 worker-index)))
	       (else
		(make-new-worker func fargs done-callback)))))))

	(define (make-new-worker func fargs done-callback)
	  (let* ((ch (make-async-channel))
		 (worker (make-worker 
			  (thread (create-thread-cb ch)) ch)))
	    (async-channel-put ch (make-async-s func fargs done-callback))
	    (semaphore-wait w-sema)
	    (set! workers (cons worker workers))
	    (semaphore-post w-sema)))

	(define (create-thread-cb channel)
	  (lambda ()
	    (let loop ()
	      (let* ((a (async-channel-get channel))
		     (r null)
		     (args (async-s-args a)))
		(if (null? args)
		    (set! r ((async-s-func a)))
		    (set! r (apply (async-s-func a)
				   args)))
		(if (not (null? (async-s-done-callback a)))
		    ((async-s-done-callback a) r)))
	      (loop))))

	(define (async-release)
	  (let loop ()
	    (if (not (null? workers))
		(begin
		  (kill-thread (worker-thread (car workers)))
		  (set! workers (cdr workers))
		  (loop))))
	  (set! workers (list)))

	(define (async-pool-size)
	  pool-size)

	(define (async-pool-size! s)
	  (if (not (integer? s))
	      (error "Pool size should be an integer."))
	  (if (< s 1)
	      (set! s 1))
	  (set! pool-size s))

	;; for debugging
	(define (async-workers)
	  workers)

	(provide async async-pool-size async-pool-size! async-release async-workers))
