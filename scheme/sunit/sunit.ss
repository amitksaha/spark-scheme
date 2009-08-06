;; A unit test framework.
;; Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal

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

(library sunit

	 (export make-test sunit-test sunit-tests sunit-suite)

	 (define-struct test (t r))

	 (define (sunit-test tst verbose cmpr)
	   (let* ((t (test-t tst)) 		   
		  (expected-result (test-r tst))
		  (r null))
	     (try	    
	      (set! r (eval t))
	      (if (null? cmpr)
		  (begin
		    (if (or (string? expected-result)
			    (list? expected-result)
			    (vector? expected-result))
			(set! cmpr equal?)
			(set! cmpr eq?))))
	      (if (not (cmpr r expected-result))
		  (begin
		    (if verbose
			(printf "~a failed with result ~a. Expected result was ~a.~n" t r expected-result))
		    #f)
		  (begin
		    (if verbose
			(printf "~a succeeded with  result ~a.~n" t r))
		    #t))
	      (catch (lambda (x)
		       (if (eq? expected-result 'error)
			   (begin
			     (if verbose
				 (printf "~a succeeded with  result ~a.~n" t x))
			     #t)
			   (begin
			     (if verbose
				 (printf "~a failed with exception ~a. Expected result was ~a.~n" t x expected-result))
			     #f)))))))

	 (define (sunit-tests title tests . args)
	   (let ((verbose #f) (cmpr null))
	     (if (not (null? args))
		 (begin
		   (set! verbose (car args))
		   (set! args (cdr args))))
	     (if (not (null? args))
		 (set! cmpr (car args)))
	     (if verbose
		 (printf "Running tests for ~a~n" title))
	     (let ((len (length tests)) (i 0) (s 0))
	       (let loop ()
		 (if (< i len)
		     (begin
		       (if (sunit-test (list-ref tests i) verbose cmpr)
			   (set! s (add1 s)))
		       (set! i (add1 i))
		       (loop))))
	       (printf "Results for ~a tests: ~a run, ~a succeeded, ~a failed.~n" title len s (- len s))
	       (list len s (- len s)))))

	 (define (sunit-suite title tests . args)
	   (printf "Running tests suite ~a~n" title)
	   (let ((total-ran 0) (s 0) (f 0)
		 (v #f) (r null))
	     (if (not (null? args))
		 (set! v (car args)))
	     (while (not (null? tests))
		    (let ((test null) (ttle null) (tsts null))
		      (set! ttle (car tests))
		      (set! tests (cdr tests))
		      (set! tsts (car tests))
		      (set! tests (cdr tests))
		      (set! r (sunit-tests ttle tsts v))
		      (set! total-ran (+ total-ran (car r)))
		      (set! r (cdr r))
		      (set! s (+ s (car r)))
		      (set! r (cdr r))
		      (set! f (+ f (car r)))))
	     (printf "Total tests: ~a~nSucceeded: ~a~nFailed: ~a~n"
		     total-ran s f)
	     (list total-ran s f))))
