;; A simple stack datastructure.
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

(module spark-stack mzscheme

	(define-struct stack-s (s))
	
	(define (stack)
	  (make-stack-s (list)))

	(define (stack-push! self v)
	  (set-stack-s-s! self (cons v (stack-s-s self)))
	  v)

	(define (stack-pop! self)
	  (let* ((s (stack-s-s self))
		 (v null))
	    (if (> (length s) 0)
		(begin
		  (set! v (car s))
		  (set-stack-s-s! self (cdr s))
		  v)
		(error "Stack underflow."))))

	(define (stack-empty? self)
	  (not (> (length (stack-s-s self)) 0)))

	(define (stack-clear! self)
	  (set-stack-s-s! self (list)))

	(define (stack-length self)
	  (length (stack-s-s self)))

	(define (stack-top self)
	  (let ((s (stack-s-s self)))
	    (if (> (length s) 0)
		(car s)
		(error "Stack underflow."))))

	(define (stack-reverse self)
	  (let ((ret (stack)))
	    (set-stack-s-s! ret (reverse (stack-s-s self)))
	    ret))

	(define (stack-set! self index new-item)
	  (let ((vec (list->vector (stack-s-s self))))
	    (vector-set! vec index new-item)
	    (set-stack-s-s! self (vector->list vec))))

	(define (stack-ref self index)
	  (list-ref (stack-s-s self) index))

	(provide stack
		 stack-push!
		 stack-pop!
		 stack-top
		 stack-empty?
		 stack-length
		 stack-clear!
		 stack-reverse
		 stack-ref
		 stack-set!))