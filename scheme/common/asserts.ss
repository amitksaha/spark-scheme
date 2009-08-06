;; Some common type assertions.
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

(library asserts

	 (import ((prefix se:: exception)))

	 (export assert-integer 
		 assert-string
		 assert-list
		 assert-boolean 
		 assert-procedure
		 assert-symbol
		 assert-vector)

	 (define (assert-integer i)
	   (if (not (integer? i))
	       (se::raise-exception "assert-integer" "Not an integer." null)	      
	       #t))

	 (define (assert-string s)
	   (if (not (string? s))
	       (se::raise-exception "assert-string" "Not a string." null)	      
	       #t))

	 (define (assert-list l)
	   (if (not (list? l))
	       (se::raise-exception "assert-list" "Not a list." null)	      
	       #t))

	 (define (assert-vector v)
	   (if (not (vector? v))
	       (se::raise-exception "assert-vector" "Not a vector." null)	      
	       #t))


	 (define (assert-boolean b)
	   (if (not (boolean? b))
	       (se::raise-exception "assert-boolean" 
				    "Not a boolean." null)
	       #t))

	 (define (assert-procedure p)
	   (if (not (procedure? p))
	       (se::raise-exception "assert-procedure" 
				    "Not a function object." null)
	       #t))

	 (define (assert-symbol s)
	   (if (not (symbol? s))
	       (se::raise-exception "assert-symbol" "Not a symbol." null)
	       #t)))