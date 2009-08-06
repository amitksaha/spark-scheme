;; Simple wrappers for MzScheme compile module. 
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

(library compile
	 
	 (require (prefix c:: (lib "compile.ss")))

	 (export compile-file)

	 (define (compile-file file . args)
	   (let ((dest null) (filter null))
	     (if (not (null? args))
		 (begin
		   (set! dest (car args))
		   (set! args (cdr args))))
	     (if (not (null? args))
		 (set! filter (car args)))
	     (cond
	      ((and (null? dest) (null? filter))
	       (c::compile-file file))
	      ((not (null? dest))
	       (if (not (null? filter))
		   (c::compile-file file dest filter)
		   (c::compile-file file dest)))))))