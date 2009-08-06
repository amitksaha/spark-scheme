;; Numeric procedures mainly for R6RS compliance.
;; Copyright (C) 2007, 2008, 2009 Vijay Mathew Pandyalakal

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

(module spark-numeric-ext mzscheme

	(provide nan? infinite? finite?)
	
	(define (nan? x)
	  (eq? x +nan.0))
	
	(define (infinite? x)
	  (eq? x +inf.0))
	
	(define (finite? x)
	  (not (infinite? x))))
