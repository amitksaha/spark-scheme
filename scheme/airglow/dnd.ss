;; Drag-n-Drop support.
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

(library airglow-dnd 

	(import (airglow-util) (asserts)
		((prefix spark.fltk:: #%spark-fltk)))

	(export dnd-copy dnd)

	(define (dnd-copy data . args)
	  (let ((clipboard 1))
	    (if (not (null? args))
		(set! clipboard (clipboard->integer (car args))))
	    (spark.fltk::fl-copy data clipboard)))	  

	(define (dnd . args)
	  (if (not (null? args))
	      (dnd-copy (car args) 0))
	  (spark.fltk::fl-dnd))

	(define (clipboard->integer c)
	  (if (integer? c)
	      c
	      (begin
		(case c
		  ((selection) 0)
		  ((primary) 1)
		  (else 1))))))
	


