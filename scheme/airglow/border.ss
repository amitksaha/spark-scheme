;; Box widget.
;; Copyright (C) 2007  Vijay Mathew Pandyalakal
 
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

(library airglow-border

	(import (airglow-util)
		(exception)
		(asserts)
		((prefix spark.fltk:: #%spark-fltk)))

	(export border)

	;; Creates and initializes a Border widget.
	;; Accepts 6 optional arguments
	;; 1. x position
	;; 2. y position
	;; 3. width
	;; 4. height
	;; 5. title
	;; 6. class, which should be any one of the types
	;; supported by the boxtype->int function.
	;; Returns the new button object on success.
	(define (border . args)
	  (let ((self null)
		(handle null)
		(x 0)
		(y 0)
		(w 0)
		(h 0) 
		(title "")
		(type null)
		(opt null))
	    (let loop ()
	      (if (not (eqv? args null))
		  (begin	
		    (set! opt (car args))
		    (set! args (cdr args))
		    (case opt
		      ((x) (set! x (car args)))
		      ((y) (set! y (car args)))
		      ((w) (set! w (car args)))
		      ((h) (set! h (car args)))
		      ((title) (set! title (car args)))
		      ((type) (set! type (car args)))
		      (else 
		       (raise-exception "border" "Invalid keyword" 'contract)))
		    (set! args (cdr args))
		    (loop))))

	    (if (eqv? type null)
		(set! handle (spark.fltk::fl-box x y w h title))
		(set! handle (spark.fltk::fl-box x y w h title 
						 (boxtype->integer type))))
	    (if (eqv? handle null)
		(raise-exception "border"
				 "Null handle to box."
				 null))

	    (set! self (new-widget handle 'box type))
	    self)))


