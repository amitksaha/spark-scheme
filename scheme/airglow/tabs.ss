;; Tabs widget.
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

(library airglow-tabs

	(import (airglow-util) (exception) (asserts)
		(util)
		((prefix spark.fltk:: #%spark-fltk)))

	(provide tabs tabs-current! tabs-current)

	;; Creates and initializes a Tabs object.
	;; Accepts 7 optional arguments
	;; 1. x position
	;; 2. y position
	;; 3. width
	;; 4. height
	;; 5. title
	;; Returns the new tabs object on success.
	(define (tabs . args)
	  (let ((self null)
		(handle null)
		(x 0)
		(y 0)
		(w 0)
		(h 0) 
		(title "")
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
		      (else (raise-exception "tabs" "Invalid keyword" 'contract)))
		    (set! args (cdr args))
		    (loop))))

	    (set! handle (spark.fltk::fl-tabs x y w h title))
	    
	    (if (eqv? handle null)
		(raise-exception "tabs"
				 "Null handle to tabs."
				 null))

	    (set! self (new-widget handle 'tabs))
	    self))

	(define (tabs-current self)
	  (let ((handle (spark.fltk::current-widget (widget-handle self))))
	    (if (not (eqv? handle null))
		(new-widget handle)
		null)))

	(define (tabs-current! self w)
	  (spark.fltk::current-widget (widget-handle self)
				      (widget-handle w))))


		 