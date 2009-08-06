;; Clock functions.
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

(library airglow-clock

	(import (airglow-util) (exception)
		((prefix spark.fltk:: #%spark-fltk)))

	(export clock clock-hour clock-minute
		 clock-second clock-current-time
		 clock-current-time!)

	;; Creates and initializes a Clock widget.
	;; Accepts 5 optional arguments
	;; 1. x position
	;; 2. y position
	;; 3. width
	;; 4. height
	;; 5. title
	;; Returns the new clock object on success.
	(define (clock . args)
	  (let ((self null)
		(handle null)
		(x 0)
		(y 0)
		(w 0)
		(h 0) 
		(title "")
		(type 'square)
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
		       (raise-exception "clock" "Invalid keyword" 'contract)))
		    (set! args (cdr args))
		    (loop))))
	    
	    (set! handle (spark.fltk::fl-clock x y w h title type))
	    
	    (if (eqv? handle null)
		(raise-exception "clock"
				 "Null handle to pack."
				 null))
	    
	    (set! self (new-widget handle 'clock))
	    self))
	
	(define (clock-hour self)
	  (spark.fltk::hour (widget-handle self)))

	(define (clock-minute self)
	  (spark.fltk::minute (widget-handle self)))

	(define (clock-second self)
	  (spark.fltk::second (widget-handle self)))

	(define (clock-current-time self)
	  (spark.fltk::current-time (widget-handle self)))

	(define (clock-current-time! self t . args)
	  (if (eqv? args null)
	      (spark.fltk::current-time (widget-handle self) t)
	      (spark.fltk::current-time (widget-handle self)
					t (car args) (car (cdr args))))))
		

