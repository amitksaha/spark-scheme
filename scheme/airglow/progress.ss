;; Progress bar widget.
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

(library airglow-progress

	 (import (airglow-util) (exception) (asserts)
		 (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export progress progress-maximum
		 progress-maximum! progress-minimum
		 progress-minimum! progress-value
		 progress-value!)

	 ;; Creates and initializes a Progress object.
	 ;; Accepts 7 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. Callback
	 ;; 7. Callback argument
	 ;; Returns the new progress object on success.
	 (define (progress . args)
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
		       (else (raise-exception "progress" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-progress x y w h title))
	     
	     (if (eqv? handle null)
		 (raise-exception "progress"
				  "Null handle to progress."
				  null))

	     (set! self (new-widget handle 'progress))
	     self))

	 (define (progress-maximum self)
	   (spark.fltk::progress-max (widget-handle self)))

	 (define (progress-maximum! self m)
	   (spark.fltk::progress-max (widget-handle self) m))

	 (define (progress-minimum self)
	   (spark.fltk::progress-min (widget-handle self)))

	 (define (progress-minimum! self m)
	   (spark.fltk::progress-min (widget-handle self) m))

	 (define (progress-value self)
	   (spark.fltk::progress-value (widget-handle self)))

	 (define (progress-value! self m)
	   (spark.fltk::progress-value (widget-handle self) m)))



