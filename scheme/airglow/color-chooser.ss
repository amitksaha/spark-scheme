;; Color-chooser functions.
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

(library airglow-color-chooser

	 (import (airglow-util) (exception) (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export color-chooser color-chooser-red 
		 color-chooser-green color-chooser-blue
		 color-chooser-hsb color-chooser-rgb 
		 color-chooser-hue color-chooser-saturation 
		 color-chooser-brightness color-chooser-show
		 color-chooser-color color-chooser-color!)

	 ;; Creates and initializes a Color-chooser object.
	 ;; Accepts 6 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; Returns the new color-chooser object on success.
	 (define (color-chooser . args)
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
		       (else (raise-exception "color-chooser" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-color-chooser x y w h title))

	     (if (eqv? handle null)
		 (raise-exception "color-chooser"
				  "Null handle to color-chooser."
				  null))

	     (set! self (new-widget handle 'color-chooser))
	     self))

	 ;; Gets the color values

	 (define (color-chooser-red self)
	   (spark.fltk::red (widget-handle self)))

	 (define (color-chooser-green self)
	   (spark.fltk::green (widget-handle self)))

	 (define (color-chooser-blue self)
	   (spark.fltk::blue (widget-handle self)))

	 (define (color-chooser-hsb self h s b)
	   (spark.fltk::hsb (widget-handle self) h s b))

	 (define (color-chooser-rgb self r g b)
	   (spark.fltk::rgb (widget-handle self) r g b))

	 (define (color-chooser-hue self)
	   (spark.fltk::hue (widget-handle self)))

	 (define (color-chooser-saturation self)
	   (spark.fltk::saturation (widget-handle self)))

	 (define (color-chooser-brightness self)
	   (spark.fltk::brightness (widget-handle self)))
	 
	 (define (color-chooser-color c)
	   (let ((color-index (color->integer c)))
	     (spark.fltk::get-color color-index)))

	 (define (color-chooser-color! color colors)
	   (let ((color-index (color->integer color))
		 (r 0) (g 0) (b 0))
	     (if (not (eqv? colors null))
		 (begin
		   (set! r (car colors))
		   (set! colors (cdr colors))
		   (set! g (car colors))
		   (set! colors (cdr colors))
		   (set! b (car colors))
		   (spark.fltk::set-color color-index r g b)))
	     color-index))

	 (define (color-chooser-show title colors) ;; colors is a list '(r g b)
	   (if (not (eqv? colors null))
	       (begin
		 (let ((r 0) (g 0) (b 0))
		   (set! r (car colors))
		   (set! colors (cdr colors))
		   (set! g (car colors))
		   (set! colors (cdr colors))
		   (set! b (car colors))
		   (spark.fltk::show-color-chooser title r g b))))))



