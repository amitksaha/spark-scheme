;; Pack functions.
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

(library airglow-pack

	 (import (airglow-util) (exception) (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export pack pack-resizable pack-resizable! 
		 pack-border-type pack-border-type! 
		 pack-spacing pack-spacing!)

	 ;; Creates and initializes a Pack widget.
	 ;; Accepts 5 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; Returns the new pack object on success.
	 (define (pack . args)
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
		       (else 
			(raise-exception "pack" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-box x y w h title))
	     
	     (if (eqv? handle null)
		 (raise-exception "pack"
				  "Null handle to pack."
				  null))
	     
	     (set! self (new-widget handle 'pack))
	     self))
	 
	 (define (pack-resizable self)
	   (widget-data self))
	 
	 (define (pack-resizable! self w)
	   (if (spark.fltk::pack-resizable (widget-handle w))
	       (begin
		 (set-widget-data! self w)
		 #t)
	       null))
	 
	 ;; Gets/Sets the box-type.
	 (define (pack-border-type self)
	   (integer->boxtype (spark.fltk::box-type (widget-handle self))))
	 
	 (define (pack-border-type! self bt)
	   (spark.fltk::box (widget-handle self) (boxtype->integer bt)))
	 
	 ;; Gets or sets the number of extra pixels of blank space 
	 ;; that are added between the children.
	 (define (pack-spacing self)
	   (spark.fltk::spacing (widget-handle self)))
	 
	 (define (pack-spacing! self s)
	   (spark.fltk::spacing (widget-handle self) s)))


