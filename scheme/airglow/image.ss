;; Image functions.
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

(library airglow-image

	 (import (airglow-util) (exception)
		 (asserts) (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export image image-handle
		 image-color-average image-copy
		 image-count image-depth 
		 image-grayscale image-draw
		 image-height image-width
		 image-inactivate image-uncache 
		 image-dispose image-create-tiled
		 ;; shared image
		 image-register-images image-shared-get
		 image-shared-find image-shared-release
		 image-shared-name image-shared-refcount 
		 image-shared-count image->shared)

	 ;; Layout of a Image object.
	 (define-struct image-s (handle) (make-inspector))
	 
	 ;; Creates and initializes a Image object.
	 ;; Accepts 2 arguments
	 ;; 1. File name
	 ;; 2. Type (optional). Should be one of:
	 ;; 'xbm, 'gif, 'xpm, 'jpg(or 'jpeg) 'png, 'pnm, 'bmp, 
	 ;; Returns the new image object on success.
	 (define (image file-name . args)
	   (let ((self null)
		 (handle null)
		 (type null)
		 (len 0))
	     (assert-string file-name)
	     (if (not (eqv? args null))
		 (begin
		   (set! len (length args))
		   (if (not (= len 1))
		       (raise-exception "image" 
					"Expects upto 2 arguments."
					'contract))
		   (set! type (car args))
		   (assert-symbol type)))
	     (if (eqv? type null)
		 (set! handle (spark.fltk::fl-image file-name))
		 (set! handle (spark.fltk::fl-image file-name type)))
	     
	     (if (eqv? handle null)
		 (raise-exception "new-image"
				  "Null handle to image."
				  null))

	     (set! self (make-image-s handle))
	     self))

	 (define (image-handle self)
	   (image-s-handle self))

	 (define (image-color-average self c i)
	   (spark.fltk::color-average (image-handle self)
				      (color->integer c) i))

	 (define (image-copy self . args)
	   (let ((handle null))
	     (if (eqv? args null)
		 (set! handle (spark.fltk::image-copy (image-handle self)))
		 (begin
		   (let ((w (car args))
			 (h (car (cdr args))))
		     (set! handle (spark.fltk::image-copy (image-handle self)
							  w h)))))
	     (make-image-s handle)))

	 (define (image-count self)
	   (spark.fltk::image-count (image-handle self)))

	 (define (image-depth self)
	   (spark.fltk::image-depth (image-handle self)))

	 (define (image-grayscale self)
	   (spark.fltk::grayscale (image-handle self)))

	 (define (image-draw self x y . args)
	   (if (eqv? args null)
	       (spark.fltk::image-draw (image-handle self) x y)
	       (begin
		 (let ((w (car args))
		       (h 0) (cx 0) (cy 0))
		   (set! args (cdr args))
		   (set! h (car args))
		   (set! args (cdr args))
		   (if (not (eqv? args null))
		       (begin
			 (set! cx (car args))
			 (set! args (cdr args))))
		   (if (not (eqv? args null))
		       (set! cy (car args)))
		   (spark.fltk::image-draw (image-handle self)
					   x y w h cx cy)))))

	 (define (image-height self)
	   (spark.fltk::image-height (image-handle self)))

	 (define (image-width self)
	   (spark.fltk::image-width (image-handle self)))

	 (define (image-inactivate self)
	   (spark.fltk::image-inactive (image-handle self)))

	 (define (image-uncache self)
	   (spark.fltk::uncache (image-handle self)))

	 (define (image-create-tiled self . args)
	   (if (eqv? args null)
	       (make-image-s (spark.fltk::tiled-image 
			      (image-handle self)))
	       (begin
		 (let ((w (car args))
		       (h 0))
		   (set! args (cdr args))
		   (set! h (car args))
		   (make-image-s (spark.fltk::tiled-image 
				  (image-handle self)
				  w h))))))

	 (define (image-dispose self)
	   (if (spark.fltk::image-dispose (image-handle self))
	       (begin
		 (set-image-s-handle! self null)
		 #t)
	       null))

	 ;; shared images

	 (define (image-shared-get file . args)
	   (let ((handle null))
	     (if (eqv? args null)
		 (set! handle (spark.fltk::shared-image-get file))
		 (set! handle (spark.fltk::shared-image-get file
							    (car args)
							    (car (cdr args)))))
	     (if (eqv? handle null)
		 (raise-exception "shared-get" "Null handle to image" null)
		 (make-image-s handle))))

	 (define (image-shared-find file . args)
	   (if (eqv? args null)
	       (make-image-s (spark.fltk::shared-image-find file))
	       (make-image-s (spark.fltk::shared-image-find file
							    (car args)
							    (car (cdr args))))))

	 (define (image-shared-release self)
	   (spark.fltk::shared-image-release (image-handle self)))

	 (define (image-shared-refcount self)
	   (spark.fltk::shared-image-refcount (image-handle self)))

	 (define (image-shared-name self)
	   (spark.fltk::shared-image-name (image-handle self)))

	 (define (image-shared-count)
	   (spark.fltk::shared-image-count))

	 (define (image-register-images)
	   (spark.fltk::register-images))

	 (define (image->shared image)
	   (make-image-s (spark.fltk::make-shared-image (image-handle image)))))



