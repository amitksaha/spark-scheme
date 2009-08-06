;; Drawing functions.
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

(library airglow-graphics

	 (import (exception) (airglow-util) (airglow-image)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export graphics-draw-box graphics-draw-frame 
		 graphics-clip graphics-unclip 
		 graphics-clipped? graphics-clip-box
		 graphics-color graphics-color! 
		 graphics-line-style graphics-draw-point 
		 graphics-draw-rectf graphics-draw-rect 
		 graphics-draw-line graphics-draw-polygon
		 graphics-draw-filled-polygon graphics-draw-xy-line 
		 graphics-draw-yx-line graphics-draw-arc
		 graphics-draw-pie
		 ;; 2-d
		 graphics-2d-push-matrix graphics-2d-pop-matrix
		 graphics-2d-scale graphics-2d-translate 
		 graphics-2d-rotate graphics-2d-mult-matrix 
		 graphics-2d-transform graphics-2d-transformed-vertex 
		 graphics-2d-begin graphics-2d-end
		 graphics-2d-gap graphics-2d-add-vertex
		 graphics-2d-curve graphics-2d-arc
		 graphics-2d-circle
		 ;; text
		 graphics-draw-text graphics-measure 
		 graphics-font-height graphics-descent 
		 graphics-text-width
		 ;;
		 graphics-font graphics-font! 
		 graphics-font-size
		 ;; overlay
		 graphics-overlay-rect graphics-overlay-clear
		 ;; offscreen
		 graphics-create-offscreen graphics-delete-offscreen
		 ;;begin-offscreen end-offscreen
		 graphics-copy-offscreen)

	 (define (graphics-draw-box bt x y w h c)
	   (spark.fltk::fl-draw-box (boxtype->integer  bt)
				    x y w h
				    (color->integer c)))
	 (define (graphics-draw-frame s x y w h r)
	   (if r
	       (spark.fltk::fl-frame2 s x y w h)
	       (spark.fltk::fl-frame s x y w h)))

	 (define (graphics-clip . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-push-no-clip)
	       (begin
		 (let ((x 0) (y 0) (w 0) (h 0))
		   (set! x (car args))
		   (set! args (cdr args))
		   (set! y (car args))
		   (set! args (cdr args))
		   (set! w (car args))
		   (set! args (cdr args))
		   (set! h (car args))
		   (spark.fltk::fl-push-clip x y w h)))))

	 (define (graphics-unclip)
	   (spark.fltk::fl-pop-clip))

	 (define (graphics-clipped? x y w h)
	   (spark.fltk::fl-not-clipped x y w h))

	 (define (graphics-clip-box x y w h)
	   (spark.fltk::fl-clip-box x y w h))

	 (define (graphics-color)
	   (spark.fltk::fl-color))

	 (define (graphics-color! . args)
	   (if (= (length args) 1)
	       (spark.fltk::fl-color (color->integer (car args)))
	       (begin
		 (let ((r 0) (g 0) (b 0))
		   (set! r (car args))
		   (set! args (cdr args))
		   (set! g (car args))
		   (set! args (cdr args))
		   (set! b (car args))
		   (spark.fltk::fl-color r g b)))))

	 (define (graphics-line-style style . args)
	   (let ((s (linestyle->integer style))
		 (width 0))
	     (if (not (eqv? args null))
		 (set! width (car args)))
	     (spark.fltk::fl-line-style s width)))

	 (define (graphics-draw-point x y)
	   (spark.fltk::fl-point x y))

	 (define (graphics-draw-rectf x y w h . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-rectf x y w h)
	       (begin
		 (let ((r 0) (g 0) (b 0))
		   (set! r (car args))
		   (set! args (cdr args))
		   (set! g (car args))
		   (set! args (cdr args))
		   (set! b (car args))
		   (set! args (cdr args))
		   (spark.fltk::fl-rectf x y w h r g b)))))

	 (define (graphics-draw-rect x y w h . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-rect x y w h)
	       (spark.fltk::fl-rect x y w h 
				    (color->integer (car args)))))

	 (define (graphics-draw-line x y x1 y1 . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-line x y x1 y1)
	       (spark.fltk::fl-line x y x1 y1
				    (car args)
				    (car (cdr args)))))

	 (define (graphics-draw-polygon x y x1 y1 x2 y2 . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-loop x y x1 y1 x2 y2)
	       (spark.fltk::fl-loop x y x1 y1 x2 y2
				    (car args)
				    (car (cdr args)))))

	 (define (graphics-draw-filled-polygon x y x1 y1 x2 y2 . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-polygon x y x1 y1 x2 y2)
	       (spark.fltk::fl-polygon x y x1 y1 x2 y2
				       (car args)
				       (car (cdr args)))))

	 (define (graphics-draw-xy-line x y x1 . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-xyline x y x1)
	       (begin
		 (let ((len (length args)))
		   (cond
		    ((= len 4) (spark.fltk::fl-xyline x y x1 (car args)))
		    ((= len 5) (spark.fltk::fl-xyline x y x1 
						      (car args)
						      (car (cdr args)))))))))

	 (define (graphics-draw-yx-line x y y1 . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-yxline x y y1)
	       (begin
		 (let ((len (length args)))
		   (cond
		    ((= len 4) (spark.fltk::fl-yxline x y y1 (car args)))
		    ((= len 5) (spark.fltk::fl-yxline x y y1 
						      (car args)
						      (car (cdr args)))))))))

	 (define (graphics-draw-arc x y w h a1 a2)
	   (spark.fltk::fl-arc x y w h a1 a2))

	 (define (graphics-draw-pie x y w h a1 a2)
	   (spark.fltk::fl-pie x y w h a1 a2))

					; 2d

	 (define (graphics-2d-push-matrix)
	   (spark.fltk::fl-push-matrix))

	 (define (graphics-2d-pop-matrix)
	   (spark.fltk::fl-pop-matrix))

	 (define (graphics-2d-scale x . y)
	   (if (eqv? y null)
	       (spark.fltk::fl-scale x)
	       (spark.fltk::fl-scale x (car y))))

	 (define (graphics-2d-translate x y)
	   (spark.fltk::fl-translate x y))

	 (define (graphics-2d-rotate x)
	   (spark.fltk::fl-rotate x))

	 (define (graphics-2d-mult-matrix a b c d x y)
	   (spark.fltk::fl-mult-matrix a b c d x y))

	 (define (graphics-2d-transform x y coord)
	   (case coord
	     ((x) (spark.fltk::fl-transform-x x y))
	     ((y) (spark.fltk::fl-transform-y x y))
	     ((dx) (spark.fltk::fl-transform-dx x y))
	     ((dy) (spark.fltk::fl-transform-dy x y))
	     (else 0)))

	 (define (graphics-2d-transformed-vertex x y)
	   (spark.fltk::fl-transformed-vertex x y))

	 (define (graphics-2d-begin type)
	   (case type
	     ((points) (spark.fltk::fl-begin-points))
	     ((line) (spark.fltk::fl-begin-line))
	     ((polygon) (spark.fltk::fl-begin-loop))
	     ((complex-polygon) (spark.fltk::fl-begin-complex-polygon))))
	 
	 (define (graphics-2d-end type)
	   (case type
	     ((points) (spark.fltk::fl-end-points))
	     ((line) (spark.fltk::fl-end-line))
	     ((polygon) (spark.fltk::fl-end-loop))
	     ((complex-polygon) (spark.fltk::fl-end-complex-polygon))))

	 (define (graphics-2d-gap)
	   (spark.fltk::fl-gap))

	 (define (graphics-2d-add-vertex x y)
	   (spark.fltk::fl-vertex x y))

	 (define (graphics-2d-curve x y x1 y1 x2 y2 x3 y3)
	   (spark.fltk::fl-curve x y x1 y1 x2 y2 x3 y3))

	 (define (graphics-2d-arc x y r start end)
	   (spark.fltk::fl-2d-arc x y r start end))

	 (define (graphics-2d-circle x y r)
	   (spark.fltk::fl-circle x y r))
	 
	 ;; text

	 (define (graphics-draw-text s x y . args)
	   (if (eqv? args null)
	       (spark.fltk::fl-draw-text s x y)
	       (begin
		 (let ((w 0) (h 0)
		       (a 0) (i null)
		       (ds #f))
		   (set! w (car args))
		   (set! args (cdr args))
		   (set! h (car args))
		   (set! args (cdr args))
		   (set! w (align->integer (car args)))
		   (set! args (cdr args))
		   (set! i (car args))
		   (if (not (eqv? i null))
		       (set! i (image-handle i)))
		   (set! args (cdr args))
		   (set! ds (car args))
		   (spark.fltk::fl-draw-text s x y w h a i ds)))))

	 (define (graphics-measure text . draw-symbols)
	   (if (eqv? draw-symbols null)
	       (spark.fltk::fl-measure text)
	       (spark.fltk::fl-measure text #t)))

	 (define (graphics-font-height)
	   (spark.fltk::fl-height))
	 
	 (define (graphics-descent)
	   (spark.fltk::fl-descent))

	 (define (graphics-text-width s)
	   (spark.fltk::fl-width s))

	 (define (graphics-font! f s)
	   (spark.fltk::fl-font (font->integer f) s))

	 (define (graphics-font)
	   (integer->font (spark.fltk::fl-font)))

	 (define (graphics-font-size)
	   (spark.fltk::fl-size))

	 (define (graphics-overlay-rect x y w h)
	   (spark.fltk::fl-overlay-rect x y w h))
	 
	 (define (graphics-overlay-clear)
	   (spark.fltk::fl-overlay-clear))

	 ;; offscreen

	 (define (graphics-create-offscreen w h)
	   (spark.fltk::fl-create-offscreen w h))

	 (define (graphics-delete-offscreen os)
	   (spark.fltk::fl-delete-offscreen os))

	 ;;(define (graphics-begin-offscreen os)
	 ;;  (spark.fltk::fl-begin-offscreen os))

	 ;;(define (graphics-end-offscreen)
	 ;;  (spark.fltk::fl-end-offscreen))

	 (define (graphics-copy-offscreen x y w h src-os src-x src-y)
	   (spark.fltk::fl-copy-offscreen x y w h src-os src-x src-y))

	 (define (linestyle->integer ls)
	   (case ls
	     ((solid) spark.fltk::FL-SOLID)
	     ((dash) spark.fltk::FL-DASH)
	     ((dot) spark.fltk::FL-DOT)
	     ((dash-dot) spark.fltk::FL-DASHDOT)
	     ((dash-dot-dot) spark.fltk::FL-DASHDOTDOT)
	     ((cap-flat) spark.fltk::FL-CAP-FLAT)
	     ((cap-round) spark.fltk::FL-CAP-ROUND)
	     ((cap-square) spark.fltk::FL-CAP-SQUARE)
	     ((join-miter) spark.fltk::FL-JOIN-MITER)
	     ((join-round) spark.fltk::FL-JOIN-ROUND)
	     ((join-bevel) spark.fltk::FL-JOIN-BEVEL)
	     (else (raise-exception "linestyle->integer"
				    "Invalid linestyle" null)))))


