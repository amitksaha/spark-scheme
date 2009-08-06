;; 3D rendering utilities.
;; Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

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

(library airglow-3d-rendering

	 (import (exception) ((prefix spark.opengl:: #%spark-opengl))
		 (airglow-3d-util))

	 (export 3d-color-clear 3d-depth-clear 3d-clear-color!
		 3d-enable 3d-disable 3d-depth-func 3d-gen-lists
		 3d-new-list 3d-end-list 3d-call-list 3d-delete-lists
		 3d-fog 3d-enable-fog 3d-flush 3d-cull-face 3d-front-face
		 3d-edge-flag 3d-line-stipple 3d-line-width 3d-point-size
		 3d-polygon-mode 3d-polygon-stipple 3d-normal
		 3d-copy-pixels 3d-read-pixels 3d-pixel-map 3d-pixel-store
		 3d-pixel-zoom 3d-pixel-transfer)

	 ;; Specifies whether a render target surface is 
	 ;; to be cleared, and which value to use.
	 ;; The optional args contains four float values 
	 ;; representing the red, green, blue, and alpha channels.
	 ;; This can be a vector or list of four real numbers, or key-value
	 ;; pairs like: 'red 0.1 'green 0.2 etc
	 (define (3d-color-clear . args)
	   (let ((colors (make-colors-vector args)))
	     (if (not (eq? colors null))
		 (3d-clear-color! colors)))
	   (clear 'color))

	 ;; Before drawing, render target surfaces may need resetting 
	 ;; to a blank canvas or to a default. This function specify which 
	 ;; value to use. If no clearing statement is included, the target
	 ;; surface is unchanged as rendering begins. Ths optional argument
	 ;; is the depth value usedwhen the depth buffer is cleared. 
	 ;; The initial value is 1.
	 (define (3d-depth-clear . args)
	   (if (not (eq? args null))
	       (spark.opengl::gl-clear-depth (car args)))
	   (clear 'depth))

	 ;; Specifies the red, green, blue, and alpha
	 ;; values used by color-clear, depth-clear and stencil-clear to clear 
	 ;; the color buffers.  Values specified are clampedto the range [0,1].
	 (define (3d-clear-color! . colors)
	   (if (not (eq? colors null))
	       (begin
		 (let ((c (car colors)))
		   (cond
		    ((vector? c) (clear-color-vec! c))
		    ((list? c) (clear-color-list! c))
		    (else (raise-exception "3d-clear-color!" "Expected vector or list" 'contract))))
		 #t)
	       #f))

	 ;; Enable/Disable server side GL capabilities.
	 (define (3d-enable cap)
	   (spark.opengl::gl-enable (capability->int cap)))

	 (define (3d-disable cap)
	   (spark.opengl::gl-disable (capability->int cap)))

	 (define (3d-depth-func f)
	   (spark.opengl::gl-depth-func (depthfunc->int f)))

	 ;; display lists

	 (define (3d-gen-lists num)
	   (spark.opengl::gl-gen-lists num))

	 (define (3d-new-list id mode)
	   (spark.opengl::gl-new-list id (listmode->int mode)))

	 (define (3d-end-list)
	   (spark.opengl::gl-end-list))

	 (define (3d-call-list id)
	   (spark.opengl::gl-call-list id))

	 (define (3d-delete-lists start-id range)
	   (spark.opengl::gl-delete-lists start-id range))

	 (define (3d-cull-face mode)
	   (let ((m spark.opengl::GL-FRONT))
	     (if (eq? mode 'back)
		 (set! m spark.opengl::GL-BACK))
	     (spark.opengl::gl-cull-face m)))

	 (define (3d-edge-flag f)
	   (if (boolean? f)
	       (spark.opengl::gl-edge-flag f)))

	 (define (3d-front-face mode)
	   (let ((m spark.opengl::GL-CW))
	     (if (eq? mode 'ccw)
		 (set! m spark.opengl::GL-CCW))
	     (spark.opengl::gl-front-face m)))

	 (define (3d-line-stipple factor pattern)
	   (spark.opengl::gl-line-stipple factor pattern))

	 (define (3d-line-width width)
	   (spark.opengl::gl-line-width width))

	 (define (3d-point-size s)
	   (spark.opengl::gl-point-size s))

	 (define (3d-polygon-mode face mode)
	   (spark.opengl::gl-polygon-mode (polyface->int face)
					  (polymode->int mode)))

	 (define (3d-normal x y z)
	   (spark.opengl::gl-normal-3f x y z))

	 (define (3d-copy-pixels x y w h type)
	   (let ((t (pixeltype->int type)))
	     (spark.opengl::gl-copy-pixels x y w h t)))

	 (define (3d-read-pixels x y w h format type)
	   (let ((f (pixelformat->int format))
		 (t (pixeldatatype->int type)))
	     (spark.opengl::gl-read-pixels x y w h f t)))

	 (define (3d-pixel-map map table)
	   (spark.opengl::gl-pixel-map-f (pixelmap->int map) table))

	 (define (3d-pixel-store pname param)
	   (if (integer? param)
	       (spark.opengl::gl-pixel-store-i (pixelpack->int pname) param)
	       (spark.opengl::gl-pixel-store-f (pixelpack->int pname) param)))

	 (define (3d-pixel-zoom x y)
	   (spark.opengl::gl-pixel-zoom x y))

	 (define (3d-pixel-transfer pname param)
	   (if (integer? param)
	       (spark.opengl::gl-pixel-transfer-i (pixeltransfer->int pname) param)
	       (spark.opengl::gl-pixel-transfer-f (pixeltransfer->int pname) param)))

	 ;; pattern is a 32x32 list of bytes.
	 (define (3d-polygon-stipple pattern)
	   (spark.opengl::gl-polygon-stipple pattern))

	 ;; :~

	 ;; fog
	 
	 (define (3d-fog pname param)
	   (spark.opengl::gl-fog-f (fog-param->int pname) 
				   (fog-coord->const param)))

	 (define (3d-enable-fog flag)
	   (if flag
	       (spark.opengl::gl-enable spark.opengl::GL-FOG)
	       (spark.opengl::gl-disable spark.opengl::GL-FOG)))

	 (define (3d-flush)
	   (spark.opengl::gl-flush))

	 ;; :~

	 (define (clear buffer-flag)
	   (let ((buffer 0))
	     (case buffer-flag
	       ((color)
		(set! buffer spark.opengl::GL-COLOR-BUFFER-BIT))
	       ((depth)
		(set! buffer spark.opengl::GL-DEPTH-BUFFER-BIT))
	       ((stencil)
		(set! buffer spark.opengl::GL-STENCIL-BUFFER-BIT))
	       (else
		(raise-exception "clear" "Invalid buffer flag." 'contract)))
	     (spark.opengl::gl-clear buffer)))

	 (define (make-colors-vector args)
	   (if (not (eq? args null))
	       (begin
		 (cond
		  ((vector? (car args))
		   (car args))
		  ((list? (car args))
		   (list->vector (car args)))
		  (else
		   (let ((colors (vector 0 0 0 0))
			 (k null))
		     
		     (while (not (eq? args null))
			    (set! k (car args))
			    (set! args (cdr args))
			    (case k
			      ((red) (vector-set! colors 0 (car args)))
			      ((green) (vector-set! colors 1 (car args)))
			      ((blue) (vector-set! colors 2 (car args)))
			      ((alpha) (vector-set! colors 3 (car args))))
			    (set! args (cdr args)))		 
		     colors))))
	       null))

	 (define (clear-color-vec! colors)
	   (spark.opengl::gl-clear-color (vector-ref colors 0)
					 (vector-ref colors 1)
					 (vector-ref colors 2)
					 (vector-ref colors 3)))

	 (define (clear-color-list! colors)
	   (let ((vec (vector 0 0 0 0)))
	     (for i in (range 4)
		  (vector-set! vec i (car colors))
		  (set! colors (cdr colors)))
	     (clear-color-vec! vec))))

