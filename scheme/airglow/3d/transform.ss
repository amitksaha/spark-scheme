;; Functions that do matrix transformations in a 3d space.
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

(library airglow-3d-transform

	 (import (exception) ((prefix spark.opengl:: #%spark-opengl)))

	 (export 3d-identity 3d-matrix 3d-translate 3d-rotate 3d-matrix-mode
		 3d-frustum 3d-push-matrix 3d-pop-matrix 3d-mult-matrix)

	 ;; Replaces the current matrix with the identity matrix.
	 (define (3d-identity)
	   (spark.opengl::gl-load-identity))

	 ;; Replace the current matrix with the specified matrix.
	 ;; 4x4 matrix is a list of 4 lists that contain 4 real
	 ;; numbers.
	 (define (3d-matrix 4x4-matrix)
	   (let ((v (make-vector 16))
		 (m null)
		 (index 0))
	     (while (not (null? 4x4-matrix))
		    (set! m (car 4x4-matrix))
		    (set! m (vector->list m))
		    (for i in (range 4)
			 (vector-set! v index (vector-ref m i))
			 (set! index (add1 index)))
		    (set! 4x4-matrix (cdr 4x4-matrix)))
	     (spark.opengl::gl-load-matrix-d 
	      (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)
	      (vector-ref v 4) (vector-ref v 5) (vector-ref v 6) (vector-ref v 7)
	      (vector-ref v 8) (vector-ref v 9) (vector-ref v 10) (vector-ref v 11)
	      (vector-ref v 12) (vector-ref v 13) (vector-ref v 14) (vector-ref v 15))))

	 (define (3d-push-matrix)
	   (spark.opengl::gl-push-matrix))

	 (define (3d-pop-matrix)
	   (spark.opengl::gl-pop-matrix))

	 ;; specify which matrix is the current matrix
	 (define (3d-matrix-mode mode)
	   (spark.opengl::gl-matrix-mode (matrix-mode->int mode)))

	 ;; See 3d-load-matrix for details on the 4x4-matrix argument.
	 (define (3d-mult-matrix 4x4-matrix)
	   (let ((v (make-vector 16))
		 (m null)
		 (index 0))
	     (while (not (null? 4x4-matrix))
		    (set! m (car 4x4-matrix))
		    (set! m (vector->list m))
		    (for i in (range 4)
			 (vector-set! v index (vector-ref m i))
			 (set! index (add1 index)))
		    (set! 4x4-matrix (cdr 4x4-matrix)))
	     (spark.opengl::gl-mult-matrix-d 
	      (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)
	      (vector-ref v 4) (vector-ref v 5) (vector-ref v 6) (vector-ref v 7)
	      (vector-ref v 8) (vector-ref v 9) (vector-ref v 10) (vector-ref v 11)
	      (vector-ref v 12) (vector-ref v 13) (vector-ref v 14) (vector-ref v 15))))

	 ;; This element contains a mathematical vector that represents the 
	 ;; distance along the x, y, and z axes. Computer graphics techniques 
	 ;; apply a translation transformation to position or move values with respect to
	 ;; acoordinate system. Conversely, translation means to move the origin of 
	 ;; the local coordinate system.
	 (define (3d-translate x y z)
	   (spark.opengl::gl-matrix-transform-d 'translate 
						x y z))

	 ;; Multiply the current matrix by a rotation matrix.
	 (define (3d-rotate angle x y z)
	   (spark.opengl::gl-matrix-transform-d 'rotate
						angle x y z))

	 ;; Multiply the current matrix by a perspective matrix.
	 ;; args can be a list of 6 float values or key-values.
	 ;; Valid keys are - left, right, bottom, top, zNear and
	 ;; zFar
	 (define (3d-frustum . args)
	   (if (not (null? args))
	       (begin
		 (let ((left 0.0) (right 0.0)
		       (bottom 0.0) (top 0.0)
		       (zNear 0.0) (zFar 0.0))
		   (if (not (symbol? (car args)))
		       (begin
			 (set! left (car args))
			 (set! args (cdr args))
			 (set! right (car args))
			 (set! args (cdr args))
			 (set! bottom (car args))
			 (set! args (cdr args))
			 (set! top (car args))
			 (set! args (cdr args))
			 (set! zNear (car args))
			 (set! args (cdr args))
			 (set! zFar (car args)))
		       (begin
			 (let ((key null))
			   (while (not (null? args))
				  (set! key (car args))
				  (set! args (cdr args))
				  (case key
				    ((left) (set! left (car args)))
				    ((right) (set! right (car args)))
				    ((bottom) (set! bottom (car args)))
				    ((top) (set! top (car args)))
				    ((zNear) (set! zNear (car args)))
				    ((zFar) (set! zFar (car args))))
				  (set! args (cdr args))))))
		   (spark.opengl::gl-frustum left right
					     bottom top
					     zNear zFar)))
	       null))
	 

	 (define (matrix-mode->int mode)
	   (if (integer? mode)
	       mode
	       (begin
		 (case mode
		   ((modelview) spark.opengl::GL-MODELVIEW)
		   ((projection) spark.opengl::GL-PROJECTION)
		   ((texture) spark.opengl::GL-TEXTURE)
		   (else 0))))))

