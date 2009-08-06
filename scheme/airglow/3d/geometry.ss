;; Geometry functions.
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

(library airglow-3d-geometry
	 
	 (import ((prefix spark.opengl:: #%spark-opengl))
		 (airglow-3d-util))

	 (export 3d-triangle 3d-quad 3d-quadric
		 3d-quadric-delete 3d-quadric-drawstyle
		 3d-quadric-normals 3d-quadric-orientation
		 3d-quadric-texture 3d-cylinder
		 3d-disk 3d-partial-disk 3d-sphere
		 3d-rect)

	 ;; Provides the information needed to for a mesh to 
	 ;; bind vertex attributes together and then organize those
	 ;; vertices into an individual triangle.
	 ;; vertices is a list of lists, that contain 
	 ;; the x, y and z points.
	 (define (3d-triangle vertices)
	   (render-shape spark.opengl::GL-TRIANGLES vertices))

	 (define (3d-quad vertices)
	   (render-shape spark.opengl::GL-QUADS vertices))

	 (define (render-shape type vertices)
	   (if (list? vertices)
	       (set! vertices (list->vector vertices)))
	   (let ((num-vs (vector-length vertices))
		 (i 0) (v null) (tmp null) (vlen 0))
	     (spark.opengl::gl-begin type)
	     (while (< i num-vs)
		    (set! v (vector-ref vertices i))
		    (if (list? v)
			(set! v (list->vector v)))
		    (set! tmp (vector-ref v 0))
		    (set! vlen (vector-length v))
		    (case tmp
		      ((color)
		       (cond ((= vlen 5)
			      (spark.opengl::gl-color-d (vector-ref v 1)
							(vector-ref v 2)
							(vector-ref v 3)
							(vector-ref v 4)))
			     (else
			      (spark.opengl::gl-color-d (vector-ref v 1)
							(vector-ref v 2)
							(vector-ref v 3)))))
		      ((texture)
		       (cond ((= vlen 2)
			      (spark.opengl::gl-tex-coord-d (vector-ref v 1)))
			     ((= vlen 3)
			      (spark.opengl::gl-tex-coord-d (vector-ref v 1)
							    (vector-ref v 2)))					
			     ((= vlen 4)
			      (spark.opengl::gl-tex-coord-d (vector-ref v 1)
							    (vector-ref v 2)
							    (vector-ref v 3)))
			     ((= vlen 5)
			      (spark.opengl::gl-tex-coord-d (vector-ref v 1)
							    (vector-ref v 2)
							    (vector-ref v 3)
							    (vector-ref v 4)))))
		      (else
		       (cond ((= vlen 2)
			      (spark.opengl::gl-vertex-d tmp (vector-ref v 1)))
			     ((= vlen 3)
			      (spark.opengl::gl-vertex-d tmp
							 (vector-ref v 1)
							 (vector-ref v 2)))
			     (else
			      (spark.opengl::gl-vertex-d tmp
							 (vector-ref v 1)
							 (vector-ref v 2)
							 (vector-ref v 3))))))
		    (set! i (add1 i)))
	     (spark.opengl::gl-end)))

	 (define (3d-quadric)
	   (spark.opengl::glu-new-quadric))

	 (define (3d-quadric-delete self)
	   (spark.opengl::glu-delete-quadric self))

	 (define (3d-quadric-drawstyle self ds)
	   (spark.opengl::glu-quadric-drawstyle self (quadric-drawstyle->integer ds)))

	 (define (3d-quadric-normals self n)
	   (spark.opengl::glu-quadric-normals self (quadric-normals->integer n)))

	 (define (3d-quadric-texture self t)
	   (let ((v spark.opengl::GLU-FALSE))
	     (if t
		 (set! v spark.opengl::GLU-TRUE))
	     (spark.opengl::glu-quadric-texture self v)))

	 (define (3d-quadric-orientation self ds)
	   (spark.opengl::glu-quadric-orientation self (quadric-orientation->integer ds)))

	 (define (3d-cylinder quadric base-radius to-radius height
			      slices stack)
	   (spark.opengl::glu-cylinder quadric base-radius to-radius height
				       slices stack))

	 (define (3d-disk quadric inner-radius outer-radius
			  slices loop)
	   (spark.opengl::glu-disk quadric inner-radius outer-radius
				   slices loop))

	 (define (3d-partial-disk quadric inner-radius outer-radius
				  slices loop
				  start-angle sweep-angle)
	   (spark.opengl::glu-partial-disk quadric inner-radius outer-radius
					   slices loop
					   start-angle sweep-angle))

	 (define (3d-sphere quadric radius
			    slices stack)
	   (spark.opengl::glu-sphere quadric radius
				     slices stack))

	 (define (3d-rect x1 y1 x2 y2)
	   (spark.opengl::gl-rectd x1 y1 x2 y2))

	 (define (quadric-drawstyle->integer ds)
	   (if (integer? ds)
	       ds
	       (begin
		 (case ds
		   ((fill) spark.opengl::GLU-FILL)
		   ((line) spark.opengl::GLU-LINE)
		   ((silhouette) spark.opengl::GLU-SILHOUETTE)
		   ((point) spark.opengl::GLU-POINT)
		   (else 0)))))

	 (define (quadric-normals->integer n)
	   (if (integer? n)
	       n
	       (begin
		 (case n
		   ((none) spark.opengl::GLU-NONE)
		   ((flat) spark.opengl::GLU-FLAT)
		   ((smooth) spark.opengl::GLU-SMOOTH)
		   (else 0)))))

	 (define (quadric-orientation->integer o)
	   (if (integer? o)
	       o
	       (begin
		 (case o
		   ((inside) spark.opengl::GLU-INSIDE)
		   ((outside) spark.opengl::GLU-OUTSIDE)
		   (else 0))))))


