;; Camera functions.
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

(library airglow-3d-camera
	 
	 (import ((prefix spark.opengl:: #%spark-opengl)))

	 (export 3d-lookat 3d-ortho)

	 (define (3d-ortho left right bottom top near far)
	   (spark.opengl::gl-ortho left right bottom top near far))

	 (define (3d-viewport x y w h)
	   (spark.opengl::gl-viewport x y w h))

	 (define (3d-lookat eye-pos interest-pos up-vector-pos)
	   (let ((eye-x 0.0) (eye-y 0.0) (eye-z 0.0)
		 (center-x 0.0) (center-y 0.0) (center-z 0.0)
		 (up-x 0.0) (up-y 0.0) (up-z 0.0))
	     (if (not (null? eye-pos))
		 (begin
		   (set! eye-x (car eye-pos))
		   (set! eye-pos (cdr eye-pos))
		   (set! eye-y (car eye-pos))
		   (set! eye-pos (cdr eye-pos))
		   (set! eye-z (car eye-pos))))
	     (if (not (null? interest-pos))
		 (begin
		   (set! center-x (car interest-pos))
		   (set! interest-pos (cdr interest-pos))
		   (set! center-y (car interest-pos))
		   (set! interest-pos (cdr interest-pos))
		   (set! center-z (car interest-pos))))
	     (if (not (null? up-vector-pos))
		 (begin
		   (set! up-x (car up-vector-pos))
		   (set! up-vector-pos (cdr up-vector-pos))
		   (set! up-y (car up-vector-pos))
		   (set! up-vector-pos (cdr up-vector-pos))
		   (set! up-z (car up-vector-pos))))
	     (spark.opengl::glu-lookat eye-x eye-y eye-z
				       center-x center-y center-z
				       up-x up-y up-z))))


