;; Implementation specific functions.
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

(library airglow-3d-extras

	 (import ((prefix spark.opengl:: #%spark-opengl)) (exception))

	 (export 3d-hint)

	 ;; Specify implementation-specific hints .
	 (define (3d-hint target mode)
	   (let ((t 0) (m 0))
	     (case target
	       ((fog) (set! t spark.opengl::GL-FOG-HINT))
	       ((line-smooth) (set! t spark.opengl::GL-LINE-SMOOTH-HINT))
	       ((perspective-correction) (set! t spark.opengl::GL-PERSPECTIVE-CORRECTION-HINT))
	       ((point-smooth) (set! t spark.opengl::GL-POINT-SMOOTH))
	       ((polygon-smooth) (set! t spark.opengl::GL-POLYGON-SMOOTH))
	       (else (raise-exception "3d-hint" "Invalid target." 'contract)))
	     (case mode
	       ((fastest) (set! m spark.opengl::GL-FASTEST))
	       ((nicest) (set! m spark.opengl::GL-NICEST))
	       ((dont-care) (set! m spark.opengl::GL-DONT-CARE))
	       (else (raise-exception "3d-hint" "Invalid mode." 'contract)))
	     (spark.opengl::gl-hint t m))))



