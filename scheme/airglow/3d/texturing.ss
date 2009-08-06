;; Texturing.
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

(library airglow-3d-texturing

	 (import ((prefix spark.opengl:: #%spark-opengl))
		 (exception) (airglow-3d-util))

	 (export 3d-texture 3d-bind-texture 3d-texture-param!
		 3d-texture-image!)

	 (define-struct texture-s (id))

	 (define current-texture null)

	 (define (3d-texture . args)
	   (let ((k null) (v null)
		 (image-name null)
		 (image-data null)
		 (image-w 0) (image-h 0)
		 (default-params #t))
	     (while (not (null? args))
		    (set! k (car args))
		    (set! args (cdr args))
		    (if (null? args)
			(raise-exception "3d-texture"
					 "Invalid key-value pair."
					 'contract))
		    (set! v (car args))
		    (set! args (cdr args))
		    (case k
		      ((image-file) (set! image-name v))
		      ((image-data) (set! image-data v))
		      ((width) (set! image-w v))
		      ((height) (set! image-h v))
		      ((default-params) (set! default-params v))
		      (else
		       (raise-exception "3d-texture"
					"Invalid key." 'contract))))
	     (let ((textures (spark.opengl::gl-gen-textures 1))
		   (id null) (img-arg image-name) (self null))
	       (if (not (null? textures))
		   (begin
		     (set! id (car textures))
		     (set! self (make-texture-s id))
		     (if (null? img-arg)
			 (set! img-arg image-data))
		     (if (not (null? img-arg))
			 (begin
			   (let ((r (3d-texture-image! self image-w image-h img-arg)))
			     (if (not (eq? r #t))
				 (begin
				   (case r
				     ((file-open-error)
				      (raise-exception "3d-texture" "file-open-error" null))
				     ((image-creation-error)
				      (raise-exception "3d-texture" "image-creation-error" null))
				     (else
				      (raise-exception "3d-texture" "unknown-error" null))))))))

		     (if default-params
			 (begin
			   (3d-texture-param! self 'min-filter 'linear)
			   (3d-texture-param! self 'mag-filter 'linear)))

		     (make-texture-s id))
		   (begin
		     (raise-exception "3d-texture" "Failed to generate texture." null))))))

	 (define (3d-bind-texture self . args)
	   (if (not (eq? current-texture (texture-s-id self)))
	       (begin
		 (if (spark.opengl::gl-bind-texture '2d (texture-s-id self))
		     (begin
		       (set! current-texture (texture-s-id self))
		       #t)
		     #f))
	       #t))

	 (define (3d-texture-param! self p v)
	   (3d-bind-texture self)
	   (let ((f null))
	     (if (integer? v)
		 (set! f spark.opengl::gl-tex-parameter-i)
		 (set! f spark.opengl::gl-tex-parameter-f))
	     (f '2d (texparam->int p) (texparamval->int v))))

	 (define (3d-texture-image! self width height img-arg)	   
	   (3d-bind-texture self)
	   (spark.opengl::gl-tex-image '2d 0 
				       spark.opengl::GL-RGB
				       width height
				       0 spark.opengl::GL-RGB
				       img-arg)))



