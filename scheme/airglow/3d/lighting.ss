;; Light functions.
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

(library airglow-3d-lighting

	 (import ((prefix spark.opengl:: #%spark-opengl)) (exception))
	 (import (airglow-3d-util))

	 (export 3d-enable-lighting 3d-enable-light
		 3d-disable-light 3d-light-f
		 3d-light-ambient 3d-light-diffuse
		 3d-light-specular 3d-light-position
		 3d-light-spot-direction 3d-light-spot-exponent
		 3d-light-spot-cutoff 3d-light-linear-attenuation
		 3d-light-quadratic-attenuation 3d-light-constant-attenuation
		 3d-color 3d-color-ub 3d-blend-f 3d-enable-blending 3d-color-mask
		 3d-index 3d-logic-op 3d-color-material 3d-material)

	 (define (3d-enable-lighting flag)
	   (if flag
	       (spark.opengl::gl-enable spark.opengl::GL-LIGHTING)
	       (spark.opengl::gl-disable spark.opengl::GL-LIGHTING)))

	 (define (3d-enable-light id)
	   (spark.opengl::gl-enable (lightid->integer id)))

	 (define (3d-disable-light id)
	   (spark.opengl::gl-disable (lightid->integer id)))

	 ;; The args are four floating point values that
	 ;; specify the RGBA. This can also be key-values
	 ;; in the style 'red 0.1, etc. All unspecified values
	 ;; default to 0.0
	 (define (3d-light-ambient id . args)
	   (if (not (null? args))
	       (begin
		 (let ((params (get-light-rgba-params args)))
		   (3d-light-f id spark.opengl::GL-AMBIENT params)))
	       (raise-exception "3d-light-ambient" "Null params." 'contract)))

	 (define (3d-light-diffuse id . args)
	   (if (not (null? args))
	       (begin
		 (let ((params (get-light-rgba-params args)))
		   (3d-light-f id spark.opengl::GL-DIFFUSE params)))
	       (raise-exception "3d-light-diffuse" "Null params." 'contract)))

	 (define (3d-light-specular id . args)
	   (if (not (null? args))
	       (begin
		 (let ((params (get-light-rgba-params args)))
		   (3d-light-f id spark.opengl::GL-SPECULAR params)))
	       (raise-exception "3d-light-specular" "Null params." 'contract)))

	 ;; params is a list of 4 real values.
	 (define (3d-light-position id params)
	   (if (not (null? params))
	       (begin
		 (if (not (= (length params) 4))
		     (raise-exception "3d-light-position" "Null params." 'contract)
		     (3d-light-f id spark.opengl::GL-POSITION params)))
	       (raise-exception "3d-light-diffuse" "Null params." 'contract)))

	 ;; params is a list of 3 real values.
	 (define (3d-light-spot-direction id params)
	   (if (not (null? params))
	       (begin
		 (if (not (= (length params) 3))
		     (raise-exception "3d-light-spot-direction" "Null params." 'contract)
		     (3d-light-f id spark.opengl::GL-SPOT-DIRECTION params)))
	       (raise-exception "3d-light-spot-direction" "Null params." 'contract)))

	 ;; p is a single real number.
	 (define (3d-light-spot-exponent id p)
	   (3d-light-f id spark.opengl::GL-SPOT-EXPONENT p))

	 (define (3d-light-spot-cutoff id p)
	   (3d-light-f id spark.opengl::GL-SPOT-CUTOFF p))

	 (define (3d-light-linear-attenuation id p)
	   (3d-light-f id spark.opengl::GL-LINEAR-ATTENUATION p))

	 (define (3d-light-quadratic-attenuation id p)
	   (3d-light-f id spark.opengl::GL-QUADRATIC-ATTENUATION p))

	 (define (3d-light-constant-attenuation id p)
	   (3d-light-f id spark.opengl::GL-CONSTANT-ATTENUATION p))


	 ;; Directly calling this function is unsafe as it
	 ;; does not validate arguments. Use one of the specific
	 ;; light functions like 3d-light-ambient.
	 (define (3d-light-f face type params)
	   (let ((mode (light-type->integer type)))
	     (spark.opengl::gl-light-f (lightid->integer face) mode params)))

	 (define (3d-color . args)
	   (color-func spark.opengl::gl-color-d args))

	 (define (3d-color-ub . args)
	   (color-func spark.opengl::gl-color-ub args))

	 (define (3d-color-mask . args)
	   (let ((r #f) (g #f) (b #f) (a #f))
	     (while (not (null? args))
		    (case (car args)
		      ((red) (set! r #t))
		      ((green) (set! g #t))
		      ((blue) (set! b #t))
		      ((alpha) (set! a #t))
		      (else (error "Invalid argument.")))
		    (set! args (cdr args)))
	     (spark.opengl::gl-color-mask r g b a)))

	 (define (3d-index i)
	   (cond
	    ((real? i)
	     (spark.opengl::gl-index-d i))
	    (else (spark.opengl::gl-index-i i))))

	 (define (3d-logic-op mode)
	   (let ((m spark.opengl::GL-CLEAR))
	     (case mode
	       ((set) (set! m spark.opengl::GL-SET))
	       ((copy) (set! m spark.opengl::GL-COPY))
	       ((copy-inverted) (set! m spark.opengl::GL-COPY-INVERTED))
	       ((noop) (set! m spark.opengl::GL-NOOP))
	       ((invert) (set! m spark.opengl::GL-INVERT))
	       ((and) (set! m spark.opengl::GL-AND))
	       ((nand) (set! m spark.opengl::GL-NAND))
	       ((or) (set! m spark.opengl::GL-OR))
	       ((nor) (set! m spark.opengl::GL-NOR))
	       ((xor) (set! m spark.opengl::GL-XOR))
	       ((equiv) (set! m spark.opengl::GL-EQUIV))
	       ((noop) (set! m spark.opengl::GL-NOOP))
	       ((and-reverse) (set! m spark.opengl::GL-AND-REVERSE))
	       ((or-reverse) (set! m spark.opengl::GL-OR-REVERSE))
	       ((and-inverted) (set! m spark.opengl::GL-AND-INVERTED))
	       ((or-inverted) (set! m spark.opengl::GL-OR-INVERTED)))
	     (spark.opengl::gl-logic-op m)))

	 (define (3d-color-material face mode)
	   (let ((f (polyface->int face))
		 (m (materialtype->int mode)))	     
	     (spark.opengl::gl-color-material f m)))

	 (define (3d-material face mode params)
	   (let ((f (polyface->int face))
		 (m (materialtype->int mode)))
	     (spark.opengl::gl-material-f f m params)))

	 (define (color-func f . args)
	   (if (not (null? args))
	       (set! args (car args)))
	   (let ((r 0.0) (g 0.0) (b 0.0) (a 0.0))
	     (if (= (length args) 4)
		 (begin
		   (set! r (car args))
		   (set! args (cdr args))
		   (set! g (car args))
		   (set! args (cdr args))
		   (set! b (car args))
		   (set! args (cdr args))
		   (set! a (car args))
		   (set! args (cdr args)))
		 (begin
		   (let ((k null) (v null))
		     (while (not (null? args))
			    (set! k (car args))
			    (set! args (cdr args))
			    (set! v (car args))
			    (set! args (cdr args))
			    (case k
			      ((red) (set! r v))
			      ((green) (set! g v))
			      ((blue) (set! b v))
			      ((alpha) (set! a v))
			      (else (raise-exception "3d-color"
						     "Invalid key."
						     'contract)))))))
	     (f r g b a)))

	 (define (3d-blend-f sfactor dfactor)
	   (spark.opengl::gl-blend-func (blendfactor->integer sfactor)
					(blendfactor->integer dfactor)))

	 (define (3d-enable-blending flag)
	   (if flag
	       (begin
		 (spark.opengl::gl-enable spark.opengl::GL-BLEND)
		 (spark.opengl::gl-enable spark.opengl::GL-DEPTH-TEST))
	       (begin
		 (spark.opengl::gl-disable spark.opengl::GL-BLEND)
		 (spark.opengl::gl-disable spark.opengl::GL-DEPTH-TEST))))

	 (define (light-type->integer type)
	   (if (integer? type)
	       type
	       (begin
		 (let ((mode 0))
		   (case type
		     ((ambient) (set! mode spark.opengl::GL-AMBIENT))
		     ((diffuse) (set! mode spark.opengl::GL-DIFFUSE))
		     ((specular) (set! mode spark.opengl::GL-SPECULAR))
		     ((position) (set! mode spark.opengl::GL-POSITION))
		     ((spot-direction) (set! mode spark.opengl::GL-SPOT-DIRECTION))
		     ((spot-exponent) (set! mode spark.opengl::GL-SPOT-EXPONENT))
		     ((spot-cutoff) (set! mode spark.opengl::GL-SPOT-CUTOFF))
		     ((linear-attenuation) (set! mode spark.opengl::GL-LINEAR-ATTENUATION))
		     ((quadratic-attenuation) (set! mode spark.opengl::GL-QUADRATIC-ATTENUATION))
		     ((constant-attenuation) (set! mode spark.opengl::GL-CONSTANT-ATTENUATION)))
		   mode))))	     

	 (define (get-light-rgba-params args)
	   (let* ((count 4) (v (make-vector count)))
	     (if (= (length args) count)
		 (begin
		   (let ((i 0))
		     (while (not (null? args))
			    (vector-set! v i (car args))
			    (set! i (add1 i))
			    (set! args (cdr args)))))
		 (begin
		   (let ((k null) (val null))
		     (while (not (null? args))
			    (set! k (car args))
			    (set! args (cdr args))
			    (set! val (car args))
			    (set! args (cdr args))
			    (case k
			      ((red) (vector-set! v 0 val))
			      ((green) (vector-set! v 1 val))
			      ((blue) (vector-set! v 2 val))
			      ((alpha) (vector-set! v 3 val))
			      (else (raise-exception "get-light-rgba-params" 
						     "Invalid key." 'contract)))))))
	     (vector->list v)))

	 (define (lightid->integer id)
	   (if (integer? id)
	       id
	       (begin
		 (case id
		   ((light0) spark.opengl::GL-LIGHT-0)
		   ((light1) spark.opengl::GL-LIGHT-1)
		   ((light2) spark.opengl::GL-LIGHT-2)
		   ((light3) spark.opengl::GL-LIGHT-3)
		   ((light4) spark.opengl::GL-LIGHT-4)
		   ((light5) spark.opengl::GL-LIGHT-5)
		   ((light6) spark.opengl::GL-LIGHT-6)
		   ((light7) spark.opengl::GL-LIGHT-7)
		   (else 0)))))

	 (define (blendfactor->integer s)
	   (if (integer? s)
	       s
	       (begin
		 (case s
		   ((zero) spark.opengl::GL-ZERO)
		   ((one) spark.opengl::GL-ONE)
		   ((dst-color) spark.opengl::GL-DST-COLOR)
		   ((one-minus-dst-color) spark.opengl::GL-ONE-MINUS-DST-COLOR)
		   ((src-alpha) spark.opengl::GL-SRC-ALPHA)
		   ((one-minus-src-alpha) spark.opengl::GL-ONE-MINUS-SRC-ALPHA)
		   ((dst-alpha) spark.opengl::GL-DST-ALPHA)
		   ((one-minus-dst-alpha) spark.opengl::GL-ONE-MINUS-DST-ALPHA)
		   ((src-alpha-saturate) spark.opengl::GL-SRC-ALPHA-SATURATE)
		   (else 0))))))

