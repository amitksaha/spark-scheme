;; Valuator widget.
;; Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal

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

(library airglow-valuator

	 (import (airglow-util) (exception) (asserts)
		 (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export valuator valuator-value
		 valuator-integer-value valuator-value!
		 valuator-bounds valuator-changed?
		 valuator-clear-changed valuator-clamp 
		 valuator-display-format valuator-increment
		 valuator-maximum valuator-maximum! 
		 valuator-minimum valuator-minimum!
		 valuator-precision valuator-range 
		 valuator-round-to-nearest-step valuator-changed!
		 valuator-step valuator-step! valuator-soft valuator-soft! 
		 valuator-lstep valuator-counter-type
		 valuator-angle1 valuator-angle1! valuator-angle2
		 valuator-angle2! valuator-angles
		 valuator-dial-type valuator-scrollvalue 
		 valuator-slider valuator-slider! valuator-slider-size
		 valuator-slider-size! valuator-slider-type
		 valuator-slider-type! value-slider-text-color
		 value-slider-text-color! value-slider-text-font
		 value-slider-text-font! value-slider-text-size
		 value-slider-text-size! scrollbar-linesize 
		 scrollbar-linesize! scrollbar-value scrollbar-value!
		 value-input-cursor-color value-input-cursor-color!
		 value-input-text-color value-input-text-color!
		 value-input-text-font value-input-text-font!
		 value-input-text-size value-input-text-size!
		 value-input-soft value-input-soft!
		 value-output-text-color value-output-text-color!
		 value-output-text-font value-output-text-font!
		 value-output-text-size value-output-text-size!
		 value-output-soft value-output-soft!)

	 ;; Creates and initializes a Tabs object.
	 ;; Accepts 8 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. type
	 ;; 7. Callback
	 ;; 8. Callback argument
	 ;; Returns the new tabs object on success.
	 (define (valuator . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 0)
		 (h 0) 
		 (type null)
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
		       ((type) (set! type (car args)))
		       (else 
			(raise-exception "valuator" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-valuator x y w h title type))
	     
	     (if (eqv? handle null)
		 (raise-exception "valuator"
				  "Null handle to valuator."
				  null))

	     (set! self (new-widget handle 'valuator type))
	     self))

	 (define (valuator-value self)
	   (spark.fltk::valuator-value (widget-handle self)))

	 (define (valuator-integer-value self)
	   (spark.fltk::valuator-int-value (widget-handle self)))

	 (define (valuator-value! self v)
	   (spark.fltk::valuator-value (widget-handle self) v))				      
	 
	 (define (valuator-bounds self a b)
	   (spark.fltk::valuator-bounds (widget-handle self) a b))

	 (define (valuator-changed? self)
	   (spark.fltk::valuator-changed (widget-handle self)))

	 (define (valuator-changed! self)
	   (spark.fltk::valuator-set-changed (widget-handle self)))

	 (define (valuator-clear-changed self)
	   (spark.fltk::valuator-clear-changed (widget-handle self)))

	 (define (valuator-clamp self v)
	   (spark.fltk::clamp (widget-handle self) v))

	 (define (valuator-display-format self)
	   (spark.fltk::valuator-format (widget-handle self)))

	 (define (valuator-increment self v n)
	   (spark.fltk::valuator-incr (widget-handle self) v n))

	 (define (valuator-maximum! self v)
	   (spark.fltk::valuator-max (widget-handle self) v))

	 (define (valuator-maximum self)
	   (spark.fltk::valuator-max (widget-handle self)))

	 (define (valuator-minimum! self v)
	   (spark.fltk::valuator-min (widget-handle self) v))

	 (define (valuator-minimum self)
	   (spark.fltk::valuator-min (widget-handle self)))

	 (define (valuator-precision self p)
	   (spark.fltk::valuator-precision (widget-handle self) p))

	 (define (valuator-range self a b)
	   (spark.fltk::valuator-range (widget-handle self) a b))

	 (define (valuator-round-to-nearest-step self p)
	   (spark.fltk::valuator-round (widget-handle self) p))

	 (define (valuator-step! self p)
	   (spark.fltk::valuator-step (widget-handle self) p))

	 (define (valuator-step self)
	   (spark.fltk::valuator-step (widget-handle self)))

	 ;; adjuster
	 
	 (define (valuator-soft! self f)
	   (spark.fltk::adjuster-soft (widget-handle self) f))

	 (define (valuator-soft self)
	   (spark.fltk::adjuster-soft (widget-handle self)))

	 ;; counter

	 (define (valuator-lstep self s)
	   (spark.fltk::counter-lstep (widget-handle self) s))

	 (define (valuator-counter-type self s)
	   (spark.fltk::counter-type (widget-handle self) (countertype->integer s)))

	 ;; dial
	 
	 (define (valuator-angle1 self)
	   (spark.fltk::dial-angle1 (widget-handle self)))

	 (define (valuator-angle1! self a)
	   (spark.fltk::dial-angle1 (widget-handle self) a))

	 (define (valuator-angle2 self)
	   (spark.fltk::dial-angle2 (widget-handle self)))

	 (define (valuator-angle2! self a)
	   (spark.fltk::dial-angle2 (widget-handle self) a))

	 (define (valuator-angles self a b)
	   (spark.fltk::dial-angles (widget-handle self) a b))

	 (define (valuator-dial-type self t)
	   (spark.fltk::dial-type (widget-handle self) (dialtype->integer t)))

	 ;; slider

	 (define (valuator-scrollvalue self windowtop windowsize first totalsize)
	   (spark.fltk::scrollvalue (widget-handle self)
				    windowtop windowsize first totalsize))

	 (define (valuator-slider self)
	   (spark.fltk::slider (widget-handle self)))

	 (define (valuator-slider! self bt)
	   (spark.fltk::slider (widget-handle self) (boxtype->integer bt)))

	 (define (valuator-slider-size self)
	   (spark.fltk::slider-size (widget-handle self)))

	 (define (valuator-slider-size! self s)
	   (spark.fltk::slider (widget-handle self) s))

	 (define (valuator-slider-type self)
	   (integer->slidertype (spark.fltk::slider-type (widget-handle self))))

	 (define (valuator-slider-type! self st)
	   (spark.fltk::slider-type (widget-handle self) (slidertype->integer st)))

	 ;; value-slider

	 (define (value-slider-text-color self)
	   (integer->color (spark.fltk::value-slider-textcolor 
			    (widget-handle self))))

	 (define (value-slider-text-color! self c)
	   (spark.fltk::value-slider-textcolor (widget-handle self)
					       (color->integer c)))

	 (define (value-slider-text-font self)
	   (integer->font (spark.fltk::value-slider-textfont 
			   (widget-handle self))))

	 (define (value-slider-text-font! self c)
	   (spark.fltk::value-slider-textfont (widget-handle self)
					      (font->integer c)))

	 (define (value-slider-text-size self)
	   (spark.fltk::value-slider-textsize
	    (widget-handle self)))

	 (define (value-slider-text-size! self c)
	   (spark.fltk::value-slider-textsize (widget-handle self)
					      c))

	 ;; scrollbar

	 (define (scrollbar-linesize self)
	   (spark.fltk::scrollbar-linesize (widget-handle self)))

	 (define (scrollbar-linesize! self s)
	   (spark.fltk::scrollbar-linesize (widget-handle self) s))

	 (define (scrollbar-value self)
	   (spark.fltk::scrollbar-value (widget-handle self)))
	 
	 (define (scrollbar-value! self position size top total)
	   (spark.fltk::scrollbar-value (widget-handle self)
					position size top total))

	 ;; value-input

	 (define (value-input-text-color self)
	   (integer->color (spark.fltk::vi-textcolor 
			    (widget-handle self))))

	 (define (value-input-text-color! self c)
	   (spark.fltk::vi-textcolor (widget-handle self)
				     (color->integer c)))

	 (define (value-input-cursor-color self)
	   (integer->color (spark.fltk::vi-cursorcolor 
			    (widget-handle self))))

	 (define (value-input-cursor-color! self c)
	   (spark.fltk::vi-cursorcolor (widget-handle self)
				       (color->integer c)))

	 (define (value-input-text-font self)
	   (integer->font (spark.fltk::vi-textfont 
			   (widget-handle self))))

	 (define (value-input-text-font! self c)
	   (spark.fltk::vi-textfont (widget-handle self)
				    (font->integer c)))

	 (define (value-input-text-size self)
	   (spark.fltk::vi-textsize
	    (widget-handle self)))

	 (define (value-input-text-size! self c)
	   (spark.fltk::vi-textsize (widget-handle self)
				    c))

	 (define (value-input-soft self)
	   (spark.fltk::vi-soft
	    (widget-handle self)))

	 (define (value-input-soft! self f)
	   (spark.fltk::vi-soft (widget-handle self)
				f))

	 ;; value-output

	 (define (value-output-text-color self)
	   (integer->color (spark.fltk::vo-textcolor 
			    (widget-handle self))))

	 (define (value-output-text-color! self c)
	   (spark.fltk::vo-textcolor (widget-handle self)
				     (color->integer c)))

	 (define (value-output-text-font self)
	   (integer->font (spark.fltk::vo-textfont 
			   (widget-handle self))))

	 (define (value-output-text-font! self c)
	   (spark.fltk::vo-textfont (widget-handle self)
				    (font->integer c)))

	 (define (value-output-text-size self)
	   (spark.fltk::vo-textsize
	    (widget-handle self)))

	 (define (value-output-text-size! self c)
	   (spark.fltk::vo-textsize (widget-handle self)
				    c))

	 (define (value-output-soft self)
	   (spark.fltk::vo-soft
	    (widget-handle self)))

	 (define (value-output-soft! self f)
	   (spark.fltk::vo-soft (widget-handle self)
				f))

	 (define (countertype->integer t)
	   (if (integer? t)
	       t
	       (case t
		 ((normal) spark.fltk::FL-NORMAL-COUNTER)
		 ((simple) spark.fltk::FL-SIMPLE-COUNTER)
		 (else spark.fltk::FL-NORMAL-COUNTER))))

	 (define (dialtype->integer t)
	   (if (integer? t)
	       t
	       (case t
		 ((normal) spark.fltk::FL-NORMAL-DIAL)
		 ((line) spark.fltk::FL-LINE-DIAL)
		 ((fill) spark.fltk::FL-FILL-DIAL)
		 (else spark.fltk::FL-NORMAL-DIAL))))

	 (define (slidertype->integer t)
	   (if (integer? t)
	       t
	       (case t
		 ((horizontal) spark.fltk::FL-HORIZONTAL-SLIDER)
		 ((vertical) spark.fltk::FL-VERTICAL-SLIDER)
		 ((vert) spark.fltk::FL-VERT-SLIDER)
		 ((hor) spark.fltk::FL-HOR-SLIDER)
		 ((vert-fill) spark.fltk::FL-VERT-FILL-SLIDER)
		 ((hor-fill) spark.fltk::FL-HOR-FILL-SLIDER)
		 ((vert-nice) spark.fltk::FL-VERT-NICE-SLIDER)
		 ((hor-nice) spark.fltk::FL-HOR-NICE-SLIDER)
		 (else spark.fltk::FL-HORIZONTAL-SLIDER))))

	 (define (integer->slidertype t)
	   (cond
	    ((= t spark.fltk::FL-HORIZONTAL-SLIDER) 'horizontal)
	    ((= t spark.fltk::FL-VERTICAL-SLIDER) 'vertical)
	    ((= t spark.fltk::FL-VERT-SLIDER) 'vert)
	    ((= t spark.fltk::FL-HOR-SLIDER) 'hor)
	    ((= t spark.fltk::FL-VERT-FILL-SLIDER) 'vert-fill)
	    ((= t spark.fltk::FL-HOR-FILL-SLIDER) 'hor-fill)
	    ((= t spark.fltk::FL-VERT-NICE-SLIDER) 'vert-nice)
	    ((= t spark.fltk::FL-HOR-NICE-SLIDER) 'hor-nice)
	    (else t))))

