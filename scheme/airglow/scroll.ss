;; Scroll bar widget.
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

(library airglow-scroll

	 (import (airglow-util) (exception) (asserts)
		 (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export scroll scroll-type scroll-type! 
		 scroll-align scroll-align!
		 scroll-x-position scroll-y-position
		 scroll-position!)

	 ;; Creates and initializes a Scroll widget.
	 ;; Accepts 8 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. type. 'horizontal, 'vertical, 'both, 
	 ;; 'horizontal-always, 'vertical-always or 'both-always
	 ;; Returns the new scroll object on success.
	 (define (scroll . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 0)
		 (h 0) 
		 (title "")
		 (tpe null)
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
		       ((type) (set! tpe (car args)))
		       (else 
			(raise-exception "scroll" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-scroll x y w h title))
	     
	     (if (eqv? handle null)
		 (raise-exception "scroll"
				  "Null handle to scroll."
				  null))

	     (set! self (new-widget handle 'scroll tpe))

	     (if (not (eqv? tpe null))
		 (scroll-type! self tpe))
	     self))

	 (define (scroll-type self)
	   (integer->scrolltype 
	    (spark.fltk::scroll-type (widget-handle self))))

	 (define (scroll-type! self t)
	   (spark.fltk::scroll-type (widget-handle self) 
				    (scrolltype->integer t)))

	 (define (scroll-align self)
	   (list->align spark.fltk::scroll-align (widget-handle self)))

	 (define (scroll-align! self a)
	   (spark.fltk::scroll-align (widget-handle self) (align->list a)))

	 (define (scroll-x-position self)
	   (spark.fltk::scroll-xpos (widget-handle self)))

	 (define (scroll-y-position self)
	   (spark.fltk::scroll-ypos (widget-handle self)))

	 (define (scroll-position! self m)
	   (spark.fltk::scroll-pos (widget-handle self) m))

	 (define (scrolltype->integer s)
	   (case s
	     ((horizontal) spark.fltk::FL-SCROLL-HORIZONTAL)
	     ((vertical) spark.fltk::FL-SCROLL-VERTICAL)
	     ((both) spark.fltk::FL-SCROLL-BOTH)
	     ((horizontal-always) spark.fltk::FL-SCROLL-HORIZONTAL-ALWAYS)
	     ((vertical-always) spark.fltk::FL-SCROLL-VERTICAL-ALWAYS)
	     ((both-always) spark.fltk::FL-SCROLL-BOTH-ALWAYS)
	     (else (raise-exception "scrolltype->integer"
				    "Not a valid type." null))))

	 (define (integer->scrolltype i)
	   (cond 
	    ((= i spark.fltk::FL-SCROLL-HORIZONTAL) 'horizontal)
	    ((= i spark.fltk::FL-SCROLL-VERTICAL) 'vertical)
	    ((= i spark.fltk::FL-SCROLL-BOTH) 'both)
	    ((= i spark.fltk::FL-SCROLL-HORIZONTAL-ALWAYS) 'horizontal-always)
	    ((= i spark.fltk::FL-SCROLL-VERTICAL-ALWAYS) 'vertical-always)
	    ((= i spark.fltk::FL-SCROLL-BOTH-ALWAYS) 'both-always)
	    (else (raise-exception "integer->scrolltype"
				   "Not a valid type." null)))))


