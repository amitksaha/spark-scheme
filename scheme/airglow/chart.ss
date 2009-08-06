;; Chart widget.
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

(library airglow-chart

	 (import (airglow-util) (exception) (asserts)
		 (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export chart chart-add chart-autosize
		 chart-autosize! chart-bounds
		 chart-bounds! chart-type
		 chart-type! chart-clear
		 chart-insert chart-replace
		 chart-maxsize chart-maxsize!
		 chart-size)

	 ;; Creates and initializes a Chart widget.
	 ;; Accepts 7 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. type. Any one of 'bar, 'filled, 'horbar,
	 ;; 'line, 'pie, 'special-pie, spike.
	 ;; 7. Callback
	 ;; 8. Callback argument
	 ;; Returns the new chart object on success.
	 (define (chart . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 0)
		 (h 0) 
		 (title "")
		 (type 'bar)
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
		       (else (raise-exception "chart" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-chart x y w h title type))
	     
	     (if (eqv? handle null)
		 (raise-exception "chart"
				  "Null handle to chart."
				  null))

	     (set! self (new-widget handle 'chart type))
	     self))

	 ;; Adds a value to the chart.
	 ;; value should be a float.
	 ;; Optional args are:
	 ;; 1. title
	 ;; 2. color
	 (define (chart-add self value . args)	  
	   (if (eqv? args null)
	       (spark.fltk::add-chart-value (widget-handle self) value)
	       (begin
		 (let ((title (car args))
		       (color 0))
		   (set! args (cdr args))
		   (if (not (eqv? args null))
		       (begin
			 (set! color (car args))
			 (spark.fltk::add-chart-value (widget-handle self) 
						      value title 
						      (color->integer color)))
		       (spark.fltk::add-chart-value (widget-handle self) 
						    value title 
						    0))))))

	 (define (chart-autosize self)	  
	   (spark.fltk::autosize (widget-handle self)))

	 (define (chart-autosize! self flag)	  
	   (let ((f 0))
	     (if flag
		 (set! f 1))
	     (spark.fltk::autosize (widget-handle self) f)))

	 (define (chart-bounds self)	  
	   (spark.fltk::bounds (widget-handle self)))

	 (define (chart-bounds! self a b)	  
	   (spark.fltk::bounds (widget-handle self) a b))

	 (define (chart-type self)	  
	   (integer->charttype (spark.fltk::chart-type (widget-handle self))))

	 (define (chart-type! self t)	  
	   (spark.fltk::chart-type (widget-handle self) (charttype->integer t)))

	 (define (chart-clear self)	  
	   (spark.fltk::clear-chart (widget-handle self)))

	 (define (chart-insert self pos value . args)	  
	   (if (eqv? args null)
	       (spark.fltk::insert-chart-value (widget-handle self) 
					       pos value)
	       (begin
		 (let ((title (car args))
		       (color 0))
		   (set! args (cdr args))
		   (if (not (eqv? args null))
		       (begin
			 (set! color (car args))
			 (spark.fltk::insert-chart-value (widget-handle self) 
							 pos value title 
							 (color->integer color)))
		       (spark.fltk::insert-chart-value (widget-handle self) 
						       pos value title 
						       0))))))

	 (define (chart-replace self pos value . args)	  
	   (if (eqv? args null)
	       (spark.fltk::replace-chart-value (widget-handle self) 
						pos value)
	       (begin
		 (let ((title (car args))
		       (color 0))
		   (set! args (cdr args))
		   (if (not (eqv? args null))
		       (begin
			 (set! color (car args))
			 (spark.fltk::replace-chart-value (widget-handle self) 
							  pos value title 
							  (color->integer color)))
		       (spark.fltk::replace-chart-value (widget-handle self) 
							pos value title 
							0))))))

	 (define (chart-maxsize self)	  
	   (spark.fltk::maxsize (widget-handle self)))

	 (define (chart-maxsize! self s)	  
	   (spark.fltk::maxsize (widget-handle self) s))

	 (define (chart-size self)	  
	   (spark.fltk::count-chart-values (widget-handle self)))
	 
	 (define (integer->charttype i)
	   (cond
	    ((= i spark.fltk::FL-BAR-CHART) 'bar)
	    ((= i spark.fltk::FL-FILLED-CHART) 'filled)
	    ((= i spark.fltk::FL-HORBAR-CHART) 'horbar)
	    ((= i spark.fltk::FL-LINE-CHART) 'line)
	    ((= i spark.fltk::FL-PIE-CHART) 'pie)
	    ((= i spark.fltk::FL-SPECIALPIE-CHART) 'special-pie)
	    ((= i spark.fltk::FL-SPIKE-CHART) 'spike)
	    (else (raise-exception "integer->charttype" 
				   "Not a valid chart type." null))))

	 (define (charttype->integer c)
	   (case c
	     ((bar) spark.fltk::FL-BAR-CHART)
	     ((filled) spark.fltk::FL-FILLED-CHART)
	     ((horbar) spark.fltk::FL-HORBAR-CHART)
	     ((line) spark.fltk::FL-LINE-CHART)
	     ((pie) spark.fltk::FL-PIE-CHART)
	     ((special-pie) spark.fltk::FL-SPECIALPIE-CHART)
	     ((spike) spark.fltk::FL-SPIKE-CHART)
	     (else (raise-exception "charttype->integer" 
				    "Not a valid chart symbol." null)))))


