;; Functions common to all Widgets and objects derived from Group
;; and Global functions used for managing UI, events etc.
;; Copyright (C) 2007  Vijay Mathew Pandyalakal
 
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

(library airglow-base
	
	(import (asserts)
		(exception)
		(airglow-util)
		((prefix spark.fltk:: #%spark-fltk)))

	(export ;; Global
	 airglow-run airglow-awake
	 airglow-contrast airglow-redraw-all
	 airglow-look-n-feel airglow-look-n-feel!
	 airglow-box-dx! airglow-box-dy!
	 airglow-box-dw! airglow-box-dh!
	 ;; threading
	 airglow-check-threads airglow-thread-manager-start
	 airglow-thread-manager-stop
	 ;; font
	 airglow-fonts! airglow-font 
	 airglow-font-name airglow-font-attributes 
	 airglow-font-sizes
	 ;; system colors
	 airglow-rgb-color airglow-system-colors 
	 airglow-foreground! airglow-background! 
	 airglow-cursor!)

	;; Global functions

	;; Runs the event loop
	(define (airglow-run)
	  (spark.fltk::run))

	(define (airglow-contrast fg bg)
	  (integer->color (spark.fltk::contrast 
			   (color->integer fg)
			   (color->integer bg))))

	(define (airglow-redraw-all)
	  (spark.fltk::redraw-all))

	(define (airglow-look-n-feel! s)
	  (let ((n (lnf->string s)))
	    (spark.fltk::scheme n)))

	(define (airglow-look-n-feel)
	  (let ((s null))
	    (set! s (spark.fltk::scheme))
	    (string->lnf s)))

	(define (airglow-box-dx! bt)
	  (spark.fltk::box-dx (boxtype->integer bt)))

	(define (airglow-box-dy! bt)
	  (spark.fltk::box-dy (boxtype->integer bt)))

	(define (airglow-box-dh! bt)
	  (spark.fltk::box-dh (boxtype->integer bt)))

	(define (airglow-box-dw! bt)
	  (spark.fltk::box-dw (boxtype->integer bt)))

	(define (airglow-awake)
	  (spark.fltk::awake))
	
	(define (airglow-check-threads)
	  (spark.fltk::check-threads))
	
	(define ag-watch-thread-running #f)

	(define (airglow-thread-manager-start)
	  (if (not ag-watch-thread-running)
	      (begin
		(spark.fltk::start-thread-manager)
		(set! ag-watch-thread-running #t))))

	(define (airglow-thread-manager-stop)
	  (if ag-watch-thread-running
	      (begin
		(spark.fltk::stop-thread-manager)
		(set! ag-watch-thread-running #f))))

	;; font

	(define (airglow-fonts!)
	  (spark.fltk::fl-set-fonts))

	(define (airglow-font f)
	  (spark.fltk::fl-get-font (font->integer f)))

	(define (airglow-font-name f)
	  (spark.fltk::fl-get-font-name (font->integer f)))

	(define (airglow-font-sizes f)
	  (spark.fltk::fl-get-font-sizes (font->integer f)))

	(define (airglow-font-attributes f)
	  (spark.fltk::fl-get-font-attributes (font->integer f)))

	;; system color
	
	(define (airglow-rgb-color c1 . args)
	  (if (eqv? args null)
	      (spark.fltk::rgb-color c1)
	      (spark.fltk::rgb-color c1
				     (car args)
				     (car (cdr args)))))

	(define (airglow-system-colors)
	  (spark.fltk::get-system-colors))

	(define (airglow-foreground! r g b)
	  (spark.fltk::fl-foreground r g b))

	(define (airglow-background! r g b)
	  (spark.fltk::fl-background r g b))

	(define (airglow-cursor! c . args)
	  (let ((fg 'black)
		(bg 'white))
	    (if (not (eqv? args null))
		(begin
		  (set! fg (car args))
		  (set! args (cdr args))))
	    (if (not (eqv? args null))
		(set! bg (car args)))		     
	    (spark.fltk::fl-cursor (cursor->integer c)
				   (color->integer fg)
				   (color->integer bg))))

	(define (string->lnf s)
	  (if (eqv? s null)
	      null
	      (begin
		(cond 
		 ((string=? s "plastic") 'aqua)
		 ((string=? s "gtk+") 'bluecurve)
		 (else null)))))

	(define (lnf->string s)
	  (case s
	    ((aqua) "plastic")
	    ((bluecurve) "gtk+")
	    (else null))))



		  