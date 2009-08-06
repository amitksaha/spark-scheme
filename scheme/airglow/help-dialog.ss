;; Help-Dialog functions.
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

(library airglow-help-dialog

	(import (airglow-util) (exception) (asserts)
		((prefix spark.fltk:: #%spark-fltk)))

	(export help-dialog help-dialog-show 
		 help-dialog-hide help-dialog-load-url
		 help-dialog-position help-dialog-resize
		 help-dialog-text-size help-dialog-text-size! 
		 help-dialog-value help-dialog-value! 
		 help-dialog-visible? help-dialog-topline! 
		 help-dialog-height help-dialog-width 
		 help-dialog-x help-dialog-y
		 help-dialog-dispose)
		 
	;; Creates and initializes a Help-Dialog widget.
	;; Accepts 1 optional argument.
	;; 1. file - html file to load.
	;; Returns the new help-dialog object on success.
	(define (help-dialog . args)
	  (let ((self null)
		(handle null))
	    (set! handle (spark.fltk::fl-help-dialog))

	    (if (eqv? handle null)
		(raise-exception "help-dialog"
				 "Null handle to help-dialog."
				 null))

	    (set! self (new-widget handle 'help-dialog))
	    (if (not (eqv? args null))
		(spark.fltk::help-dialog-load handle (car args)))
	    self))

	(define (help-dialog-show self)
	  (spark.fltk::help-dialog-show 
	   (widget-handle self)))

	(define (help-dialog-hide self)
	  (spark.fltk::help-dialog-hide 
	   (widget-handle self)))

	(define (help-dialog-load-url self f)
	  (spark.fltk::help-dialog-load 
	   (widget-handle self) f))

	(define (help-dialog-position self x y)
	  (spark.fltk::help-dialog-position 
	   (widget-handle self)
	   x y))

	(define (help-dialog-resize self x y w h)
	  (spark.fltk::help-dialog-resize 
	   (widget-handle self)
	   x y w h))

	(define (help-dialog-text-size self)
	  (spark.fltk::help-dialog-text-size 
	   (widget-handle self)))

	(define (help-dialog-text-size! self s)
	  (spark.fltk::help-dialog-text-size 
	   (widget-handle self) s))

	(define (help-dialog-value self)
	  (spark.fltk::help-dialog-value
	   (widget-handle self)))

	(define (help-dialog-value! self s)
	  (spark.fltk::help-dialog-value
	   (widget-handle self) s))

	(define (help-dialog-visible? self)
	  (spark.fltk::help-dialog-visible
	   (widget-handle self)))

	(define (help-dialog-topline! self s)
	  (spark.fltk::help-dialog-topline
	   (widget-handle self) s))

	(define (help-dialog-x self)
	  (spark.fltk::help-dialog-xpos
	   (widget-handle self)))

	(define (help-dialog-y self)
	  (spark.fltk::help-dialog-ypos
	   (widget-handle self)))

	(define (help-dialog-width self)
	  (spark.fltk::help-dialog-width
	   (widget-handle self)))

	(define (help-dialog-height self)
	  (spark.fltk::help-dialog-height
	   (widget-handle self)))

	(define (help-dialog-dispose self)
	  (if (spark.fltk::help-dialog-dispose
	       (widget-handle self))
	      (begin
		(set-widget-handle! self null)
		#t)
	      null)))
				     
