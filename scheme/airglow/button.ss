;; Button functions.
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

(library airglow-button

	(import (airglow-util) (exception) (asserts)
		((prefix spark.fltk:: #%spark-fltk)))

	(export button button-state button-state! 
		 button-toggled? button-turn-on 
		 button-down-border button-down-border! 
		 button-type button-type! button-callback-when
		 button-callback-when!)

	;; Creates and initializes a Button widget.
	;; Accepts 6 optional arguments
	;; 1. x position
	;; 2. y position
	;; 3. width
	;; 4. height
	;; 5. title
	;; 6. class, which should be any one of:
	;; 'normal
	;; 'check - creates a checkbox
	;; 'light - displayes the 'on' state by turning on a light.
	;; 'repeat - generates the callback continuously as long as it is held down.
	;; 'return - generates the callback when the user press the enter key.
	;; 'radio - radio button
	;; 'round - round button
	;; 'toggle - push button.
	;; Returns the new button object on success.
	(define (button . args)
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
		       (raise-exception "button" "Invalid keyword" 'contract)))
		    (set! args (cdr args))
		    (loop))))

	    (if (or (eqv? tpe null) (eqv? tpe 'normal))
		(set! handle (spark.fltk::fl-button x y w h title))
		(set! handle (spark.fltk::fl-button x y w h title tpe)))

	    (if (eqv? handle null)
		(raise-exception "button"
				 "Null handle to button."
				 null))

	    (set! self (new-widget handle 'button tpe))
	    (if (eqv? tpe 'radio)
		(button-type! self 'radio))
	    self))

	;; Gets/Sets the value of the button.
	;; The value is either true or false.
	(define (button-state self)
	  (spark.fltk::button-state (widget-handle self)))
	
	(define (button-state! self s)
	  (if s
	      (spark.fltk::button-state (widget-handle self) 1)
	      (spark.fltk::button-state (widget-handle self) 0)))
	
	(define (button-toggled? self)
	  (spark.fltk::button-state (widget-handle self)))
	
	;; Turns on the button. If type is 'radio all other
	;; buttons in the group are turned off.
	(define (button-turn-on self)
	  (if (eqv? (widget-type self) 'radio)
	      (spark.fltk::set-only (widget-handle self))
	      (button-state self #t)))

	;; Gets/Sets the down-box-type.
	(define (button-down-border self)
	  (integer->boxtype (spark.fltk::down-box 
			     (widget-handle self))))

	(define (button-down-border! self dbt)
	  (spark.fltk::down-box (widget-handle self) 
				(boxtype->integer dbt)))

	;; Gets/Sets the button-type.
	;; Valid arguments are 'toggle and 'radio.
	;; Will returns zero if the value has not changed.
	(define (button-type self)
	  (integer->buttontype (spark.fltk::button-type 
				(widget-handle self))))

	(define (button-type! self t)
	  (spark.fltk::button-type (widget-handle self) 
				   (buttontype->integer t)))

	;; Gets/Sets when the callback is done.
	(define (button-callback-when self)
	  (integer->when (spark.fltk::when (widget-handle self))))

	(define (button-callback-when! self w)
	  (spark.fltk::when (widget-handle self) 
			    (when->integer w)))			     
	
	(define (integer->buttontype i)
	  (cond
	    ((= i spark.fltk::FL-TOGGLE-BUTTON) 'toggle)
	    ((= i spark.fltk::FL-RADIO-BUTTON) 'radio)
	    (else 0)))

	(define (buttontype->integer b)
	  (case b
	    ((toggle) spark.fltk::FL-TOGGLE-BUTTON)
	    ((radio) spark.fltk::FL-RADIO-BUTTON)
	    (else 0))))


