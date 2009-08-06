;; Input field functions.
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

(library airglow-input-field

	 (import (airglow-util) (exception) (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export input-field input-field-copy
		 input-field-cut input-field-type
		 input-field-type! input-field-insert
		 input-field-mark input-field-mark!
		 input-field-maximum-length input-field-maximum-length! 
		 input-field-position? input-field-position!
		 input-field-readonly? input-field-readonly!
		 input-field-replace input-field-undo
		 input-field-wrap? input-field-wrap! 
		 input-field-cursor-color input-field-cursor-color!
		 input-field-char-at input-field-size 
		 input-field-text-color input-field-text-color!
		 input-field-text-font input-field-text-font! 
		 input-field-text-size input-field-text-size!
		 input-field-value input-field-value!)


	 ;; Creates and initializes a Input-Field widget.
	 ;; Accepts 6 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. class, which should be any one of:
	 ;; 'normal, 'file, 'float, 'int, 'multiline, 'output, 'secret
	 ;; Returns the new input-field object on success.
	 (define (input-field . args)
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
			(raise-exception "input-field" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))
	     
	     (if (or (eqv? tpe null) (eqv? tpe 'normal))
		 (set! handle (spark.fltk::fl-input x y w h title))
		 (set! handle (spark.fltk::fl-input x y w h title tpe)))

	     (if (eqv? handle null)
		 (raise-exception "input-field"
				  "Null handle to input-field."
				  null))

	     (set! self (new-widget handle 'input-field tpe))
	     (if (eqv? tpe 'radio)
		 (input-field-type! self 'radio))
	     self))

	 ;; Put the current selection between mark() and position() 
	 ;; into the specified clipboard. Give a second argument to 
	 ;; map clipboard to current text selection.
	 (define (input-field-copy self . args)
	   (if (eqv? args null)
	       (spark.fltk::copy (widget-handle self) 1)
	       (spark.fltk::copy (widget-handle self) 0)))

	 ;; Deletes the current selection. cut(n) deletes n characters 
	 ;; after the position(). cut(-n) deletes n characters before 
	 ;; the position(). cut(a,b) deletes the characters between 
	 ;; offsets a and b. A, b, and n are all clamped to the size 
	 ;; of the string. The mark and point are left where the deleted text was.
	 (define (input-field-cut self . args)
	   (if (eqv? args null)
	       (spark.fltk::cut (widget-handle self))
	       (begin
		 (let ((len (length args)))
		   (if (= len 1)
		       (spark.fltk::cut (widget-handle self)
					(car args))
		       (begin
			 (if (= len 2)
			     (spark.fltk::cut (widget-handle self)
					      (car args)
					      (car (cdr args)))
			     (raise-exception "cut" 
					      "Takes no more than 3 arguments."
					      'contract))))))))

	 ;; Copy all the previous contiguous cuts from the undo 
	 ;; information to the clipboard. This is used to make ^K work.
	 (define (input-field-copy-cuts self)
	   (spark.fltk::copy-cuts (widget-handle self)))

	 ;; Gets/Sets the input type.
	 ;; Valid types are:
	 ;; 'normal, 'float, 'int, 'multiline,
	 ;; 'secret, 'type, 'readonly, 'normal-output,
	 ;; 'multiline-output, 'input-wrap, 'multiline-input-wrap,
	 ;; 'multiline-output-wrap
	 (define (input-field-type self)
	   (integer->input-type (spark.fltk::input-type (widget-handle self))))

	 
	 (define (input-field-type! self t)
	   (spark.fltk::input-type (widget-handle self)
				   (input-type->integer t))
	   (set-widget-type! self t)
	   #t)

	 (define (input-field-insert self text)
	   (spark.fltk::insert-text (widget-handle self) text))

	 ;; Gets/Sets the current selection mark.
	 (define (input-field-mark self)
	   (spark.fltk::mark (widget-handle self)))

	 (define (input-field-mark! self m)
	   (spark.fltk::mark (widget-handle self) m))

	 ;; Gets/Sets the maximum length of the field.
	 (define (input-field-maximum-length self)
	   (spark.fltk::maximum-size (widget-handle self)))

	 (define (input-field-maximum-length! self ml)
	   (spark.fltk::maximum-size (widget-handle self) ml))

	 (define (input-field-position? self)
	   (spark.fltk::selection-position (widget-handle self)))

	 (define (input-field-position! self new-pos new-mark)
	   (spark.fltk::selection-position (widget-handle self)
					   new-pos new-mark))

	 (define (input-field-readonly? self)
	   (spark.fltk::read-only (widget-handle self)))

	 (define (input-field-readonly! self flag)
	   (spark.fltk::read-only (widget-handle self) flag))

	 (define (input-field-replace self start end text)
	   (spark.fltk::replace-text (widget-handle self)
				     start end text))
	 (define (input-field-undo self)
	   (spark.fltk::undo (widget-handle self)))

	 (define (input-field-wrap? self)
	   (spark.fltk::wrap-text (widget-handle self)))

	 ;; mode can be either #t or 'on.
	 (define (input-field-wrap! self mode)
	   (let ((arg #f))
	     (if (or mode (eqv? mode 'on))
		 (set! arg #t))
	     (spark.fltk::wrap-text (widget-handle self) arg)))

	 (define (input-field-cursor-color self)
	   (integer->color (spark.fltk::input-cursor-color 
			    (widget-handle self))))

	 (define (input-field-cursor-color! self cc)
	   (spark.fltk::input-cursor-color 
	    (widget-handle self)
	    (color->integer cc)))

	 (define (input-field-char-at self index)
	   (spark.fltk::char-at (widget-handle self)))

	 (define (input-field-size self)
	   (spark.fltk::text-length (widget-handle self)))

	 (define (input-field-text-color self)
	   (integer->color (spark.fltk::input-text-color 
			    (widget-handle self))))

	 (define (input-field-text-color! self tc)
	   (spark.fltk::input-text-color 
	    (widget-handle self)
	    (color->integer tc)))

	 (define (input-field-text-font self)
	   (integer->font (spark.fltk::input-text-font 
			   (widget-handle self))))

	 (define (input-field-text-font! self tf)
	   (spark.fltk::input-text-font 
	    (widget-handle self)
	    (font->integer tf)))

	 (define (input-field-text-size self)
	   (spark.fltk::input-text-size (widget-handle self)))

	 (define (input-field-text-size! self ts)
	   (spark.fltk::input-text-size (widget-handle self)
					ts))

	 (define (input-field-value self)
	   (spark.fltk::input-value (widget-handle self)))

	 (define (input-field-value! self v)
	   (spark.fltk::input-value (widget-handle self)
				    v))

	 (define (input-field-down-box self)
	   (integer->boxtype (spark.fltk::file-input-down-box
			      (widget-handle self))))

	 (define (input-field-down-box! self db)
	   (spark.fltk::file-input-down-box
	    (widget-handle self)
	    (boxtype->integer db)))

	 (define (input-type->integer i)
	   (case i
	     ((normal) spark.fltk::NORMAL-INPUT) 
	     ((float) spark.fltk::FLOAT-INPUT) 
	     ((int) spark.fltk::INT-INPUT) 
	     ((multiline) spark.fltk::MULTILINE-INPUT) 
	     ((secret) spark.fltk::SECRET-INPUT) 
	     ((type) spark.fltk::INPUT-TYPE) 
	     ((readonly) spark.fltk::INPUT-READONLY) 
	     ((normal-output) spark.fltk::NORMAL-OUTPUT) 
	     ((multiline-output) spark.fltk::MULTILINE-OUTPUT) 
	     ((input-wrap) spark.fltk::INPUT-WRAP) 
	     ((multiline-input-wrap) spark.fltk::MULTILINE-INPUT-WRAP) 
	     ((multiline-output-wrap) spark.fltk::MULTILINE-OUTPUT-WRAP) 
	     (else spark.fltk::NORMAL-INPUT)))

	 (define (integer->input-type i)
	   (cond
	    ((= i spark.fltk::NORMAL-INPUT) 'normal)
	    ((= i spark.fltk::FLOAT-INPUT) 'float)
	    ((= i spark.fltk::INT-INPUT) 'int)
	    ((= i spark.fltk::MULTILINE-INPUT) 'multiline)
	    ((= i spark.fltk::SECRET-INPUT) 'secret)
	    ((= i spark.fltk::INPUT-TYPE) 'type)
	    ((= i spark.fltk::INPUT-READONLY) 'readonly)
	    ((= i spark.fltk::NORMAL-OUTPUT) 'normal-output)
	    ((= i spark.fltk::MULTILINE-OUTPUT) 'multiline-output)
	    ((= i spark.fltk::INPUT-WRAP) 'input-wrap)
	    ((= i spark.fltk::MULTILINE-INPUT-WRAP) 'multiline-input-wrap)
	    ((= i spark.fltk::MULTILINE-OUTPUT-WRAP) 'multiline-output-wrap)
	    (else 'normal))))

