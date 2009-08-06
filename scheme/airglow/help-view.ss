;; Help-View functions.
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

(library airglow-help-view

	 (import (airglow-util) (exception) (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export help-view help-view-directory 
		 help-view-file-name help-view-link-callback! 
		 help-view-load-url! help-view-text-length
		 help-view-text-color help-view-text-color! 
		 help-view-text-font help-view-text-font!
		 help-view-text-size help-view-text-size!
		 help-view-text help-view-text! 
		 help-view-topline help-view-topline!
		 help-view-title)

	 ;; Creates and initializes a Help-View object.
	 ;; Accepts 5 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; Returns the new help-view object on success.
	 (define (help-view . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 1)
		 (h 1) 
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
		       (else (raise-exception "help-view" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-help-view x y w h title))
	     
	     (if (eqv? handle null)
		 (raise-exception "help-view"
				  "Null handle to help-view."
				  null))

	     (set! self (new-widget handle 'help-view))
	     self))

	 ;; Returns the current directory base path 
	 ;; for the file in the buffer.
	 (define (help-view-directory self)
	   (spark.fltk::help-file-root 
	    (widget-handle self)))
	 
	 ;; Returns the current file-name
	 (define (help-view-file-name self)
	   (spark.fltk::help-file-name 
	    (widget-handle self)))

	 (define (help-view-link-callback! self cb)
	   (assert-procedure cb)
	   (if (not (= (procedure-arity cb) 2))
	       (raise-exception "link-callback" 
				"Callback should take 2 arguments."
				null))
	   (spark.fltk::link-callback (widget-handle self) cb))

	 (define (help-view-load-url! self file-url)
	   (spark.fltk::load-url (widget-handle self) file-url))

	 (define (help-view-text-length self)
	   (spark.fltk::help-text-length (widget-handle self)))

	 (define (help-view-text-color self)
	   (integer->color 
	    (spark.fltk::help-text-color (widget-handle self))))

	 (define (help-view-text-color! self c)
	   (spark.fltk::help-text-color (widget-handle self)
					(color->integer c)))

	 (define (help-view-text-font self)
	   (integer->font
	    (spark.fltk::help-text-font (widget-handle self))))

	 (define (help-view-text-font! self f)
	   (spark.fltk::help-text-font (widget-handle self)
				       (font->integer f)))

	 (define (help-view-text-size self)
	   (spark.fltk::help-text-size (widget-handle self)))

	 (define (help-view-text-size! self s)
	   (spark.fltk::help-text-size (widget-handle self)
				       s))

	 (define (help-view-text self)
	   (spark.fltk::help-text (widget-handle self)))

	 (define (help-view-text! self t)
	   (spark.fltk::help-text (widget-handle self)
				  t))

	 (define (help-view-topline self)
	   (spark.fltk::help-topline (widget-handle self)))
	 
	 (define (help-view-topline! self t)
	   (spark.fltk::help-topline (widget-handle self)
				     t))

	 (define (help-view-title self)
	   (spark.fltk::help-title (widget-handle self))))



