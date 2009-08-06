;; Window functions.
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

(library airglow-window 

	 (import (airglow-widget) (airglow-util) (exception)
		 (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export window window-border-shown?
		 window-show-border window-cursor
		 window-cursor! window-fullscreen 
		 window-hotspot window-iconize 
		 window-icon-label window-icon-label! 
		 window-title window-title!
		 window-modal? window-modal! 
		 window-resize window-show
		 window-hide window-shown?
		 window-size-range window-xclass
		 window-xclass!)

	 ;; Save old window position here
	 (define-struct pos (x y w h))
	 
	 ;; Creates and initializes a Window object.
	 ;; Accepts 6 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. class, which should be either of:
	 ;; 'double - creates a double buffered window.
	 ;; 'single - default on most platforms.
	 ;; 'gl - a window which can render opengl.
	 ;; Returns the new window object on success.
	 (define (window . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 300)
		 (h 400) 
		 (title "")
		 (type 'single)
		 (tmp null))
	     (let loop ()
	       (if (not (eqv? args null))
		   (begin	
		     (set! tmp (car args))
		     (set! args (cdr args))
		     (if (eqv? args null)
			 (begin
			   (raise-exception "window"
					    "Invalid keyword argument for (window)"
					    'contract)))
		     (case tmp
		       ((x) (set! x (car args)))
		       ((y) (set! y (car args)))
		       ((w) (set! w (car args)))
		       ((h) (set! h (car args)))
		       ((title) (set! title (car args)))
		       ((type) (set! type (car args)))
		       (else (raise-exception "window"
					      "Invalid keyword for (window)"
					      'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-window x y w h title type))

	     (if (eqv? handle null)
		 (raise-exception "window"
				  "Null handle to window."
				  null))

	     (set! self (new-widget handle 'window type))
	     (let ((p (make-pos 0 0 0 0)))
	       (set-widget-data! self p))
	     self))

	 ;; Returns true if the border is shown.
	 (define (window-border-shown? self)
	   (if (= (spark.fltk::border (widget-handle self)) 1)
	       #t
	       #f))

	 ;; Shows/hides the window border	    
	 (define (window-show-border self flag)
	   (if flag
	       (spark.fltk::border (widget-handle self) 1)
	       (spark.fltk::border (widget-handle self) 0)))

	 ;; Sets/Gets the cursor
	 (define (window-cursor self)
	   (integer->cursor (spark.fltk::change-cursor 
			     (widget-handle self))))

	 (define (window-cursor! self c)
	   (spark.fltk::change-cursor (widget-handle self) 
				      (cursor->integer c)))
	 
	 ;; Turns on/off fullscreen mode.
	 ;; If the fullscreen mode is turned off, position and size
	 ;; previous to the fullscreen mode is restored.	
	 (define (window-fullscreen self flag)
	   (let ((p (widget-data self)))
	     (if flag	      
		 (begin
		   (set-pos-x! p (widget-x self))
		   (set-pos-y! p (widget-y self))
		   (set-pos-w! p (widget-width self))
		   (set-pos-h! p (widget-height self))
		   (set-widget-data! self p)
		   (spark.fltk::fullscreen (widget-handle self)))
		 (spark.fltk::fullscreen-off (widget-handle self)
					     (pos-x p)
					     (pos-y p)
					     (pos-w p)
					     (pos-h p)))))

	 ;; Position the window so that mouse is pointing at the given position.
	 (define (window-hotspot self x y offscreen)
	   (spark.fltk::hotspot (widget-handle self) x y offscreen))

	 (define (window-iconize self)
	   (spark.fltk::iconize (widget-handle self)))

	 ;; Gets/Sets the icon label.
	 (define (window-icon-label self)
	   (spark.fltk::icon-label (widget-handle self)))

	 (define (window-icon-label! self il)
	   (spark.fltk::icon-label (widget-handle self) il))

	 ;; Gets/Sets the title bar label.
	 (define (window-title self)
	   (spark.fltk::title-bar-label (widget-handle self)))

	 (define (window-title! self t)
	   (spark.fltk::title-bar-label (widget-handle self) t))

	 ;; Returns true if the window is modal.
	 (define (window-modal? self)
	   (spark.fltk::modal (widget-handle self)))

	 ;; Turns on/off the modal flag.
	 (define (window-modal! self flag)
	   (if flag
	       (spark.fltk::set-modal (widget-handle self))
	       (spark.fltk::set-non-modal (widget-handle self))))

	 ;; Resize the window.
	 (define (window-resize self x y w h)
	   (spark.fltk::resize-window x y w h))
	 
	 ;; Displays the window. args should be a list of strings
	 ;; which will be passed as arguments to the low-level function.
	 ;; If it is null current command line arguments are passed.
	 (define (window-show self . args)
	   (let ((argv null))
	     (if (eqv? args null)
		 (set! argv (list "spark"))
		 (begin
		   (set! argv (car args))
		   (assert-list argv)))
	     (spark.fltk::show-window (widget-handle self) argv)))

	 (define (window-hide self)
	   (spark.fltk::hide-window (widget-handle self)))

	 ;; returns true if the window is displayed.
	 (define (window-shown? self)
	   (spark.fltk::shown (widget-handle self)))

	 ;; Set the allowable range the user can resize this window to. 
	 ;; This only works for top-level windows.
	 (define (window-size-range self minw minh . args)
	   (if (<= minw 0)
	       (raise-exception "size-range" "minw must be greater than 0."))
	   (if (<= minh 0)
	       (raise-exception "size-range" "minh must be greater than 0."))
	   (let ((maxw 0) (maxh 0) (dw 0) (dh 0) (aspect #f))
	     (if (not (eqv? args null))
		 (begin
		   (let ((len (length args)) (rest null))
		     (if (>= len 1)
			 (begin
			   (set! maxw (car args))
			   (set! rest (cdr args))))
		     (if (>= len 2)
			 (begin
			   (set! maxh (car rest))
			   (set! rest (cdr rest))))
		     (if (>= len 3)
			 (begin
			   ;; dw and dh are size increments.
			   (set! dw (car rest))
			   (set! rest (cdr rest))))
		     (if (>= len 4)
			 (begin
			   (set! dh (car rest))
			   (set! rest (cdr rest))))
		     (if (>= len 5)
			 (begin
			   ;; aspect is a flag that indicates that the 
			   ;; window should preserve it's aspect ratio. 
			   ;; This only works if both the maximum and minimum 
			   ;; have the same aspect ratio. (ignored on WIN32 
			   ;; and by many X window managers)
			   (set! aspect (car rest)))))))
	     (spark.fltk::size-range (widget-handle self)
				     minw minh maxw maxh dw dh)))

	 (define (window-xclass! self s)
	   (spark.fltk::xclass (widget-handle self) s))

	 (define (window-xclass self)
	   (spark.fltk::xclass (widget-handle self))))

