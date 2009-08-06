;; A widget that can be customized from scheme.
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

(library airglow-widget

	 (import (exception) (asserts) (airglow-util)
		 (airglow-image)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export widget-show widget-enable!
		 widget-enabled? widget-label-align
		 widget-label-align! widget-callback-argument
		 widget-callback-argument! widget-border
		 widget-border! widget-callback
		 widget-callback! widget-changed?
		 widget-changed! widget-disable-navigation
		 widget-bg-color widget-bg-color! 
		 widget-parent? widget-child? 
		 widget-label widget-label! 
		 widget-image widget-image!
		 widget-do-callback widget-height 
		 widget-hide widget-label-color
		 widget-label-color! widget-label-size
		 widget-label-size! widget-label-font
		 widget-label-font! widget-label-type
		 widget-label-type! widget-parent
		 widget-position widget-redraw
		 widget-redraw-label widget-resize 
		 widget-selection-color widget-selection-color! 
		 widget-size widget-set-focus 
		 widget-tooltip widget-tooltip! 
		 widget-user-data widget-user-data!
		 widget-visible? widget-width
		 widget-x widget-y 
		 widget-callback-when! widget-callback-when? 
		 widget-handle-type widget-handle-type! 
		 widget-dispose widget-damage
		 widget-damage!
		 ;; custom-widget
		 widget widget-draw!
		 widget-events!)

	 ;; Widget functions

	 ;; Displays the widget on screen.
	 (define (widget-show self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::show widget)))
	 
	 ;; Turns on/off the enabled state of the widget. 
	 ;; If the optional argument is true, the widget is not grayed.
	 (define (widget-enable! self flag . args)	  
	   (let ((widget (widget-handle self)))
	     (if flag
		 (spark.fltk::activate widget)
		 (if (eqv? args null)
		     (spark.fltk::deactivate widget)
		     (begin
		       (if (car args)
			   (spark.fltk::output widget)
			   (spark.fltk::deactivate widget)))))))
	 
	 ;; Returns true if the widget is enabled.
	 ;; If the optional argument is given, returns true only
	 ;; if all the parents are also active.
	 (define (widget-enabled? self . args)
	   (let ((widget (widget-handle self)))
	     (if (eqv? args null)
		 (spark.fltk::active widget)
		 (spark.fltk::active-r widget))))

	 ;; Returns true if the widget is visible.
	 ;; If the optional argument is given, returns true only
	 ;; if all the parents are also visible.
	 (define (widget-visible? self . args)
	   (let ((widget (widget-handle self)))
	     (if (eqv? args null)
		 (spark.fltk::visible widget)
		 (spark.fltk::visible-r widget))))

	 ;; Gets/Sets the label alignment.
	 (define (widget-label-align self)	  
	   (let ((widget (widget-handle self)))
	     (align->list (spark.fltk::align widget))))

	 (define (widget-label-align! self a)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::align widget (list->align a))))

	 ;; Gets/Sets the callback argument.
	 (define (widget-callback-argument self)	  
	   (let ((widget (widget-handle self)))
	     (spark.fltk::argument widget)))

	 (define (widget-callback-argument! self ca)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::argument widget ca)))

	 ;; Gets/Sets the box-type.
	 (define (widget-border self)
	   (let ((widget (widget-handle self)))
	     (integer->boxtype (spark.fltk::box widget))))

	 (define (widget-border! self bt)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::box widget (boxtype->integer bt))))
	 
	 ;; Gets/Sets the widget callback.
	 (define (widget-callback self)	  
	   (let ((widget (widget-handle self)))
	     (spark.fltk::callback widget)))

	 (define (widget-callback! self cb . args)
	   (let ((num-args 0)
		 (widget (widget-handle self)))
	     (assert-procedure cb)
	     (set! num-args (procedure-arity cb))
	     (assert-integer num-args)
	     (if (not (= num-args 2))
		 (raise-exception "callback" 
				  "Procedure should take exactly two arguments."))
	     (spark.fltk::set-callback-widget! widget self)
	     (spark.fltk::callback widget cb)
	     (if (not (eqv? args null))
		 (widget-callback-argument! self (car args)))
	     #t))

	 ;; Returns true if the widget's value has changed.
	 (define (widget-changed? self)	  
	   (let ((widget (widget-handle self)))
	     (spark.fltk::changed widget)))

	 ;; Sets/Clears the changed flag.
	 (define (widget-changed! self flag)	  
	   (let ((widget (widget-handle self)))
	     (if flag
		 (spark.fltk::set-changed widget)
		 (spark.fltk::clear-changed widget))))

	 ;; Disables keyboard navigation within this widget.
	 (define (widget-disable-navigation self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::clear-visible-focus)))

	 ;; Gets/Sets the background color.
	 (define (widget-bg-color self)	  
	   (let ((widget (widget-handle self)))
	     (integer->color (spark.fltk::color widget))))

	 (define (widget-bg-color! self bgc)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::color widget (color->integer bgc))))

	 ;; Returns true if the widget contains the child-widget
	 ;; or if the child is same as the self.
	 (define (widget-parent? self child)
	   (let ((widget (widget-handle self))
		 (child-widget (widget-handle child)))
	     (spark.fltk::contains widget child-widget)))

	 ;; Returns true if the widget is a child of or the same as parent.
	 (define (widget-child? self parent)
	   (let ((widget (widget-handle self))
		 (p-widget (widget-handle parent)))
	     (spark.fltk::inside widget p-widget)))

	 ;; Gets/Sets the label of the widget by copying the
	 ;; string to a new buffer.
	 (define (widget-label self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::label widget)))

	 (define (widget-label! self l)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::copy-label widget l)))

	 ;; Gets/Sets the label image for the active or inactive states.
	 (define (widget-image self state)
	   (let ((widget (widget-handle self)))
	     (if (eqv? state 'active)
		 (spark.fltk::image widget)
		 (spark.fltk::deimage widget))))

	 (define (widget-image! self img . args)
	   (let ((widget (widget-handle self)) 
		 (image null) (state 'active))
	     (if (not (eqv? args null))
		 (set! state (car args)))
	     (set! image (image-handle img))
	     (if (eqv? state 'active)
		 (spark.fltk::image widget image)
		 (spark.fltk::deimage widget image))))

	 ;; Forces the widget to execute it's callback,
	 ;; with an optional argument.
	 (define (widget-do-callback self . args)
	   (let ((widget (widget-handle self)))
	     (if (eqv? args null)
		 (spark.fltk::do-callback widget)
		 (spark.fltk::do-callback widget (car args)))))

	 ;; Returns the height of the widget.
	 (define (widget-height self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::height widget)))

	 ;; Returns the width of the widget.
	 (define (widget-width self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::width widget)))

	 ;; Returns the x position of the widget.
	 (define (widget-x self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::x-pos widget)))

	 ;; Returns the y position of the widget.
	 (define (widget-y self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::y-pos widget)))

	 ;; Hides the widget.
	 (define (widget-hide self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::hide widget)))

	 ;; Gets/Sets the label color.
	 (define (widget-label-color self)
	   (let ((widget (widget-handle self)))
	     (integer->color (spark.fltk::label-color widget))))

	 (define (widget-label-color! self lc)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::label-color widget (color->integer lc))))

	 ;; Gets/Sets the label size.
	 (define (widget-label-size self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::label-size widget)))

	 (define (widget-label-size! self sz)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::label-size widget sz)))

	 ;; Gets/Sets the label font.
	 (define (widget-label-font self)
	   (let ((widget (widget-handle self)))
	     (integer->font (spark.fltk::label-font widget))))

	 (define (widget-label-font! self f)
	   (let ((widget (widget-handle self))
		 (f (make-font f)))
	     (spark.fltk::label-font widget f)))				     

	 ;; Gets/Sets the label type.
	 (define (widget-label-type self)
	   (let ((widget (widget-handle self)))
	     (integer->labeltype (spark.fltk::label-type widget))))

	 (define (widget-label-type! self lt)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::label-type widget (labeltype->integer lt))))

	 ;; Returns the parent of the widget or null, if the widget
	 ;; has no parent.
	 (define (widget-parent self)
	   (let ((widget (widget-handle self))
		 (p null))
	     (set! p (spark.fltk::parent widget))
	     (if (not (eqv? p null))
		 (new-widget p)
		 null)))

	 ;; Changes the position of the widget
	 (define (widget-position self x y)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::position widget x y)))

	 ;; Changes the size of the widget
	 (define (widget-size self w h)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::size widget w h)))

	 ;; Marks the widget as needing a redarw.
	 (define (widget-redraw self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::redraw widget)))

	 ;; Marks the widget as needing a label redarw.
	 (define (widget-redraw-label self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::redraw-label widget)))

	 ;; Changes the position and size of the widget.
	 (define (widget-resize self x y w h)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::resize widget x y w h)))

	 ;; Gets/Sets the selection color.
	 (define (widget-selection-color self)
	   (let ((widget (widget-handle self)))
	     (integer->color (spark.fltk::selection-color widget))))

	 (define (widget-selection-color! self sc)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::selection-color widget 
					  (color->integer sc))))

	 ;; Puts the focus in this widget.
	 (define (widget-set-focus self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::take-focus widget)))

	 ;; Gets/Sets the tooltip text.
	 (define (widget-tooltip self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::tooltip widget)))

	 (define (widget-tooltip! self t)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::tooltip widget t)))

	 ;; Gets/Sets the callback argment.
	 (define (widget-user-data self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::user-data widget)))

	 (define (widget-user-data! self ud)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::user-data widget ud)))

	 (define (widget-callback-when? self)
	   (let ((widget (widget-handle self))
		 (name (struct-name self))
		 (proc spark.fltk::when))
	     (if (eqv? name 'input-field)
		 (set! proc spark.fltk::input-when))
	     (integer->when (proc widget))))

	 ;; Sets when the callback is done.
	 ;; Pass the optional argument as 'remove to remove the callback 'when' bit.
	 (define (widget-callback-when! self w . args)
	   (if (list? w)
	       (widget-add-callbacks-when self w)
	       (begin
		 (let ((widget (widget-handle self))
		       (name (struct-name self))
		       (proc spark.fltk::when))
		   (if (eqv? name 'input-field)
		       (set! proc spark.fltk::input-when))
		   (if (eqv? args null)
		       (proc widget (when->integer w))
		       (begin
			 (case (car args) 
			   ((remove) (proc widget (when->integer w) #t))
			   ((add) (proc widget (when->integer w) #f)))))))))

	 (define (widget-add-callbacks-when self w-list)
	   (let loop ()
	     (if (not (eqv? w-list null))
		 (begin
		   (widget-callback-when! self (car w-list) 'add)
		   (set! w-list (cdr w-list))
		   (loop))))
	   #t)

	 (define (widget-handle-type self)
	   (spark.fltk::type (widget-handle self)))
	 
	 (define (widget-handle-type! self t)
	   (spark.fltk::type (widget-handle self) t))

	 (define (widget-damage self)
	   (spark.fltk::damage (widget-handle self)))

	 (define (widget-damage! self c . args)
	   (if (eqv? args null)
	       (spark.fltk::damage (widget-handle self) c)
	       (begin
		 (let ((x (car args)) (y 0)
		       (w 0) (h 0))
		   (set! args (cdr args))
		   (set! y (car args))
		   (set! args (cdr args))
		   (set! w (car args))
		   (set! args (cdr args))
		   (set! h (car args))
		   (spark.fltk::damage (widget-handle self) c
				       x y w h)))))

	 (define (widget-dispose self)
	   (let ((widget (widget-handle self)))
	     (spark.fltk::dispose-widget widget)))

	 ;; Creates and initializes a Widget object.
	 ;; Accepts 8 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. super, name of the super type.
	 ;; 7. draw callback
	 ;; 8. Event handler
	 ;; Returns the new widget object on success.
	 (define (widget . args)
	   (let ((self null)
		 (handle null)
		 (draw-cb null)
		 (draw-cb-arg null)		
		 (handle-events-cb null)
		 (handle-events-cb-arg null)
		 (x 0) (y 0)
		 (w 0) (h 0)
		 (title "") (super "window")
		 (tmp null))
	     (let loop ()
	       (if (not (eqv? args null))
		   (begin
		     (set! tmp (car args))
		     (set! args (cdr args))
		     (if (eqv? args null)
			 (raise-exception "widget"
					  "Invalid keyword argument for (widget)"
					  'contract))
		     (case tmp
		       ((x) (set! x (car args)))
		       ((y) (set! y (car args)))
		       ((w) (set! w (car args)))
		       ((h) (set! h (car args)))
		       ((title) (set! title (car args)))
		       ((super) (set! super (car args)))
		       ((draw) (set! draw-cb (car args)))
		       ((draw-arg) (set! draw-cb-arg (car args)))
		       ((events) (set! handle-events-cb (car args)))
		       ((events-arg) (set! handle-events-cb-arg (car args)))
		       (else (raise-exception "widget"
					      "Invalid keyword for (widget)"
					      'contract)))
		     (set! args (cdr args))
		     (loop))))
	     (set! handle (spark.fltk::new-widget x y w h title 
						  super
						  draw-cb 
						  draw-cb-arg
						  handle-events-cb
						  handle-events-cb-arg))
	     (if (eqv? handle null)
		 (raise-exception "widget"
				  "Null handle to widget."
				  null))
	     (set! self (new-widget handle))
	     (spark.fltk::set-callback-widget! handle self)
	     self))

	 (define (widget-draw! self cb . user-arg)
	   (if (null? user-arg)
	       (spark.fltk::draw-callback (widget-handle self) cb)
	       (spark.fltk::draw-callback (widget-handle self) cb user-arg)))

	 (define (widget-events! self cb user-arg)
	   (if (null? user-arg)
	       (spark.fltk::handle-event-callback (widget-handle self) cb)
	       (spark.fltk::handle-event-callback (widget-handle self) cb user-arg))))

