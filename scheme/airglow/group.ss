;; Group functions.
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

(library airglow-group

	 (import (airglow-util) (exception) (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export group group-add group-index-of group-at
		 group-start group-finish group-destroy-all
		 group-resizable group-remove group-insert-at
		 group-dispose)

	 ;; Creates and initializes a Group object.
	 ;; Accepts 5 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; Returns the new button object on success.
	 (define (group . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 0)
		 (h 0) 
		 (title null)
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
		       (else (raise-exception "group" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (set! handle (spark.fltk::fl-group x y w h title))
	     
	     (if (eqv? handle null)
		 (raise-exception "group"
				  "Null handle to group."
				  null))

	     (set! self (new-widget handle 'group))
	     self))

	 ;; Adds the widget to the Group.
	 ;; If the optional argument is true, 
	 ;; makes the widget the resizable widget.
	 (define (group-add self widget . args)
	   (let ((f spark.fltk::add))
	     (if (not (eqv? args null))
		 (begin 
		   (if (car args)
		       (set! f spark.fltk::add-resizable))))
	     (f (widget-handle self) (widget-handle widget))))

	 (define (group-resizable self . args)
	   (if (eqv? args null)
	       (spark.fltk::resizable (widget-handle self))
	       (spark.fltk::resizable (widget-handle self) 
				      (widget-handle (car args)))))

	 ;; (begin) sets the current group so you can build the 
	 ;; widget tree by just constructing the widgets.
	 (define (group-start self)
	   (spark.fltk::begin (widget-handle self)))

	 ;; Widgets are added to the parent of the group.
	 (define (group-finish self)
	   (spark.fltk::end (widget-handle self)))

	 ;; Returns the child widget at the given index.
	 (define (group-at self index)
	   (spark.fltk::child-at (widget-handle self) index))

	 (define (group-index-of self widget)
	   (spark.fltk::index-of-child (widget-handle self)
				       (widget-handle widget)))

	 ;; Removes and releases the memory held by the child widgets.
	 (define (group-destroy-all self)
	   (spark.fltk::delete-children (widget-handle self)))

	 (define (group-remove self widget)
	   (spark.fltk::remove-child (widget-handle self)
				     (widget-handle widget)))

	 (define (group-insert-at self widget index)
	   (spark.fltk::insert-at (widget-handle self)
				  (widget-handle widget)
				  index))

	 ;; Releases the memory held by the Group Widget 
	 ;; and all it's child widgets.
	 (define (group-dispose self)
	   (if (spark.fltk::group-dispose (widget-handle self))
	       (set-widget-handle! self null))))



