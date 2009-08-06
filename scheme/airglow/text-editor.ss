;; Text-Editor widget.
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

(library airglow-text-editor

	 (import (airglow-util) (exception)  (asserts)
		 (util) (airglow-text-buffer)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export text-editor text-editor-buffer
		 text-editor-buffer! text-editor-cursor-color
		 text-editor-cursor-color! text-editor-cursor-style!
		 text-editor-hide-cursor text-editor-create-style-buffer 
		 text-editor-highlight-data text-editor-in-selection?
		 text-editor-insert-text text-editor-insert-position 
		 text-editor-insert-position! text-editor-move-down
		 text-editor-move-up text-editor-move-right
		 text-editor-move-left text-editor-move-right-by-word 
		 text-editor-replace text-editor-position-style
		 text-editor-move-left-by-word text-editor-redisplay-range 
		 text-editor-scrollbar-align text-editor-scrollbar-align!
		 text-editor-scrollbar-width text-editor-scrollbar-width! 
		 text-editor-show-cursor text-editor-show-insert-position 
		 text-editor-text-color text-editor-text-color! 
		 text-editor-text-font text-editor-text-font! 
		 text-editor-text-size text-editor-text-size! 
		 text-editor-word-end text-editor-word-start 
		 text-editor-wrap-mode
		 ;; text-editor
		 text-editor-add-key-binding! text-editor-remove-key-binding! 
		 text-editor-key-event)

	 ;; Creates and initializes a Text-Editor object.
	 ;; Accepts 8 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. type: 'normal, 'editor
	 ;; Returns the new text-editor object on success.
	 (define (text-editor . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 0)
		 (h 0) 
		 (title "")
		 (type 'normal)
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
		       (else 
			(raise-exception "text-editor" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (if (eqv? type 'editor)
		 (set! handle (spark.fltk::fl-text-editor x y w h title))
		 (set! handle (spark.fltk::fl-text-editor x y w h title)))
	     
	     (if (eqv? handle null)
		 (raise-exception "text-editor"
				  "Null handle to text-editor."
				  null))

	     (set! self (new-widget handle 'text-editor type))
	     self))

	 (define (text-editor-buffer self)
	   (widget-data self))

	 (define (text-editor-buffer! self b)
	   (if (spark.fltk::text-display-buffer (widget-handle self)
						(text-buffer-handle b))
	       (begin
		 (set-widget-data! self b)
		 #t)
	       null))

	 (define (text-editor-cursor-color self)
	   (integer->color (spark.fltk::cursor-color 
			    (widget-handle self))))

	 (define (text-editor-cursor-color! self c)
	   (spark.fltk::cursor-color (widget-handle self)
				     (color->integer c)))

	 (define (text-editor-cursor-style! self s)
	   (spark.fltk::cursor-style (widget-handle self)
				     (cursorstyle->integer s)))

	 (define (text-editor-hide-cursor self)
	   (spark.fltk::hide-cursor (widget-handle self)))

	 (define (text-editor-show-cursor self)
	   (spark.fltk::show-cursor (widget-handle self)))

	 (define (text-editor-create-style-buffer self . args)
	   (let ((style-buf (text-buffer (text-buffer-size 
					  (text-editor-buffer self)))))
	     (if (not (eqv? style-buf null))
		 (begin
		   (if (eqv? args null)
		       (text-buffer-fill style-buf #\A)
		       (text-buffer-fill style-buf (car args)))))
	     style-buf))

	 (define (text-editor-highlight-data self 
					     style-buffer style-table
					     style-unfinished-cb
					     cb-arg)
	   (if (not (eqv? style-unfinished-cb null))
	       (begin
		 (let ((num-args 0))
		   (assert-procedure style-unfinished-cb)
		   (set! num-args (procedure-arity style-unfinished-cb))
		   (if (not (= num-args 2))
		       (raise-exception "highlight-data" 
					"callback should take exactly two arguments." 
					null)))))
	   (let ((c-style-table (create-c-style-table style-table)))
	     (spark.fltk::highlight-data (widget-handle self)
					 (text-buffer-handle style-buffer)
					 c-style-table
					 style-unfinished-cb
					 cb-arg)))	   

	 (define (text-editor-in-selection? self)
	   (spark.fltk::in-selection (widget-handle self)))

	 (define (text-editor-insert-text self text)
	   (spark.fltk::insert (widget-handle self) text))

	 (define (text-editor-replace self text)
	   (spark.fltk::overstrike (widget-handle self) text))

	 (define (text-editor-insert-position self)
	   (spark.fltk::insert-position (widget-handle self)))

	 (define (text-editor-insert-position! self p)
	   (spark.fltk::insert-position (widget-handle self) p))

	 (define (text-editor-move-up self)
	   (spark.fltk::move-up (widget-handle self)))

	 (define (text-editor-move-down self)
	   (spark.fltk::move-down (widget-handle self)))

	 (define (text-editor-move-left self)
	   (spark.fltk::move-left (widget-handle self)))

	 (define (text-editor-move-right self)
	   (spark.fltk::move-right (widget-handle self)))

	 (define (text-editor-move-right-by-word self)
	   (spark.fltk::next-word (widget-handle self)))

	 (define (text-editor-move-left-by-word self)
	   (spark.fltk::previous-word (widget-handle self)))

	 (define (text-editor-position-style self line-start-pos line-len
					     line-index disp-index)
	   (spark.fltk::position-style (widget-handle self)
				       line-start-pos line-len
				       line-index disp-index))

	 (define (text-editor-redisplay-range self start end)
	   (spark.fltk::redisplay-range (widget-handle self)
					start end))

	 (define (text-editor-scrollbar-align self)
	   (integer->align (spark.fltk::scrollbar-align 
			    (widget-handle self))))

	 (define (text-editor-scrollbar-align! self a)
	   (spark.fltk::scrollbar-align 
	    (widget-handle self) (align->integer a)))

	 (define (text-editor-scrollbar-width self)
	   (spark.fltk::scrollbar-width
	    (widget-handle self)))
	 
	 (define (text-editor-scrollbar-width! self w)
	   (spark.fltk::scrollbar-width (widget-handle self) w))

	 (define (text-editor-show-insert-position self)
	   (spark.fltk::show-insert-position (widget-handle self)))

	 (define (text-editor-text-color self)
	   (integer->color (spark.fltk::text-display-text-color 
			    (widget-handle self))))

	 (define (text-editor-text-color! self c)
	   (spark.fltk::text-display-text-color (widget-handle self)
						(color->integer c)))

	 (define (text-editor-text-font self)
	   (integer->font (spark.fltk::text-display-text-font
			   (widget-handle self))))

	 (define (text-editor-text-font! self c)
	   (spark.fltk::text-display-text-font (widget-handle self)
					       (font->integer c)))

	 (define (text-editor-text-size self)
	   (spark.fltk::text-display-text-size (widget-handle self)))
	 
	 (define (text-editor-text-size! self c)
	   (if (eqv? c 'normal)
	       (set! c spark.fltk::FL-NORMAL-SIZE))
	   (spark.fltk::text-display-text-size (widget-handle self) c))

	 (define (text-editor-word-end self pos)
	   (spark.fltk::word-end (widget-handle self) pos))

	 (define (text-editor-word-start self pos)
	   (spark.fltk::word-start (widget-handle self) pos))

	 (define (text-editor-wrap-mode self switch pos)
	   (cond
	    ((eqv? switch 'on) (set! switch #t))
	    ((eqv? switch 'off) (set! switch #f)))
	   (spark.fltk::wrap-mode (widget-handle self) 
				  switch pos))

	 ;; text-editor

	 (define (text-editor-add-key-binding! self key state cb)
	   (set! state (state-list->integer-list state))
	   (assert-procedure cb)
	   (spark.fltk::add-key-binding (widget-handle self)
					(key->integer key)
					state cb))

	 (define (text-editor-remove-key-binding! self key state)
	   (set! state (state-list->integer-list state))
	   (spark.fltk::remove-key-binding (widget-handle self)
					   (key->integer key)))

	 (define (text-editor-key-event self key directive)
	   (spark.fltk::kf-key-event (widget-handle self)
				     (key->integer key)
				     directive))
	 
	 (define (state-list->integer-list sl)
	   (let ((ret (list))
		 (s null))
	     (let loop ()
	       (if (not (eqv? sl null))
		   (begin
		     (set! s (state->integer (car sl)))
		     (set! ret (append ret (list s)))
		     (set! sl (cdr sl))
		     (loop))))
	     ret))

	 (define (create-c-style-table style-table)
	   (let ((style (car style-table))
		 (ret (list)) (c 0) (f 0) (s 0))
	     (let loop ()
	       (if (not (eqv? style null))
		   (begin
		     (set! c (color->integer (car style)))
		     (set! style (cdr style))
		     (set! f (font->integer (car style)))
		     (set! style (cdr style))
		     (set! s (car style))
		     (if (eqv? s 'normal)
			 (set! s spark.fltk::FL-NORMAL-SIZE))
		     (set! ret (append ret (list (list c f s))))
		     (set! style-table (cdr style-table))
		     (if (= (length style-table) 0)
			 (set! style-table null))
		     (if (not (eqv? style-table null))
			 (set! style (car style-table))
			 (set! style null))
		     (loop))))
	     ret)))

