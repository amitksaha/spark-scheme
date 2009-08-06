;; Text-Buffer widget.
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

(library airglow-text-buffer
	 
	 (import (airglow-util) (exception) (asserts)
		 (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export text-buffer text-buffer-fill
		 text-buffer-add-modify-callback 
		 text-buffer-append-text text-buffer-append-file 
		 text-buffer-call-modify-callbacks 
		 text-buffer-character-at text-buffer-clear
		 text-buffer-copy-from text-buffer-count-displayed-characters
		 text-buffer-count-lines text-buffer-expand-character 
		 text-buffer-find text-buffer-highlight
		 text-buffer-highlight-position text-buffer-highlight-text 
		 text-buffer-insert-column text-buffer-insert-file 
		 text-buffer-insert text-buffer-size text-buffer-line-end
		 text-buffer-line-start text-buffer-line-text
		 text-buffer-load-from-file text-buffer-null-substitution-character
		 text-buffer-save-to-file text-buffer-replace 
		 text-buffer-remove-modify-callback text-buffer-delete
		 text-buffer-remove-secondary-selection 
		 text-buffer-remove-selection text-buffer-replace-secondary-selection 
		 text-buffer-replace-selection text-buffer-rewind-lines 
		 text-buffer-search text-buffer-secondary-selection-text
		 text-buffer-secondary-select text-buffer-secondary-unselect 
		 text-buffer-selected? text-buffer-selection-position 
		 text-buffer-selection-text text-buffer-select
		 text-buffer-skip text-buffer-tab-width text-buffer-tab-width! 
		 text-buffer-text text-buffer-text! text-buffer-unhighlight
		 text-buffer-unselect text-buffer-word-end 
		 text-buffer-word-start text-buffer-handle)

	 ;; Layout of a Text-Buffer object.
	 (define-struct text-buffer-s (handle)
	   (make-inspector))
	 
	 ;; Creates and initializes a Text-Buffer object.
	 ;; Accepts 1 optional arguments
	 ;; 1. initial-size
	 ;; Returns the new text-buffer object on success.
	 (define (text-buffer . args)
	   (let ((self null)
		 (handle null)
		 (s 0))
	     (if (not (eqv? args null))
		 (begin
		   (set! s (car args))
		   (assert-integer s)))
	     
	     (set! handle (spark.fltk::fl-text-buffer s))
	     
	     (if (eqv? handle null)
		 (raise-exception "text-buffer"
				  "Null handle to text-buffer."
				  null))

	     (set! self (make-text-buffer-s handle))
	     self))

	 (define (text-buffer-handle self)
	   (text-buffer-s-handle self))

	 (define (text-buffer-fill self c)
	   (spark.fltk::text-buffer-fill (text-buffer-handle self) c))

	 (define (text-buffer-add-modify-callback self cb arg)
	   (assert-procedure cb)
	   (if (not (eqv? (procedure-arity cb) 6))
	       (raise-exception "add-modify-callback"
				"Callback procedure should take 6 arguments." 
				null))
	   (spark.fltk::add-modify-callback (text-buffer-handle self) cb arg))

	 (define (text-buffer-append-text self text)
	   (spark.fltk::append (text-buffer-handle self) text))

	 (define (text-buffer-append-file self file)
	   (spark.fltk::append-file (text-buffer-handle self) file))

	 (define (text-buffer-call-modify-callbacks self)
	   (spark.fltk::call-modify-callbacks (text-buffer-handle self)))

	 (define (text-buffer-character-at self pos)
	   (spark.fltk::character (text-buffer-handle self) pos))

	 (define (text-buffer-clear self start end rect-start rect-end)
	   (spark.fltk::clear-rectangular (text-buffer-handle self) 
					  start end
					  rect-start rect-end))

	 (define (text-buffer-copy-from self from from-start from-end to-pos)
	   (spark.fltk::copy-buffer (text-buffer-handle self) 
				    (text-buffer-handle from) 
				    from-start from-end to-pos))

	 (define (text-buffer-count-displayed-characters self line-start-pos target-pos)
	   (spark.fltk::count-displayed-characters (text-buffer-handle self) 
						   line-start-pos
						   target-pos))

	 (define (text-buffer-count-lines self start end)
	   (spark.fltk::count-lines (text-buffer-handle self) 
				    start end))

	 (define (text-buffer-expand-character self pos indent)
	   (spark.fltk::expand-character (text-buffer-handle self) 
					 pos indent))

	 (define (text-buffer-find-character self c direction)
	   (case direction
	     ((back) (spark.fltk::findchar-backward (text-buffer-handle self)
						    c))
	     (else (spark.fltk::findchar-forward (text-buffer-handle self)
						 c))))

	 (define (text-buffer-find self text direction)
	   (if (string? text)
	       (begin
		 (case direction
		   ((back) (spark.fltk::findchars-backward 
			    (text-buffer-handle self)
			    text))
		   (else (spark.fltk::findchars-forward 
			  (text-buffer-handle self)
			  text))))
	       (text-buffer-find-character self text direction)))

	 (define (text-buffer-highlight self start end . args)
	   (if (eqv? args null)
	       (spark.fltk::highlight (text-buffer-handle self) 
				      start end)
	       (begin
		 (let ((rect-start 0)
		       (rect-end 0))
		   (set! rect-start (car args))
		   (set! args (cdr args))
		   (set! rect-end (car args))
		   (spark.fltk::highlight-rectangular (text-buffer-handle self)
						      start end
						      rect-start rect-end)))))

	 (define (text-buffer-highlight-position self)
	   (spark.fltk::highlight-position (text-buffer-handle self)))

	 (define (text-buffer-highlight-text self)
	   (spark.fltk::highlight-text (text-buffer-handle self)))

	 (define (text-buffer-insert-column self col start-pos text)
	   (spark.fltk::insert-column (text-buffer-handle self)
				      col start-pos text))

	 (define (text-buffer-insert-file self pos file)
	   (spark.fltk::insert-file (text-buffer-handle self)
				    file pos))
	 
	 (define (text-buffer-insert self pos text)
	   (spark.fltk::text-buffer-insert-text (text-buffer-handle self)
						pos text))

	 (define (text-buffer-size self)
	   (spark.fltk::text-buffer-length (text-buffer-handle self)))

	 (define (text-buffer-line-end self pos)
	   (spark.fltk::line-end (text-buffer-handle self) pos))

	 (define (text-buffer-line-start self pos)
	   (spark.fltk::line-start (text-buffer-handle self) pos))

	 (define (text-buffer-line-text self pos)
	   (spark.fltk::line-text (text-buffer-handle self) pos))

	 (define (text-buffer-load-from-file self file)
	   (spark.fltk::load-file (text-buffer-handle self) file))

	 (define (text-buffer-null-substitution-character self)
	   (spark.fltk::null-substitution-character (text-buffer-handle self)))

	 (define (text-buffer-save-to-file self file . args)
	   (if (eqv? args null)
	       (spark.fltk::save-file (text-buffer-handle self) file)
	       (begin
		 (let ((start 0)
		       (end 0))
		   (set! start (car args))
		   (set! args (cdr args))
		   (set! end (car args))
		   (spark.fltk::output-file (text-buffer-handle self) 
					    file start end)))))

	 (define (text-buffer-replace self start end text . args)
	   (if (eqv? args null)
	       (spark.fltk::replace-buffer-text (text-buffer-handle self)
						start end text)
	       (begin
		 (let ((rect-start 0)
		       (rect-end 0))
		   (set! rect-start (car args))
		   (set! args (cdr args))
		   (set! rect-end (car args))
		   (spark.fltk::replace-rectangular (text-buffer-handle self) 
						    start end
						    rect-start rect-end
						    text)))))

	 (define (text-buffer-delete self start end . args)
	   (if (eqv? args null)
	       (spark.fltk::remove-buffer-text (text-buffer-handle self) 
					       start end)
	       (begin
		 (let ((rect-start 0)
		       (rect-end 0))
		   (set! rect-start (car args))
		   (set! args (cdr args))
		   (set! rect-end (car args))
		   (spark.fltk::remove-rectangular (text-buffer-handle self) 
						   start end
						   rect-start rect-end)))))
	 
	 (define (text-buffer-remove-modify-callback self cb arg)
	   (assert-procedure cb)
	   (if (not (eqv? (procedure-arity cb) 6))
	       (raise-exception "add-modify-callback"
				"Callback procedure should take 6 arguments." 
				null))
	   (spark.fltk::remove-modify-callback (text-buffer-handle self) cb arg))

	 (define (text-buffer-remove-secondary-selection self)
	   (spark.fltk::remove-secondary-selection (text-buffer-handle self)))

	 (define (text-buffer-remove-selection self)
	   (spark.fltk::remove-selection (text-buffer-handle self)))

	 (define (text-buffer-replace-secondary-selection self text)
	   (spark.fltk::replace-secondary-selection (text-buffer-handle self)
						    text))

	 (define (text-buffer-replace-selection self text)
	   (spark.fltk::replace-selection (text-buffer-handle self) text))

	 (define (text-buffer-rewind-lines self start-pos n-lines)
	   (spark.fltk::rewind-lines (text-buffer-handle self)
				     start-pos n-lines))

	 (define (text-buffer-search self text start-pos . args)
	   (let ((direction 'forward)
		 (match-case #f))
	     (if (not (eqv? args null))
		 (begin
		   (set! direction (car args))
		   (set! args (cdr args))))
	     (if (not (eqv? args null))
		 (set! match-case (car args)))
	     (case direction
	       ((back) (spark.fltk::search-backward (text-buffer-handle self)
						    start-pos text match-case))
	       (else (spark.fltk::search-forward (text-buffer-handle self)
						 start-pos text match-case)))))

	 (define (text-buffer-secondary-selection-text self)
	   (spark.fltk::secondary-selection-text (text-buffer-handle self)))

	 (define (text-buffer-secondary-select self start end . args)
	   (if (eqv? args null)
	       (spark.fltk::secondary-select (text-buffer-handle self)
					     start end)
	       (begin
		 (let ((rect-start 0)
		       (rect-end 0))
		   (set! rect-start (car args))
		   (set! args (cdr args))
		   (set! rect-end (car args))
		   (spark.fltk::secondary-select-rectangular 
		    (text-buffer-handle self)
		    start end rect-start rect-end)))))

	 (define (text-buffer-secondary-unselect self)
	   (spark.fltk::secondary-unselect (text-buffer-handle self)))
	 
	 (define (text-buffer-selected? self)
	   (spark.fltk::selected (text-buffer-handle self)))

	 (define (text-buffer-selection-position self . args)
	   (if (eqv? args null)
	       (spark.fltk::selection-position (text-buffer-handle self) #f)
	       (spark.fltk::selection-position (text-buffer-handle self) #t)))

	 (define (text-buffer-selection-text self)
	   (spark.fltk::selection-text (text-buffer-handle self)))

	 (define (text-buffer-select self start end . args)
	   (if (eqv? args null)
	       (spark.fltk::text-buffer-select (text-buffer-handle self)
					       start end)
	       (begin
		 (let ((rect-start 0)
		       (rect-end 0))
		   (set! rect-start (car args))
		   (set! args (cdr args))
		   (set! rect-end (car args))
		   (spark.fltk::select-rectangular
		    (text-buffer-handle self)
		    start end rect-start rect-end)))))

	 (define (text-buffer-skip self type start num)
	   (case type
	     ((chars) (spark.fltk::skip-displayed-characters 
		       (text-buffer-handle self) start num))
	     (else (spark.fltk::skip-lines
		    (text-buffer-handle self) start num))))

	 (define (text-buffer-tab-width self)
	   (spark.fltk::tab-distance (text-buffer-handle self)))

	 (define (text-buffer-tab-width! self w)
	   (spark.fltk::tab-distance (text-buffer-handle self) w))

	 (define (text-buffer-text self . args)
	   (if (eqv? args null)
	       (spark.fltk::text-buffer-text (text-buffer-handle self))
	       (begin
		 (let ((len (length args))
		       (start 0)
		       (end 0)
		       (rect-start 0)
		       (rect-end 0))
		   (set! start (car args))
		   (set! args (cdr args))
		   (set! end (car args))
		   (if (= len 2)
		       (spark.fltk::text-range (text-buffer-handle self)
					       start end)
		       (begin
			 (set! args (cdr args))
			 (set! rect-start (car args))
			 (set! args (cdr args))
			 (set! rect-end (car args))
			 (spark.fltk::text-in-rectangle (text-buffer-handle self)
							start end
							rect-start rect-end)))))))

	 (define (text-buffer-text! self t)
	   (spark.fltk::text-buffer-text (text-buffer-handle self) t))

	 (define (text-buffer-unhighlight self)
	   (spark.fltk::unhighlight (text-buffer-handle self)))

	 (define (text-buffer-unselect self)
	   (spark.fltk::unselect (text-buffer-handle self)))

	 (define (text-buffer-word-end self pos)
	   (spark.fltk::text-buffer-word-end (text-buffer-handle self) pos))

	 (define (text-buffer-word-start self pos)
	   (spark.fltk::text-buffer-word-start (text-buffer-handle self) pos)))


