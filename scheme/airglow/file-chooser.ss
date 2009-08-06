;; File-Chooser functions.
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

(library airglow-file-chooser

	 (import (airglow-util) (exception) (asserts)
		 (util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (provide file-chooser file-chooser-bg-color
		  file-chooser-bg-color! file-chooser-count
		  file-chooser-directory file-chooser-directory!
		  file-chooser-filter file-chooser-filter! 
		  file-chooser-filter-value file-chooser-filter-value!
		  file-chooser-visible file-chooser-icon-size
		  file-chooser-icon-size! file-chooser-label
		  file-chooser-label! file-chooser-ok-label 
		  file-chooser-ok-label! file-chooser-preview
		  file-chooser-rescan file-chooser-text-color
		  file-chooser-text-color! file-chooser-text-font
		  file-chooser-text-font! file-chooser-text-size
		  file-chooser-text-size! file-chooser-value
		  file-chooser-value! file-chooser-visible?
		  file-chooser-type file-chooser-type! 
		  file-chooser-dispose
		  ;; globals
		  file-chooser-show file-relative 
		  file-absolute file-name file-ext
		  file-change-ext file-dir?
		  file-match?)

	 ;; Creates and initializes a File-Chooser widget.
	 ;; Accepts 4 optional arguments
	 ;; 1. Path.
	 ;; 2. Pattern.
	 ;; 3. Type. Should be any one of 'single, 'multi, 'create
	 ;; 'directorty.
	 ;; 4. Title.
	 ;; Returns the new file-chooser object on success.
	 (define (file-chooser . args)
	   (let ((self null)
		 (handle null)
		 (path "./")
		 (pattern "*.*")
		 (type 'single)
		 (title "Choose File")
		 (opt null))
	     (let loop ()
	       (if (not (eqv? args null))
		   (begin
		     (set! opt (car args))
		     (set! args (cdr args))
		     (case opt
		       ((path) (set! path (car args)))
		       ((pattern) (set! pattern (car args)))
		       ((title) (set! title (car args)))
		       ((type) (set! type (car args)))
		       (else (raise-exception "file-chooser" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))
	     
	     (set! handle (spark.fltk::fl-file-chooser path pattern
						       (type->integer type)
						       title))	    
	     (if (eqv? handle null)
		 (raise-exception "file-chooser"
				  "Null handle to file-chooser."
				  null))

	     (set! self (new-widget handle 'file-chooser type))
	     self))

	 (define (file-chooser-bg-color self)
	   (integer->color 
	    (spark.fltk::file-chooser-color (widget-handle self))))

	 (define (file-chooser-bg-color! self c)
	   (spark.fltk::file-chooser-color (widget-handle self)
					   (color->integer c)))

	 (define (file-chooser-count self)
	   (spark.fltk::file-chooser-count (widget-handle self)))

	 (define (file-chooser-directory self)
	   (spark.fltk::file-chooser-directory (widget-handle self)))

	 (define (file-chooser-directory! self d)
	   (spark.fltk::file-chooser-directory (widget-handle self) d))

	 (define (file-chooser-filter self)
	   (spark.fltk::file-chooser-filter (widget-handle self)))

	 (define (file-chooser-filter! self f)
	   (spark.fltk::file-chooser-filter (widget-handle self) f))

	 (define (file-chooser-filter-value self)
	   (spark.fltk::file-chooser-filter-value (widget-handle self)))

	 (define (file-chooser-filter-value! self f)
	   (spark.fltk::file-chooser-filter-value (widget-handle self) f))

	 (define (file-chooser-visible self flag)
	   (if flag
	       (spark.fltk::file-chooser-show (widget-handle self))
	       (spark.fltk::file-chooser-hide (widget-handle self))))

	 (define (file-chooser-icon-size self)
	   (spark.fltk::file-chooser-icon-size (widget-handle self)))

	 (define (file-chooser-icon-size! self i)
	   (spark.fltk::file-chooser-icon-size (widget-handle self) i))

	 (define (file-chooser-label self)
	   (spark.fltk::file-chooser-label (widget-handle self)))

	 (define (file-chooser-label! self l)
	   (spark.fltk::file-chooser-label (widget-handle self) l))

	 (define (file-chooser-ok-label self)
	   (spark.fltk::file-chooser-ok-label (widget-handle self)))

	 (define (file-chooser-ok-label! self l)
	   (spark.fltk::file-chooser-ok-label (widget-handle self) l))

	 (define (file-chooser-preview self)
	   (spark.fltk::file-chooser-preview (widget-handle self)))

	 (define (file-chooser-rescan self)
	   (spark.fltk::file-chooser-rescan (widget-handle self)))

	 (define (file-chooser-text-color self)
	   (integer->color 
	    (spark.fltk::file-chooser-text-color (widget-handle self))))

	 (define (file-chooser-text-color! self c)
	   (spark.fltk::file-chooser-text-color (widget-handle self) 
						(color->integer c)))

	 (define (file-chooser-text-font self)
	   (integer->font 
	    (spark.fltk::file-chooser-text-font (widget-handle self))))

	 (define (file-chooser-text-font! self f)
	   (spark.fltk::file-chooser-text-font (widget-handle self) 
					       (font->integer f)))

	 (define (file-chooser-text-size self)
	   (spark.fltk::file-chooser-text-size (widget-handle self)))

	 (define (file-chooser-text-size! self s)
	   (spark.fltk::file-chooser-text-size (widget-handle self) s))

	 (define (file-chooser-value self)
	   (spark.fltk::file-chooser-value (widget-handle self)))

	 (define (file-chooser-value! self v)
	   (spark.fltk::file-chooser-value (widget-handle self) v))

	 (define (file-chooser-type self)
	   (spark.fltk::file-chooser-type (widget-handle self)))

	 (define (file-chooser-type! self t)
	   (spark.fltk::file-chooser-type (widget-handle self) t))

	 (define (file-chooser-visible? self)
	   (spark.fltk::file-chooser-visible (widget-handle self)))

	 (define (file-chooser-dispose self)
	   (spark.fltk::file-chooser-dispose (widget-handle self)))

	 ;; globals

	 (define (file-chooser-show message pattern deffile . args)
	   (let ((relative #f)
		 (ok-lbl null)
		 (ret null))
	     (if (not (eqv? args null))
		 (begin
		   (set! relative (car args))
		   (set! args (cdr args))))
	     (if (not (eqv? args null))
		 (set! ok-lbl (car args)))
	     (if (not (eqv? ok-lbl null))
		 (spark.fltk::fl-file-chooser-ok-label ok-lbl))
	     (set! ret (spark.fltk::show-file-chooser message pattern
						      deffile relative))
	     (if (not (eqv? ok-lbl null))
		 (spark.fltk::fl-file-chooser-ok-label null))
	     ret))

	 (define (file-relative file-name)
	   (spark.fltk::filename-relative file-name))

	 (define (file-absolute file-name)
	   (spark.fltk::filename-absolute file-name))

	 (define (file-name file-name)
	   (spark.fltk::filename-name file-name))

	 (define (file-ext file-name)
	   (spark.fltk::filename-ext file-name))

	 (define (file-change-ext file-name from-ext)
	   (spark.fltk::filename-setext file-name from-ext))

	 (define (file-dir? file-name)
	   (spark.fltk::filename-isdir file-name))

	 (define (file-match? file-name pattern)
	   (spark.fltk::filename-match file-name pattern))

	 (define (type->integer t)
	   (case t
	     ((single) spark.fltk::FC-SINGLE)
	     ((multi) spark.fltk::FC-MULTI)
	     ((create) spark.fltk::FC-CREATE)
	     ((directory) spark.fltk::FC-DIRECTORY)
	     (else (raise-exception "type->integer"
				    "Not a valid file-chooser type."
				    null)))))


