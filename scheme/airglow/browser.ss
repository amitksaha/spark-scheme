;; Browser functions.
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

(library airglow-browser

	 (import (airglow-util) (exception) (asserts)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export browser browser-scrollbar 
		 browser-hposition browser-hposition! 
		 browser-vscroll-position browser-vscroll-position!
		 browser-move-scrollbar browser-scrollbar-width
		 browser-scrollbar-width! browser-text-color
		 browser-text-color! browser-text-font
		 browser-text-font! browser-text-size
		 browser-text-size! browser-add
		 browser-scroll browser-column-separator-char
		 browser-column-separator-char! browser-column-widths
		 browser-column-widths! browser-hide-line
		 browser-show-line browser-load-file
		 browser-type! browser-data browser-data! browser-move
		 browser-remove browser-count browser-swap 
		 browser-text browser-text! 
		 browser-line-visible? browser-make-line-visible
		 browser-deselect-all browser-selected
		 browser-selected? browser-icon-size
		 browser-icon-size! browser-filter
		 browser-filter! browser-file-type
		 browser-file-type! browser-load-directory
		 browser-check-all browser-checked? 
		 browser-checked! browser-value
		 browser-count-checked)

	 ;; Creates and initializes a Browser widget.
	 ;; Accepts 6 optional arguments
	 ;; 1. x position
	 ;; 2. y position
	 ;; 3. width
	 ;; 4. height
	 ;; 5. title
	 ;; 6. class, which should be any one of:
	 ;; 'file, 'hold, 'multi, 'select, 'check
	 ;; Returns the new browser object on success.
	 (define (browser . args)
	   (let ((self null)
		 (handle null)
		 (x 0)
		 (y 0)
		 (w 1)
		 (h 1) 
		 (title "")
		 (tp null)
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
		       ((type) (set! tp (car args)))
		       (else (raise-exception "browser" "Invalid keyword" 'contract)))
		     (set! args (cdr args))
		     (loop))))

	     (if (eqv? tp null)
		 (set! handle (spark.fltk::fl-browser x y w h title))
		 (begin
		   (if (eqv? tp 'check)
		       (set! handle (spark.fltk::fl-check-browser x y w h 
								  title tp))
		       (set! handle (spark.fltk::fl-browser x y w h 
							    title tp)))))

	     (if (eqv? handle null)
		 (raise-exception "browser"
				  "Null handle to browser."
				  null))

	     (set! self (new-widget handle 'browser tp))
	     self))

	 ;; Sets the scrollbar style.
	 (define (browser-scrollbar self scroll-bar)
	   (spark.fltk::has-scrollbar (widget-handle self)
				      (scrollbar->integer scroll-bar)))

	 ;; Gets or sets the horizontal scrolling position of the list, 
	 ;; which is the pixel offset of the list items within the list area.
	 (define (browser-hposition self)
	   (spark.fltk::hposition (widget-handle self)))

	 (define (browser-hposition! self hp)
	   (spark.fltk::hposition (widget-handle self) hp))

	 ;; Gets or sets the vertical scrolling position of the list, 
	 ;; which is the pixel offset of the list items within the list area.
	 (define (browser-vscroll-position self)
	   (spark.fltk::vscroll-position (widget-handle self)))

	 (define (browser-vscroll-position! self vsp)
	   (spark.fltk::vscroll-position (widget-handle self) vsp))

	 ;; Moves the vertical scroll bar to left or right.
	 (define (browser-move-scrollbar self direction)
	   (case direction
	     ((left) (spark.fltk::scrollbar-left (widget-handle self)))
	     ((right) (spark.fltk::scrollbar-right (widget-handle self)))))

	 ;; Gets/Sets the width of the scrollbar.
	 (define (browser-scrollbar-width self)
	   (spark.fltk::scrollbar-width (widget-handle self)))

	 (define (browser-scrollbar-width! self sbw)
	   (spark.fltk::scrollbar-width (widget-handle self) sbw))

	 ;; Gets/Sets the text color.
	 (define (browser-text-color self)
	   (integer->color (spark.fltk::text-color (widget-handle self))))

	 (define (browser-text-color! self tc)
	   (spark.fltk::text-color (widget-handle self) 
				   (color->integer tc)))

	 ;; Gets/Sets the text font.
	 (define (browser-text-font self)
	   (integer->font (spark.fltk::text-font (widget-handle self))))

	 (define (browser-text-font! self tf)
	   (spark.fltk::text-font (widget-handle self) 
				  (font->integer tf)))

	 ;; Gets/Sets the text size.
	 (define (browser-text-size self)
	   (spark.fltk::text-size (widget-handle self)))
	 
	 (define (browser-text-size! self ts)
	   (spark.fltk::text-size (widget-handle self) 
				  ts))

	 ;; Deselects all items in the browser.
	 ;; If the optional argument is true, tries to call the
	 ;; widget callback.
	 (define (browser-deselect-all self . args)
	   (if (eqv? args null)
	       (spark.fltk::deselect (widget-handle self) #f)
	       (spark.fltk::deselect (widget-handle self) (car args))))

	 ;; Sets the selected state of item n.
	 (define (browser-selected self n flag)
	   (spark.fltk::select-item (widget-handle self) n flag))

	 ;; Returns true if the item n is selected.
	 (define (browser-selected? self n)
	   (spark.fltk::browser-selected (widget-handle self) n))
	 
	 ;; Adds a line of text to the browser
	 (define (browser-add self text . args)
	   (if (not (eqv? (widget-type self) 'check))
	       (spark.fltk::add-line (widget-handle self) text)
	       (browser-cb-add self text args)))

	 ;; Scrolls the browser so that the top, bottom or middle line is n,
	 (define (browser-scroll self n direction)
	   (case direction
	     ((top) (spark.fltk::topline (widget-handle self) n))
	     ((bottom) (spark.fltk::bottomline (widget-handle self) n))
	     ((middle) (spark.fltk::middleline (widget-handle self) n))))

	 ;; Removes all items from the browser.
	 (define (browser-clear self)
	   (if (not (eqv? (widget-type self) 'check))
	       (spark.fltk::clear (widget-handle self))
	       (browser-cb-clear self)))

	 ;; Gets/Sets the current column separator character. 
	 (define (browser-column-separator-char self)
	   (spark.fltk::column-char (widget-handle self)))

	 (define (browser-column-separator-char! self c)
	   (spark.fltk::column-char (widget-handle self) c))

	 ;; Gets/Sets the column widths array.
	 (define (browser-column-widths self)
	   (spark.fltk::column-widths (widget-handle self)))

	 (define (browser-column-widths! self cw)
	   (spark.fltk::column-widths (widget-handle self) cw))

	 ;; Gest/Sets the data for the given line.
	 (define (browser-data self line)
	   (spark.fltk::data (widget-handle self) line))

	 (define (browser-data! self line d)
	   (spark.fltk::data (widget-handle self) line d))

	 ;; Hides the line from the user.
	 (define (browser-hide-line self line)
	   (spark.fltk::hide-line (widget-handle self) line))

	 ;; Shows the line to the user.
	 (define (browser-show-line self line)
	   (spark.fltk::show-line (widget-handle self) line))

	 ;; Clears the browser and loads each line of the file to it.
	 (define (browser-load-file self file-name)
	   (spark.fltk::load (widget-handle self) file-name))

	 ;; Line 'from' is moved to 'to'
	 (define (browser-move self from to)
	   (spark.fltk::move (widget-handle self) from to))

	 ;; Swaps two lines.
	 (define (browser-swap self a b)
	   (spark.fltk::swap (widget-handle self) a b))

	 ;; Removes the line n
	 (define (browser-remove self n)
	   (if (not (eqv? (widget-type self) 'check))
	       (spark.fltk::remove (widget-handle self) n)
	       (browser-cb-remove self n)))

	 ;; Retuns the number of lines in the browser
	 (define (browser-count self)
	   (if (not (eqv? (widget-type self) 'check))
	       (spark.fltk::count (widget-handle self))
	       (browser-cb-count self)))

	 ;; Gets/Sets text for line n.
	 (define (browser-text self n)
	   (if (not (eqv? (widget-type self) 'check))
	       (spark.fltk::text (widget-handle self) n)
	       (browser-cb-text self n)))

	 (define (browser-text! self n t)
	   (if (not (eqv? (widget-type self) 'check))
	       (spark.fltk::text (widget-handle self) n t)))

	 ;; Returns true if line n is visible.
	 (define (browser-line-visible? self n)
	   (spark.fltk::line-visible (widget-handle self) n))

	 ;; Displays line n.
	 (define (browser-make-line-visible self n)
	   (spark.fltk::line-visible (widget-handle self) n))

	 ;; file browser

	 ;; Gets/Sets the size of icons. The default is 20 pixels.
	 (define (browser-icon-size self)
	   (spark.fltk::iconsize (widget-handle self)))

	 (define (browser-icon-size! self s)
	   (spark.fltk::iconsize (widget-handle self) s))

	 ;; Gets/Sets the file browser filter pattern
	 ;; The following syntax is used by pattern:
	 ;; o * matches any sequence of 0 or more characters.
	 ;; o ? matches any single character.
	 ;; o [set] matches any character in the set. Set can contain 
	 ;; any single characters, or a-z to represent a range. 
	 ;; To match ] or - they must be the first characters. 
	 ;; To match ^ or ! they must not be the first characters.
	 ;; o [^set] or [!set] matches any character not in the set.
	 ;; o {X|Y|Z} or {X,Y,Z} matches any one of the subexpressions literally.
	 ;; o \x quotes the character x so it has no special meaning.
	 ;; o x all other characters must be matched exactly.
	 (define (browser-filter self)
	   (spark.fltk::filter (widget-handle self)))
	 
	 (define (browser-filter! self f)
	   (spark.fltk::filter (widget-handle self) f))

	 ;; Sets or gets the file browser type, 'files or 'directories. 
	 ;; When set to 'files, both files and directories are shown. 
	 ;; Otherwise only directories are shown.
	 (define (browser-file-type self)
	   (integer->filetype (spark.fltk::file-type (widget-handle self))))

	 (define (browser-file-type! self ft)
	   (spark.fltk::file-type (widget-handle self) 
				  (filetype->integer ft)))

	 ;; Load a directory to the file browser.
	 ;; Optional argument can specify a sort method, 
	 ;; which should be any one of:
	 ;; 'alpha, 'case-alpha, 'case-numeric, 'numeric
	 ;; The default is numeric.
	 (define (browser-load-directory self dir . args)
	   (if (eqv? args null)
	       (spark.fltk::load-dir (widget-handle self) dir)
	       (spark.fltk::load-dir (widget-handle self) dir
				     (sorttype->integer (car args)))))

	 ;; Dynamically sets the type of a browser.
	 ;; Type should be any one of:
	 ;; 'normal, 'select, 'hold or 'multi
	 (define (browser-type! self t)
	   (spark.fltk::browser-type (widget-handle self)
				     (browser-type->integer t)))

	 ;; Check-Browser

	 ;; Add a line to the browser and optionally checks it.
	 (define (browser-cb-add self text . args)
	   (if (eqv? args null)
	       (spark.fltk::cb-add-line (widget-handle self)
					text)
	       (spark.fltk::cb-add-line (widget-handle self)
					text
					(car args))))

	 ;; Checks/Unchecks all entries in the browser.
	 (define (browser-check-all self flag)
	   (if (eqv? (widget-type self) 'check)
	       (begin
		 (if flag
		     (spark.fltk::cb-check-all (widget-handle self))
		     (spark.fltk::cb-check-none (widget-handle self))))))

	 ;; Returns true if ith item is checked.
	 (define (browser-checked? self i)
	   (if (eqv? (widget-type self) 'check)
	       (spark.fltk::cb-checked (widget-handle self) i)))

	 ;; Sets the checked state of ith item.
	 (define (browser-checked! self i flag)
	   (if (eqv? (widget-type self) 'check)
	       (begin
		 (let ((f 0))
		   (if flag
		       (set! f 1))
		   (spark.fltk::cb-checked (widget-handle self) i f)))))

	 ;; Returns the count of checked items.
	 (define (browser-count-checked self)
	   (if (eqv? (widget-type self) 'check)
	       (spark.fltk::cb-nchecked (widget-handle self))))

	 ;; Returns the selected index.
	 (define (browser-value self)
	   (if (eqv? (widget-type self) 'check)
	       (spark.fltk::cb-value (widget-handle self))))

	 ;; Returns the count of checked items.
	 (define (browser-cb-count self)
	   (spark.fltk::cb-nitems (widget-handle self)))

	 (define (browser-cb-clear self)
	   (spark.fltk::cb-clear (widget-handle self)))

	 (define (browser-cb-remove self i)
	   (spark.fltk::cb-remove (widget-handle self) i))

	 (define (browser-cb-text self i)
	   (spark.fltk::cb-text (widget-handle self) i))

	 (define (sorttype->integer s)
	   (case s
	     ((alpha) spark.fltk::FL-ALPHASORT)
	     ((case-alpha) spark.fltk::FL-CASEALPHASORT)
	     ((case-numeric) spark.fltk::FL-CASENUMERICSORT)
	     (else spark.fltk::FL-NUMERICSORT)))

	 (define (integer->filetype i)
	   (cond
	    ((= i spark.fltk::FL-FILES) 'files)
	    ((= i spark.fltk::FL-DIRECTORIES) 'directories)
	    (else 'files)))

	 (define (filetype->integer f)
	   (case f
	     ((directories) spark.fltk::FL-DIRECTORIES)
	     (else spark.fltk::FL-FILES)))

	 (define (scrollbar->integer s)
	   (case s
	     ((horzontal) spark.fltk::SCROLL-HORIZONTAL)
	     ((vertical) spark.fltk::SCROLL-VERTICAL)
	     ((horzontal-always) spark.fltk::SCROLL-HORIZONTAL-ALWAYS)
	     ((vertical-always) spark.fltk::SCROLL-VERTICAL-ALWAYS)
	     ((both-always) spark.fltk::SCROLL-BOTH-ALWAYS)
	     (else spark.fltk::NO-SCROLLS)))

	 (define (browser-type->integer t)
	   (case t
	     ((normal) spark.fltk::NORMAL-BROWSER)
	     ((select) spark.fltk::SELECT-BROWSER)
	     ((hold) spark.fltk::HOLD-BROWSER)
	     ((multi) spark.fltk::MULTI-BROWSER)
	     (else spark.fltk::NORMAL-BROWSER))))

