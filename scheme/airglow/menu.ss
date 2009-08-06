;; Menu functions.
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

(library airglow-menu

	(import (airglow-util) (exception)  (asserts)
		(util)
		((prefix spark.fltk:: #%spark-fltk)))

	(export menu menu-add menu! menu-copy 
		 menu-find menu-callback! menu-remove 
		 menu-last-selected menu-label
		 menu-label! menu-label-type
		 menu-label-type! menu-label-font
		 menu-label-font! menu-label-size
		 menu-label-size! menu-label-color
		 menu-label-color! menu-shortcut! 
		 menu-value menu-select
		 menu-visible? menu-show 
		 menu-hide menu-active? 
		 menu-activate menu-changed!
		 menu-changed? menu-choice
		 menu-down-box menu-down-box!
		 menu-popup menu-pulldown)
		 
	;; Creates and initializes a Menu object.
	;; Accepts 7 optional arguments
	;; 1. x position
	;; 2. y position
	;; 3. width
	;; 4. height
	;; 5. title
	;; 6. type. Any one of 'bar, 'choice or 'button.
	;; Returns the new menu object on success.
	(define (menu . args)
	  (let ((self null)
		(handle null)
		(x 0)
		(y 0)
		(w 0)
		(h 0) 
		(title "")
		(type 'bar)
		(popup-type null)
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
		      (else (raise-exception "menu" "Invalid keyword" 'contract)))
		    (set! args (cdr args))
		    (loop))))
	    (if (menu-popup? type)
		(begin
		  (set! popup-type type)
		  (set! type 'button)))

	    (set! handle (spark.fltk::fl-menu x y w h title type))
	    
	    (if (eqv? handle null)
		(raise-exception "menu"
				 "Null handle to menu."
				 null))

	    (set! self (new-widget handle 'menu type))
	    (set-widget-data! self #f)

	    (if (not (eqv? popup-type null))
		(menu-popup! self popup-type))
	    self))

	;; Adds a menu. Takes label as the only requires argument.
	;; Optional arguments are:
	;; 1. Shortcut.
	;; 2. Callback function
	;; 3. Callback user argument.
	;; 4. Menu flags.
	(define (menu-add self label . args)
	  (let ((shortcut 0)
		(cb null) (ud null)
		(flags '(0)) 
		(key null) (val null))
	    (let loop ()
	      (if (not (eqv? args null))
		  (begin
		    (set! key (car args))
		    (set! args (cdr args))
		    (if (eqv? args null)
			(raise-exception "menu-add"
					 "Invalid keyword-value for argument"
					 'contract))
		    (set! val (car args))
		    (set! args (cdr args))
		    (case key
		      ((shortcut) 
		       (set! shortcut (list->shortcut 
				       (string->shortcutlist val) 0)))
		      ((callback) (set! cb val))
		      ((user-data) (set! ud val))
		      ((flags)
		       (if (symbol? val)
			   (set! flags (list (menuflag->integer val)))
			   (set! flags (list->flags val flags))))
		      (else (raise-exception "menu-add" 
					     "Invalid keyword argument"
					     'contract)))
		    (loop))))
	    (if (spark.fltk::add-menu-item (widget-handle self) 
					   label shortcut cb ud flags)
		(begin
		  (if (not (eqv? cb null))
		      (begin
			(let ((mi (menu-find self label)))
			  (if (not (eqv? mi null))
			      (begin
				(set! mi (widget-handle mi))
				(spark.fltk::set-menu-item-callback-widget! 
				 mi
				 self)
				#t))))))
		null)))

	;; Sets the menus in a menu bar.
	;; menu-vec is a vector that describes the menu-items.
	;; Sample menu-vec:
	;; (define menu-vec 
	;;              #(#("File" "Ctrl+F")
	;; 		#(#("New" "Ctrl+N")
	;; 		  #("Open" "Ctrl+O")
	;; 		  #("Save" "Ctrl+S")
	;; 		  #("Quit" "Ctrl+Q"))
	;; 		#("Edit") 
	;; 		#(#("Cut" "Ctrl+X")
	;; 		  #("Copy" "Ctrl+C")
	;; 		  #("Paste" "Ctrl+V"))
	;; 		#("Build") 
	;; 		#(#("Configuration")
	;; 		  #(#("Debug") 
	;; 		    #("Release")))
	;; 		#("Help") 
	;; 		#(#("Tutorial")
	;; 		  #("Reference")
	;; 		  #("About"))))
	(define (menu! self menu-vec)
	  (raise-exception "menu!" "Function is not portable. Use menu-add."
			   null)
	  (if (widget-data self)
	      (raise-exception "menu!"
			       "Cannot call menu! on popup. Use menu-add instead." 
			       null))
	  (let ((menus null))
	    (if (vector? menu-vec)
		(set! menus (vector->menus menu-vec))
		(set! menus (list->menus menu-vec)))
	    (spark.fltk::copy-menu-items (widget-handle self) menus)))

	(define (menu-copy self src-menu)
	  (spark.fltk::copy-menu-items-from (widget-handle self) 
					    (widget-handle src-menu)))

	(define (menu-find self menu-path)
	  (let ((h (spark.fltk::find-menu-item (widget-handle self) menu-path)))
	    (if (not (eqv? h null))
		(begin
		  (let ((ret (new-widget h 'menu)))
		    (set-widget-data! ret #f)
		    ret))
		null)))

	(define (menu-callback! self menu-path cb . args)
	  (let ((mi (widget-handle (menu-find self menu-path))))
	    (if (not (eqv? mi null))
		(begin
		  (let ((arg null))
		    (if (not (eqv? args null))
			(set! arg (car args)))
		    (if (spark.fltk::menu-item-callback mi cb arg)
			(spark.fltk::set-menu-item-callback-widget! mi self)
			#f))))))

	;; Sets the menu a popup menu.
	;; Valid button values are 'popup1, 'popup2,
	;; 'popup3, 'popup12, 'popup13, 'popup23.
	(define (menu-popup! self button)
	  (let ((t (popuptype->integer button)))
	    (if (spark.fltk::menu-button-type (widget-handle self) t)
		(set-widget-data! self #t))))

	(define (menu-remove self menu-item)
	  (spark.fltk::remove-menu-item (widget-handle self) menu-item))

	(define (menu-last-selected self)
	  (let ((ret 
		 (new-widget (spark.fltk::mvalue (widget-handle self)) 'menu)))
	    (set-widget-data! ret #f)
	    ret))

	;; menu-item functions

	;; Gets/Sets the label of the menu-item.
	(define (menu-label! self l)
	  (spark.fltk::menu-item-label (widget-handle self) l))

	(define (menu-label self)
	  (spark.fltk::menu-item-label (widget-handle self)))

	;; Gets/Sets the label type.
	(define (menu-label-type self . args)
	  (integer->labeltype (spark.fltk::menu-item-label-type 
			       (widget-handle self))))

	(define (menu-label-type! self lt)
	  (spark.fltk::menu-item-label-type (widget-handle self) 
					    (labeltype->integer
					     lt)))

	;; Gets/Sets the label size.
	(define (menu-label-size self)
	  (spark.fltk::menu-item-label-size (widget-handle self)))

	(define (menu-label-size! self ls)
	  (spark.fltk::menu-item-label-size (widget-handle self) ls))

	;; Gets/Sets the label font.
	(define (menu-label-font self)
	  (integer->font (spark.fltk::menu-item-label-font 
			  (widget-handle self))))

	(define (menu-label-font! self lf)
	  (spark.fltk::menu-item-label-font (widget-handle self) 
					    (font->integer lf)))

	;; Gets/Sets the label color.
	(define (menu-label-color self)
	  (integer->color (spark.fltk::menu-item-label-color 
			   (widget-handle self))))

	
	(define (menu-label-color! self lc)
	  (spark.fltk::menu-item-label-color (widget-handle self) 
					     (color->integer lc)))

	;; Sets the shortcut.
	(define (menu-shortcut! self s)
	  (spark.fltk::menu-item-shortcut (widget-handle self) 
					  ((list->shortcut 
					    (string->shortcutlist s) 0))))

	;; Returns the value of a check or radio menu item.
	(define (menu-value self)
	  (spark.fltk::menu-item-value (widget-handle self)))

	;; Checks the radio button. If the optional argument is true,
	;; other radios are turned off. 
	(define (menu-select self flag . args)
	  (if (eqv? args null)
	      (begin
		(if flag
		    (spark.fltk::menu-item-check (widget-handle self))
		    (spark.fltk::menu-item-uncheck (widget-handle self))))
	      (begin
		(if (and flag (car args))
		    (spark.fltk::menu-item-check-only (widget-handle self))
		    (spark.fltk::menu-item-uncheck (widget-handle self))))))

	(define (menu-visible? self)
	  (spark.fltk::menu-item-is-visible (widget-handle self)))

	(define (menu-show self)
	  (spark.fltk::menu-item-set-visible (widget-handle self) #t))

	(define (menu-hide self)
	  (spark.fltk::menu-item-set-visible (widget-handle self) #f))

	(define (menu-active? self)
	  (spark.fltk::menu-item-is-active (widget-handle self)))

	(define (menu-activate self flag)
	  (if flag
	      (spark.fltk::menu-item-activate (widget-handle self))
	      (spark.fltk::menu-item-deactivate (widget-handle self))))

	(define (menu-popup self . args)
	  (let ((x -1) (y -1)
		(title null)
		(picked 0))
	    (if (not (eqv? args null))
		(begin
		  (set! x (car args))
		  (set! args (cdr args))
		  (if (not (eqv? args null))
		      (begin
			(set! y (car args))
			(set! args (cdr args))))
		  (if (not (eqv? args null))
		      (begin
			(set! title (car args))
			(set! args (cdr args))))
		  (if (not (eqv? args null))
		      (begin
			(set! picked (car args))
			(set! args (cdr args))))))
	    (spark.fltk::popup (widget-handle self) x y title picked 0)))

	(define (menu-pulldown self w h . args)
	  (let ((x -1) (y -1)
		(title null)
		(picked 0))
	    (if (not (eqv? args null))
		(begin
		  (set! x (car args))
		  (set! args (cdr args))
		  (if (not (eqv? args null))
		      (begin
			(set! y (car args))
			(set! args (cdr args))))
		  (if (not (eqv? args null))
		      (begin
			(set! title (car args))
			(set! args (cdr args))))
		  (if (not (eqv? args null))
		      (begin
			(set! picked (car args))
			(set! args (cdr args))))))
	    (spark.fltk::pulldown (widget-handle self) 
				  x y w h 
				  picked 0
				  title 0)))		

	;; choice menu

	(define (menu-changed? self)
	  (spark.fltk::choice-changed (widget-handle self)))

	(define (menu-changed! self flag)
	  (if flag
	      (spark.fltk::choice-set-changed (widget-handle self))
	      (spark.fltk::choice-clear-changed (widget-handle self))))

	(define (menu-choice self . args)
	  (if (eqv? args null)
	      (spark.fltk::choice-value (widget-handle self))
	      (spark.fltk::choice-value (widget-handle self) (car args))))

	(define (menu-down-box self)
	  (integer->boxtype (spark.fltk::choice-down-box (widget-handle self))))

	(define (menu-down-box! self db)
	  (spark.fltk::choice-down-box (widget-handle self) 
				       (boxtype->integer db)))

	(define (vector->menus menu-vec)
	  (let ((ret (list))
		(menu-i null)
		(len (vector-length menu-vec))
		(index 0))
	    (let loop ()
	      (if (< index len)
		  (begin
		    (set! menu-i (vector-ref menu-vec index))
		    (if (vector? menu-i)
			(begin
			  (let ((f-item (vector-ref menu-i 0)))
			    (if (vector? f-item)
				(begin
				  (set! ret (append ret (vector->menus menu-i)))
				  (set! ret (append ret (list (term-menu-item)))))
				(set! ret (append ret (list (vector->menuitem menu-i))))))))
		    (set! index (+ index 1))
		    (loop))))
	    ret))

	(define (list->menus menu-vec)
	  (let ((ret (list))
		(menu-i (car menu-vec)))
	    (let loop ()
	      (if (not (eqv? menu-i null))
		  (begin
		    (if (list? menu-i)
			(begin
			  (let ((f-item (car menu-i)))
			    (if (list? f-item)
				(begin
				  (set! ret (append ret (list->menus menu-i)))
				  (set! ret (append ret (list (term-menu-item)))))
				(set! ret (append ret (list (list->menuitem menu-i))))))))
		    (set! menu-vec (cdr menu-vec))
		    (if (not (eqv? menu-vec null))
			(set! menu-i (car menu-vec))
			(set! menu-i null))
		    (loop))))
	    ret))

	(define (term-menu-item) null)

	;; Creates a fltk menu item pointer from a vector in the format:
	;; [label ((shortcut-keys) shortcut-char) (flags)]
	(define (vector->menuitem menu-vec)
	  (let ((label "") (shortcut 0)
		(cb null) (ud null) (tmp null) 
		(flags (list 0))
		(len (vector-length menu-vec))
		(indx 0))
	    (if (>= len 1)
		(set! label (vector-ref menu-vec 0)))
	    (if (>= len 2)
		(set! shortcut (list->shortcut 
				(string->shortcutlist 
				 (vector-ref menu-vec 1)) 0)))
	    (if (>= len 3)
		(begin
		  (let ((tmp (vector-ref menu-vec 2)))
		    (if (procedure? tmp)
			(set! cb tmp)
			(set! indx 2)))))
	    (if (>= len 4)
		(set! ud (vector-ref menu-vec 3)))
	    (if (>= len 5)
		(set! indx 4))
	    (if (> indx 0)
		(begin
		  (let* ((tmp (vector-ref menu-vec indx))
			 (beg null))
		    (if (not (eqv? tmp 'null))
			(begin
			  (set! beg (car tmp))
			  (if (or (eqv? beg 'list)
				  (eqv? beg 'quote))
			      (set! tmp (car (cdr tmp))))
			  (if (symbol? tmp)
			      (set! flags (list (menuflag->integer tmp)))
			      (set! flags (list->flags tmp flags))))))))
	    (spark.fltk::fl-menu-item label shortcut cb ud flags)))	    

	(define (list->menuitem menu-vec)
	  (let ((label "") (shortcut 0)
		(cb null) (ud null) (tmp null) 
		(flags (list 0)))
	    (if (not (eqv? menu-vec null))
		(begin
		  (set! label (car menu-vec))
		  (set! menu-vec (cdr menu-vec))))
	    (if (not (eqv? menu-vec null))
		(begin
		  (set! shortcut (list->shortcut
				  (string->shortcutlist
				   (car menu-vec)) 0))
		  (set! menu-vec (cdr menu-vec))))
	    (if (not (eqv? menu-vec null))
		(begin
		  (let ((tmp (car menu-vec)))
		    (if (procedure? tmp)
			(set! cb tmp)
			(begin
			  (if (symbol? tmp)
			      (set! flags (list (menuflag->integer tmp)))
			      (set! flags (list->flags tmp flags))))))
		  (set! menu-vec (cdr menu-vec))))
	    (if (not (eqv? menu-vec null))
		(begin
		  (set! ud (car menu-vec))
		  (set! menu-vec (cdr menu-vec))))
	    (if (not (eqv? menu-vec null))
		(begin
		  (let ((tmp (car menu-vec)))
		    (if (symbol? tmp)
			(set! flags (list (menuflag->integer tmp)))
			(set! flags (list->flags tmp flags))))))
	    (spark.fltk::fl-menu-item label shortcut cb ud flags)))

	(define (list->shortcut lst default)
	  (if (and (not (list? lst)) (not (eqv? lst null)))
	      default
	      (begin
		(if (not (eqv? lst null))
		    (begin
		      (let ((skeys (car lst)) (ci 0) 
			    (shortcut 0) (tmp null))
			(let loop ()
			  (set! shortcut (+ shortcut (key->integer (car skeys))))
			  (set! skeys (cdr skeys))
			  (if (not (eqv? skeys null))
			      (loop)))
			(set! tmp (cdr lst))
			(if (not (eqv? tmp null))
			    (begin
			      (set! tmp (car tmp))
			      (set! ci (char->integer tmp))))
			(set! shortcut (+ shortcut ci))
			shortcut))
		    default))))
	
	(define (list->flags lst default)
	  (if (and (not (list? lst)) (not (eqv? lst null)))
	      (raise-exception "list->flags"
			       "Menu flags should be a list or symbol."
			       null)
	      (begin
		(if (not (eqv? lst null))
		    (begin
		      (let ((flags (list 0)) (tmp lst))
			(let loop ()
			  (set! flags (append flags (list (menuflag->integer (car tmp)))))
			  (set! tmp (cdr tmp))
			  (if (not (eqv? tmp null))
			      (loop)))
			flags))
		    default))))

	(define (string->shortcutlist s)
	  (if (not (string? s))
	      null
	      (begin
		(let ((tokens (string-split s (list #\+)))
		      (skeys (list)) (c null)
		      (ret null))	
		  (if (not (eqv? tokens null))
		      (begin
			(let ((len (length tokens))
			      (count 0) (token (car tokens)))
			  (let loop ()
			    (if (= count (- len 1))
				(set! c (string-ref token 0))
				(set! skeys (append skeys (list 
							   (string->key token)))))
			    (set! count (+ count 1))
			    (set! tokens (cdr tokens))
			    (if (not (eqv? tokens null))
				(begin
				  (set! token (car tokens))
				  (loop)))))))
		  (set! ret (list skeys c))
		  ret))))

	(define (popuptype->integer p)
	  (case p
	    ((popup1) spark.fltk::FL-MENU-POPUP1)
	    ((popup2) spark.fltk::FL-MENU-POPUP2)
	    ((popup3) spark.fltk::FL-MENU-POPUP3)
	    ((popup12) spark.fltk::FL-MENU-POPUP12)
	    ((popup13) spark.fltk::FL-MENU-POPUP13)
	    ((popup23) spark.fltk::FL-MENU-POPUP23)
	    (else (raise-exception "popuptype->integer" 
				   "Not a supported popup type." null))))

	(define (menu-popup? p)
	  (if (or (eqv? p 'popup1)
		  (eqv? p 'popup2)
		  (eqv? p 'popup3)
		  (eqv? p 'popup12)
		  (eqv? p 'popup13)
		  (eqv? p 'popup23))
	      #t
	      #f)))

