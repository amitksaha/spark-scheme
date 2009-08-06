;; Utility functions.
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

(library airglow-util
	
	(import (exception)
		((prefix spark.fltk:: #%spark-fltk)))


	(define-struct widget (handle class type data) 
	  (make-inspector))
	
	(define (new-widget h . args)
	  (if (eqv? args null)
	      (make-widget h null null null)
	      (begin
		(let ((class (car args))
		      (type null))
		  (set! args (cdr args))
		  (if (not (eqv? args null))
		      (set! type (car args)))		  
		  (make-widget h class type null)))))

	(define (struct-name s)
	  (let ()
	    (define-values (struct-type skipped) (struct-info s))
	    (define-values (name ifk afk accessor mutator ik ss skipped?)
	      (struct-type-info struct-type))
	    name))
	
	(define (integer->color i)
	  (cond
	    ((= i spark.fltk::FL-FOREGROUND-COLOR) 'foreground)
	    ((= i spark.fltk::FL-BACKGROUND2-COLOR) 'background2)
	    ((= i spark.fltk::FL-INACTIVE-COLOR) 'inactive)
	    ((= i spark.fltk::FL-SELECTION-COLOR) 'selection)
	    ((= i spark.fltk::FL-GRAY0) 'gray)
	    ((= i spark.fltk::FL-DARK3) 'dark3)
	    ((= i spark.fltk::FL-DARK2) 'dark2)
	    ((= i spark.fltk::FL-DARK1) 'dark1)
	    ((= i spark.fltk::FL-BACKGROUND-COLOR) 'background)
	    ((= i spark.fltk::FL-LIGHT1) 'light1)
	    ((= i spark.fltk::FL-LIGHT2) 'light2)
	    ((= i spark.fltk::FL-LIGHT3) 'light3)
	    ((= i spark.fltk::FL-BLACK) 'black)
	    ((= i spark.fltk::FL-RED) 'red)
	    ((= i spark.fltk::FL-GREEN) 'green)
	    ((= i spark.fltk::FL-YELLOW) 'yellow)
	    ((= i spark.fltk::FL-BLUE) 'blue)
	    ((= i spark.fltk::FL-MAGENTA) 'magenta)
	    ((= i spark.fltk::FL-CYAN) 'cyan)
	    ((= i spark.fltk::FL-DARK-RED) 'dark-red)
	    ((= i spark.fltk::FL-DARK-GREEN) 'dark-green)
	    ((= i spark.fltk::FL-DARK-YELLOW) 'dark-yellow)
	    ((= i spark.fltk::FL-DARK-BLUE) 'dark-blue)
	    ((= i spark.fltk::FL-DARK-MAGENTA) 'dark-magenta)
	    ((= i spark.fltk::FL-DARK-CYAN) 'dark-cyan)
	    ((= i spark.fltk::FL-WHITE) 'white)
	    (else (raise-exception "integer->color" 
				   "Not a supported constant." null))))

	(define (color->integer c)
	  (if (integer? c)
	      c
	      (begin
		(case c
		  ((foreground) spark.fltk::FL-FOREGROUND-COLOR)
		  ((background2) spark.fltk::FL-BACKGROUND2-COLOR)
		  ((background) spark.fltk::FL-BACKGROUND-COLOR)
		  ((inactive) spark.fltk::FL-INACTIVE-COLOR)
		  ((selection) spark.fltk::FL-SELECTION-COLOR)
		  ((black) spark.fltk::FL-BLACK)
		  ((red) spark.fltk::FL-RED)
		  ((green) spark.fltk::FL-GREEN)
		  ((yellow) spark.fltk::FL-YELLOW)
		  ((blue) spark.fltk::FL-BLUE)
		  ((magenta) spark.fltk::FL-MAGENTA)
		  ((cyan) spark.fltk::FL-CYAN)
		  ((dark-red) spark.fltk::FL-DARK-RED)
		  ((gray) spark.fltk::FL-GRAY0)
		  ((gray0) spark.fltk::FL-GRAY0)
		  ((dark3) spark.fltk::FL-DARK3)
		  ((dark2) spark.fltk::FL-DARK2)
		  ((dark1) spark.fltk::FL-DARK1)
		  ((light3) spark.fltk::FL-LIGHT3)
		  ((light2) spark.fltk::FL-LIGHT2)
		  ((light1) spark.fltk::FL-LIGHT1)
		  ((dark-green) spark.fltk::FL-DARK-GREEN)
		  ((dark-yellow) spark.fltk::FL-DARK-YELLOW)
		  ((dark-blue) spark.fltk::FL-DARK-BLUE)
		  ((dark-magenta) spark.fltk::FL-DARK-MAGENTA)
		  ((dark-cyan) spark.fltk::FL-DARK-CYAN)
		  ((white) spark.fltk::FL-WHITE)
		  (else (raise-exception "color->integer" 
					 "Not a supported symbol." null))))))

	(define (integer->boxtype i)
	  (cond
	    ((= i spark.fltk::FL-NO-BOX) 'none)
	    ((= i spark.fltk::FL-FLAT-BOX) 'flat)
	    ((= i spark.fltk::FL-UP-BOX) 'up)
	    ((= i spark.fltk::FL-DOWN-BOX) 'down)
	    ((= i spark.fltk::FL-UP-FRAME) 'up-frame)
	    ((= i spark.fltk::FL-DOWN-FRAME) 'down-frame)
	    ((= i spark.fltk::FL-THIN-UP-BOX) 'thin-up)
	    ((= i spark.fltk::FL-THIN-DOWN-BOX) 'thin-down)
	    ((= i spark.fltk::FL-THIN-UP-FRAME) 'thin-up-frame)
	    ((= i spark.fltk::FL-THIN-DOWN-FRAME) 'thin-down-frame)
	    ((= i spark.fltk::FL-ENGRAVED-BOX) 'engraved)
	    ((= i spark.fltk::FL-EMBOSSED-BOX) 'embossed)
	    ((= i spark.fltk::FL-ENGRAVED-FRAME) 'engraved-frame)
	    ((= i spark.fltk::FL-EMBOSSED-FRAME) 'embossed-frame)
	    ((= i spark.fltk::FL-BORDER-BOX) 'border)
	    ((= i spark.fltk::FL-SHADOW-BOX) 'shadow)
	    ((= i spark.fltk::FL-BORDER-FRAME) 'border-frame)
	    ((= i spark.fltk::FL-SHADOW-FRAME) 'shadow-frame)
	    ((= i spark.fltk::FL-ROUNDED-BOX) 'rounded)
	    ((= i spark.fltk::FL-RSHADOW-BOX) 'rshadow)
	    ((= i spark.fltk::FL-ROUNDED-FRAME) 'rounded-frame)
	    ((= i spark.fltk::FL-RFLAT-BOX) 'rflat)
	    ((= i spark.fltk::FL-ROUND-UP-BOX) 'round-up)
	    ((= i spark.fltk::FL-ROUND-DOWN-BOX) 'round-down)
	    ((= i spark.fltk::FL-DIAMOND-UP-BOX) 'diamond-up)
	    ((= i spark.fltk::FL-DIAMOND-DOWN-BOX) 'diamond-down)
	    ((= i spark.fltk::FL-OVAL-BOX) 'oval)
	    ((= i spark.fltk::FL-OSHADOW-BOX) 'oshadow)
	    ((= i spark.fltk::FL-OVAL-FRAME) 'oval-frame)
	    ((= i spark.fltk::FL-OFLAT-BOX) 'oflat)
	    ((= i spark.fltk::FL-PLASTIC-UP-BOX) 'plastic-up)	
	    ((= i spark.fltk::FL-PLASTIC-DOWN-BOX) 'plastic-down)
	    ((= i spark.fltk::FL-PLASTIC-UP-FRAME) 'plastic-up-frame)
	    ((= i spark.fltk::FL-PLASTIC-DOWN-FRAME) 'plastic-down-frame)
	    ((= i spark.fltk::FL-PLASTIC-THIN-UP-BOX) 'plastic-thin-up)
	    ((= i spark.fltk::FL-PLASTIC-THIN-DOWN-BOX) 'plastic-thin-down)
	    ((= i spark.fltk::FL-PLASTIC-ROUND-UP-BOX) 'plastic-round-up)
	    ((= i spark.fltk::FL-PLASTIC-ROUND-DOWN-BOX) 'plastic-round-down)
	    ((= i spark.fltk::FL-GTK-UP-BOX) 'gtk-up)
	    ((= i spark.fltk::FL-GTK-DOWN-BOX) 'gtk-down)
	    ((= i spark.fltk::FL-GTK-UP-FRAME) 'gtk-up-frame)
	    ((= i spark.fltk::FL-GTK-DOWN-FRAME) 'gtk-down-frame)
	    ((= i spark.fltk::FL-GTK-THIN-UP-BOX) 'gtk-thin-up)
	    ((= i spark.fltk::FL-GTK-THIN-DOWN-BOX) 'gtk-thin-down)
	    ((= i spark.fltk::FL-GTK-THIN-UP-FRAME) 'gtk-thin-up)
	    ((= i spark.fltk::FL-GTK-THIN-DOWN-FRAME) 'gtk-thin-down)
	    ((= i spark.fltk::FL-GTK-ROUND-UP-BOX) 'gtk-round-up)
	    ((= i spark.fltk::FL-GTK-ROUND-DOWN-BOX) 'gtk-round-down)
	    ((= i spark.fltk::FL-FREE-BOXTYPE) 'free)
	    (else (raise-exception "integer->box-type" 
				   "Not a supported constant." null))))

	(define (boxtype->integer b)
	  (case b
	    ((none) spark.fltk::FL-NO-BOX)
	    ((flat) spark.fltk::FL-FLAT-BOX)
	    ((up) spark.fltk::FL-UP-BOX)
	    ((down) spark.fltk::FL-DOWN-BOX)
	    ((up-frame) spark.fltk::FL-UP-FRAME)
	    ((down-frame) spark.fltk::FL-DOWN-FRAME)
	    ((thin-up) spark.fltk::FL-THIN-UP-BOX)
	    ((thin-down) spark.fltk::FL-THIN-DOWN-BOX)
	    ((thin-up-frame) spark.fltk::FL-THIN-UP-FRAME)
	    ((thin-down-frame) spark.fltk::FL-THIN-DOWN-FRAME)
	    ((engraved) spark.fltk::FL-ENGRAVED-BOX)
	    ((embossed) spark.fltk::FL-EMBOSSED-BOX)
	    ((engraved-frame) spark.fltk::FL-ENGRAVED-FRAME)
	    ((embossed-frame) spark.fltk::FL-EMBOSSED-FRAME)
	    ((border) spark.fltk::FL-BORDER-BOX)
	    ((shadow) spark.fltk::FL-SHADOW-BOX)
	    ((border-frame) spark.fltk::FL-BORDER-FRAME)
	    ((shadow-frame) spark.fltk::FL-SHADOW-FRAME)
	    ((rounded) spark.fltk::FL-ROUNDED-BOX)
	    ((rshadow) spark.fltk::FL-RSHADOW-BOX)
	    ((rounded-frame) spark.fltk::FL-ROUNDED-FRAME)
	    ((rflat) spark.fltk::FL-RFLAT-BOX)
	    ((round-up) spark.fltk::FL-ROUND-UP-BOX)
	    ((round-down) spark.fltk::FL-ROUND-DOWN-BOX)
	    ((diamond-up) spark.fltk::FL-DIAMOND-UP-BOX)
	    ((diamond-down) spark.fltk::FL-DIAMOND-DOWN-BOX)
	    ((oval) spark.fltk::FL-OVAL-BOX)
	    ((oshadow) spark.fltk::FL-OSHADOW-BOX)
	    ((oval-frame) spark.fltk::FL-OVAL-FRAME)
	    ((oflat) spark.fltk::FL-OFLAT-BOX)
	    ((plastic-up) spark.fltk::FL-PLASTIC-UP-BOX)
	    ((plastic-down) spark.fltk::FL-PLASTIC-DOWN-BOX)
	    ((plastic-up-frame) spark.fltk::FL-PLASTIC-UP-FRAME)
	    ((plastic-down-frame) spark.fltk::FL-PLASTIC-DOWN-FRAME)
	    ((plastic-thin-up) spark.fltk::FL-PLASTIC-THIN-UP-BOX)
	    ((plastic-thin-down) spark.fltk::FL-PLASTIC-THIN-DOWN-BOX)
	    ((plastic-round-up) spark.fltk::FL-PLASTIC-ROUND-UP-BOX)
	    ((plastic-round-down) spark.fltk::FL-PLASTIC-ROUND-DOWN-BOX)
	    ((gtk-up) spark.fltk::FL-GTK-UP-BOX)
	    ((gtk-down) spark.fltk::FL-GTK-DOWN-BOX)
	    ((gtk-up-frame) spark.fltk::FL-GTK-UP-FRAME)
	    ((gtk-down-frame) spark.fltk::FL-GTK-DOWN-FRAME)
	    ((gtk-thin-up) spark.fltk::FL-GTK-THIN-UP-BOX)
	    ((gtk-thin-down) spark.fltk::FL-GTK-THIN-DOWN-BOX)
	    ((gtk-thin-up-frame) spark.fltk::FL-GTK-THIN-UP-FRAME)
	    ((gtk-thin-down-frame) spark.fltk::FL-GTK-THIN-DOWN-FRAME)
	    ((gtk-round-up) spark.fltk::FL-GTK-ROUND-UP-BOX)
	    ((gtk-round-down) spark.fltk::FL-GTK-ROUND-DOWN-BOX)
	    ((free) spark.fltk::FL-FREE-BOXTYPE)
	    (else (raise-exception "box-type->integer" 
				   "Not a supported symbol." null))))

	  (define (integer->align i)
	    (cond
	      ((= i spark.fltk::FL-ALIGN-BOTTOM) 'bottom)
	      ((= i spark.fltk::FL-ALIGN-CENTER) 'center)
	      ((= i spark.fltk::FL-ALIGN-CLIP) 'clip)
	      ((= i spark.fltk::FL-ALIGN-INSIDE) 'inside)
	      ((= i spark.fltk::FL-ALIGN-LEFT) 'left)
	      ((= i spark.fltk::FL-ALIGN-RIGHT) 'right)
	      ((= i spark.fltk::FL-ALIGN-TEXT-OVER-IMAGE) 'text-over-image)
	      ((= i spark.fltk::FL-ALIGN-TOP) 'top)
	      ((= i spark.fltk::FL-ALIGN-WRAP) 'wrap)
	      (else (raise-exception "integer->align" 
				     "Not a supported constant." null))))

	  (define (align->integer a)
	    (case a
	      ((bottom) spark.fltk::FL-ALIGN-BOTTOM)
	      ((center) spark.fltk::FL-ALIGN-CENTER)
	      ((clip) spark.fltk::FL-ALIGN-CLIP)
	      ((inside) spark.fltk::FL-ALIGN-INSIDE)
	      ((left) spark.fltk::FL-ALIGN-LEFT)
	      ((right) spark.fltk::FL-ALIGN-RIGHT)
	      ((text-over-image) spark.fltk::FL-ALIGN-TEXT-OVER-IMAGE)
	      ((top) spark.fltk::FL-ALIGN-TOP)
	      ((wrap) spark.fltk::FL-ALIGN-WRAP)
	      (else (raise-exception "align->integer" 
				     "Not a supported symbol." null))))

	  (define (list->align a)
	    (if (not (list? a))
		(list (align->integer a))
		(begin
		  (let ((ret (list)))
		    (let loop ()
		      (if (not (eqv? a null))
			  (begin
			    (set! ret (append ret 
					      (list 
					       (align->integer 
						(car a)))))
			    (set! a (cdr a))
			    (loop))))
		    ret))))

	  (define (align->list a)
	    (let ((ret (list)))
	      (let loop ()
		(if (not (eqv? a null))
		    (begin
		      (set! ret (append ret (list (integer->align (car a)))))
		      (set! a (cdr a))
		      (loop))))
	      ret))

	  (define (integer->font i)
	    (if (integer? i)
		i
		(begin
		  (cond
		   ((= i spark.fltk::FL-HELVETICA) 'helvetica)
		   ((= i spark.fltk::FL-HELVETICA-BOLD) 'helvetica-bold)
		   ((= i spark.fltk::FL-HELVETICA-ITALIC) 'helvetica-italic)
		   ((= i spark.fltk::FL-HELVETICA-BOLD-ITALIC) 'helvetica-bold-italic)
		   ((= i spark.fltk::FL-COURIER) 'courier)
		   ((= i spark.fltk::FL-COURIER-BOLD) 'courier-bold)
		   ((= i spark.fltk::FL-COURIER-ITALIC) 'courier-italic)
		   ((= i spark.fltk::FL-COURIER-BOLD-ITALIC) 'courier-bold-italic)
		   ((= i spark.fltk::FL-TIMES) 'times)
		   ((= i spark.fltk::FL-TIMES-BOLD) 'times-bold)
		   ((= i spark.fltk::FL-TIMES-ITALIC) 'times-italic)
		   ((= i spark.fltk::FL-TIMES-BOLD-ITALIC) 'times-bold-italic)
		   ((= i spark.fltk::FL-SYMBOL) 'symbol)
		   ((= i spark.fltk::FL-SCREEN) 'screen)
		   ((= i spark.fltk::FL-SCREEN-BOLD) 'screen-bold)
		   ((= i spark.fltk::FL-ZAPF-DINGBATS) 'zapf-dingbats)
		   ((= i spark.fltk::FL-FREE-FONT) 'free-font)
		   ((= i spark.fltk::FL-BOLD) 'bold)
		   ((= i spark.fltk::FL-ITALIC) 'italic)
		   (else (raise-exception "integer->font" 
					  "Not a supported constant." null))))))

	  (define (font->integer f)
	    (case f
	      ((helvetica) spark.fltk::FL-HELVETICA)
	      ((helvetica-bold) spark.fltk::FL-HELVETICA-BOLD)
	      ((helvetica-italic) spark.fltk::FL-HELVETICA-ITALIC)
	      ((helvetica-bold-italic) spark.fltk::FL-HELVETICA-BOLD-ITALIC)
	      ((courier) spark.fltk::FL-COURIER)
	      ((courier-bold) spark.fltk::FL-COURIER-BOLD)
	      ((courier-italic) spark.fltk::FL-COURIER-ITALIC)
	      ((courier-bold-italic) spark.fltk::FL-COURIER-BOLD-ITALIC)
	      ((times) spark.fltk::FL-TIMES)
	      ((times-bold) spark.fltk::FL-TIMES-BOLD)
	      ((times-italic) spark.fltk::FL-TIMES-ITALIC)
	      ((times-bold-italic) spark.fltk::FL-TIMES-BOLD-ITALIC)
	      ((symbol) spark.fltk::FL-SYMBOL)
	      ((screen) spark.fltk::FL-SCREEN)
	      ((screen-bold) spark.fltk::FL-SCREEN-BOLD)
	      ((zapf-dingbats) spark.fltk::FL-ZAPF-DINGBATS)
	      ((free-font) spark.fltk::FL-FREE-FONT)
	      ((bold) spark.fltk::FL-BOLD)
	      ((italic) spark.fltk::FL-ITALIC)
	      (else f)))

	  (define (make-font f)
	    (let ((ret 0))
	      (if (list? f)
		  (begin
		    (let loop ()
		      (if (not (null? f))
			  (begin
			    (set! ret (+ ret (font->integer (car f))))
			    (set! f (cdr f))
			    (loop)))))
		  (set! ret (font->integer f)))
	      ret))

	  (define (integer->labeltype i)
	    (cond
	      ((= i spark.fltk::FL-NORMAL-LABEL) 'normal)
	      ((= i spark.fltk::FL-NO-LABEL) 'none)
	      ((= i spark.fltk::FL-SHADOW-LABEL) 'shadow)
	      ((= i spark.fltk::FL-ENGRAVED-LABEL) 'engraved)
	      ((= i spark.fltk::FL-EMBOSSED-LABEL) 'embossed)
	      ((= i spark.fltk::FL-ICON-LABEL) 'icon)
	      (else (raise-exception "integer->labeltype" 
				     "Not a supported constant." null))))

	  (define (labeltype->integer i)
	    (case i
	      ((normal) spark.fltk::FL-NORMAL-LABEL)
	      ((none) spark.fltk::FL-NO-LABEL)
	      ((shadow) spark.fltk::FL-SHADOW-LABEL)
	      ((engraved) spark.fltk::FL-ENGRAVED-LABEL)
	      ((embossed) spark.fltk::FL-EMBOSSED-LABEL)
	      ((icon) spark.fltk::FL-ICON-LABEL)
	      (else (raise-exception "labeltype->integer" 
				     "Not a supported symbol." null))))

	  (define (integer->when i)
	    (cond
	      ((= i spark.fltk::FL-WHEN-NEVER) 'never)
	      ((= i spark.fltk::FL-WHEN-CHANGED) 'changed)
	      ((= i spark.fltk::FL-WHEN-RELEASE) 'release)
	      ((= i spark.fltk::FL-WHEN-RELEASE-ALWAYS) 'release-always)
	      ((= i spark.fltk::FL-WHEN-ENTER-KEY) 'enter-key)
	      ((= i spark.fltk::FL-WHEN-ENTER-KEY-ALWAYS) 'enter-key-always)
	      ((= i spark.fltk::FL-WHEN-ENTER-KEY-CHANGED) 'enter-key-changed)
	      ((= i spark.fltk::FL-WHEN-NOT-CHANGED) 'not-changed)
	      (else (raise-exception "integer->when" 
				     "Not a supported constant." null))))

	  (define (when->integer w)
	    (case w
	      ((never) spark.fltk::FL-WHEN-NEVER)
	      ((changed) spark.fltk::FL-WHEN-CHANGED)
	      ((release) spark.fltk::FL-WHEN-RELEASE)
	      ((release-always) spark.fltk::FL-WHEN-RELEASE-ALWAYS)
	      ((enter-key) spark.fltk::FL-WHEN-ENTER-KEY)
	      ((enter-key-always) spark.fltk::FL-WHEN-ENTER-KEY-ALWAYS)
	      ((enter-key-changed) spark.fltk::FL-WHEN-ENTER-KEY-CHANGED)
	      ((not-changed) spark.fltk::FL-WHEN-NOT-CHANGED)
	      (else (raise-exception "when->integer" 
				     "Not a supported symbol." null))))

	  (define (integer->cursor i)
	    (cond
	      ((= i spark.fltk::FL-CURSOR-DEFAULT) 'default)
	      ((= i spark.fltk::FL-CURSOR-ARROW) 'arrow)
	      ((= i spark.fltk::FL-CURSOR-CROSS) 'cross)
	      ((= i spark.fltk::FL-CURSOR-WAIT) 'wait)
	      ((= i spark.fltk::FL-CURSOR-INSERT) 'insert)
	      ((= i spark.fltk::FL-CURSOR-HAND) 'hand)
	      ((= i spark.fltk::FL-CURSOR-HELP) 'help)
	      ((= i spark.fltk::FL-CURSOR-MOVE) 'move)
	      ((= i spark.fltk::FL-CURSOR-NS) 'ns)
	      ((= i spark.fltk::FL-CURSOR-WE) 'we)
	      ((= i spark.fltk::FL-CURSOR-NWSE) 'nwse)
	      ((= i spark.fltk::FL-CURSOR-NESW) 'nesw)
	      ((= i spark.fltk::FL-CURSOR-NONE) 'none)
	      ((= i spark.fltk::FL-CURSOR-N) 'n)
	      ((= i spark.fltk::FL-CURSOR-NE) 'ne)
	      ((= i spark.fltk::FL-CURSOR-E) 'e)
	      ((= i spark.fltk::FL-CURSOR-SE) 'se)
	      ((= i spark.fltk::FL-CURSOR-S) 's)
	      ((= i spark.fltk::FL-CURSOR-SW) 'sw)
	      ((= i spark.fltk::FL-CURSOR-W) 'w)
	      ((= i spark.fltk::FL-CURSOR-NW) 'nw)
	      (else 'default)))

	  (define (cursor->integer c)
	    (if (integer? c)
		c
		(begin
		  (case c
		    ((default) spark.fltk::FL-CURSOR-DEFAULT)
		    ((arrow) spark.fltk::FL-CURSOR-ARROW)
		    ((cross) spark.fltk::FL-CURSOR-CROSS)
		    ((wait) spark.fltk::FL-CURSOR-WAIT)
		    ((insert) spark.fltk::FL-CURSOR-INSERT)
		    ((hand) spark.fltk::FL-CURSOR-HAND)
		    ((help) spark.fltk::FL-CURSOR-HELP)
		    ((move) spark.fltk::FL-CURSOR-MOVE)
		    ((ns) spark.fltk::FL-CURSOR-NS)
		    ((we) spark.fltk::FL-CURSOR-WE)
		    ((nwse) spark.fltk::FL-CURSOR-NWSE)
		    ((nesw) spark.fltk::FL-CURSOR-NESW)
		    ((none) spark.fltk::FL-CURSOR-NONE)
		    ((n) spark.fltk::FL-CURSOR-N)
		    ((ne) spark.fltk::FL-CURSOR-NE)
		    ((e) spark.fltk::FL-CURSOR-E)
		    ((se) spark.fltk::FL-CURSOR-SE)
		    ((s) spark.fltk::FL-CURSOR-S)
		    ((sw) spark.fltk::FL-CURSOR-SW)
		    ((w) spark.fltk::FL-CURSOR-W)
		    ((nw) spark.fltk::FL-CURSOR-NW)
		    (else (raise-exception "cursor->integer"
					   "Not a supported symbol." null))))))

	  (define (key->integer k)
	    (if (integer? k)
		k
		(begin
		  (case k
		    ((button) spark.fltk::FL-Button)
		    ((backspace) spark.fltk::FL-BackSpace)
		    ((tab) spark.fltk::FL-Tab)
		    ((enter) spark.fltk::FL-Enter)
		    ((pause) spark.fltk::FL-Pause)
		    ((Scroll-Lock) spark.fltk::FL-Scroll-Lock)
		    ((escape) spark.fltk::FL-Escape)
		    ((home) spark.fltk::FL-Home)
		    ((left) spark.fltk::FL-Left)
		    ((up) spark.fltk::FL-Up)
		    ((right) spark.fltk::FL-Right)
		    ((down) spark.fltk::FL-Down)
		    ((page-up) spark.fltk::FL-Page-Up)
		    ((page-down) spark.fltk::FL-Page-Down)
		    ((end) spark.fltk::FL-End)
		    ((print) spark.fltk::FL-Print)
		    ((insert) spark.fltk::FL-Insert)
		    ((menu) spark.fltk::FL-Menu)
		    ((help) spark.fltk::FL-Help)
		    ((Num-Lock) spark.fltk::FL-Num-Lock)
		    ((kp) spark.fltk::FL-KP)
		    ((kp-enter) spark.fltk::FL-KP-Enter)
		    ((kp-last) spark.fltk::FL-KP-Last)
		    ((f) spark.fltk::FL-F)
		    ((shift-l) spark.fltk::FL-Shift-L)
		    ((shift-r) spark.fltk::FL-Shift-R)
		    ((control-l) spark.fltk::FL-Control-L)
		    ((control-r) spark.fltk::FL-Control-R)
		    ((Caps-Lock) spark.fltk::FL-Caps-Lock)
		    ((meta-l) spark.fltk::FL-Meta-L)
		    ((meta-r) spark.fltk::FL-Meta-R)
		    ((alt-l) spark.fltk::FL-Alt-L)
		    ((alt-r) spark.fltk::FL-Alt-R)
		    ((delete) spark.fltk::FL-Delete)
		    ((SHIFT) spark.fltk::FL-SHIFT)
		    ((CAPS-LOCK) spark.fltk::FL-CAPS-LOCK)
		    ((ctrl) spark.fltk::FL-CTRL)
		    ((alt) spark.fltk::FL-ALT)
		    ((NUM-LOCK) spark.fltk::FL-NUM-LOCK)
		    ((meta) spark.fltk::FL-META)
		    ((SCROLL-LOCK) spark.fltk::FL-SCROLL-LOCK)
		    ((button1) spark.fltk::FL-BUTTON1)
		    ((button2) spark.fltk::FL-BUTTON2)
		    ((button3) spark.fltk::FL-BUTTON3)
		    ((buttons) spark.fltk::FL-BUTTONS)
		    (else (raise-exception "key->integer"
					   "Not a supported symbol." null))))))

	  (define ascii-table (vector null null null
				      null null null
				      null null null
				      null null null
				      null null null
				      null null null
				      null null null
				      null null null
				      null null null
				      null null null
				      null 'space 'exclamation
				      'double-quote 'hash
				      'dollar 'percent 'ampersand
				      'quote 'opening-bracket 'closing-bracket
				      'asterisk 'plus 'comma 'minus 'period
				      'slash '0 '1 '2 '3 '4 '5 '6 '7 '8 '9
				      'colon 'semi-colon 'lt 'equal 'gt 'question-mark
				      'at 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O
				      'P 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z 'opening-square-bracket
				      'back-slash 'closing-square-bracket 'caret
				      'underscore 'grave 'a 'b 'c 'd 'e 'f 'g 'h
				      'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 
				      'x 'y 'z 'opening-curly-bracket 'pipe 'closing-curly-bracket
				      'tilde 'delete))

	  (define ascii-table-size (vector-length ascii-table))

	  (define (keycode->ascii i)
	    (if (and (>= i 0)
		     (< i ascii-table-size))
		(vector-ref ascii-table (- i 1))
		null))

	  (define (integer->key i)
	    (let ((a (keycode->ascii i)))
	      (if (not (null? a))
		  a
		  (begin
		    (cond
		     ((= i spark.fltk::FL-Button) 'button)
		     ((= i spark.fltk::FL-BackSpace) 'backspace) 
		     ((= i spark.fltk::FL-Tab)' tab) 
		     ((= i spark.fltk::FL-Enter) 'enter) 
		     ((= i spark.fltk::FL-Pause) 'pause) 
		     ((= i spark.fltk::FL-Scroll-Lock) 'Scroll-Lock) 
		     ((= i spark.fltk::FL-Escape) 'escape) 
		     ((= i spark.fltk::FL-Home) 'home) 
		     ((= i spark.fltk::FL-Left) 'left) 
		     ((= i spark.fltk::FL-Up) 'up) 
		     ((= i spark.fltk::FL-Right) 'right) 
		     ((= i spark.fltk::FL-Down) 'down) 
		     ((= i spark.fltk::FL-Page-Up) 'page-up) 
		     ((= i spark.fltk::FL-Page-Down) 'page-down) 
		     ((= i spark.fltk::FL-End) 'end) 
		     ((= i spark.fltk::FL-Print) 'print) 
		     ((= i spark.fltk::FL-Insert) 'insert) 
		     ((= i spark.fltk::FL-Menu) 'menu) 
		     ((= i spark.fltk::FL-Help) 'help) 
		     ((= i spark.fltk::FL-Num-Lock) 'Num-Lock) 
		     ((= i spark.fltk::FL-KP) 'kp) 
		     ((= i spark.fltk::FL-KP-Enter) 'kp-enter) 
		     ((= i spark.fltk::FL-KP-Last) 'kp-last) 
		     ((= i spark.fltk::FL-F) 'f) 
		     ((= i spark.fltk::FL-Shift-L) 'shift-l) 
		     ((= i spark.fltk::FL-Shift-R) 'shift-r) 
		     ((= i spark.fltk::FL-Control-L) 'control-l) 
		     ((= i spark.fltk::FL-Control-R) 'control-r) 
		     ((= i spark.fltk::FL-Caps-Lock) 'Caps-Lock) 
		     ((= i spark.fltk::FL-Meta-L) 'meta-l) 
		     ((= i spark.fltk::FL-Meta-R) 'meta-r) 
		     ((= i spark.fltk::FL-Alt-L) 'alt-l) 
		     ((= i spark.fltk::FL-Alt-R) 'alt-r) 
		     ((= i spark.fltk::FL-Delete) 'delete) 
		     ((= i spark.fltk::FL-SHIFT) 'SHIFT) 
		     ((= i spark.fltk::FL-CAPS-LOCK) 'CAPS-LOCK) 
		     ((= i spark.fltk::FL-CTRL) 'ctrl) 
		     ((= i spark.fltk::FL-ALT) 'alt) 
		     ((= i spark.fltk::FL-NUM-LOCK) 'NUM-LOCK) 
		     ((= i spark.fltk::FL-META) 'meta) 
		     ((= i spark.fltk::FL-SCROLL-LOCK) 'SCROLL-LOCK) 
		     ((= i spark.fltk::FL-BUTTON1) 'button1) 
		     ((= i spark.fltk::FL-BUTTON2) 'button2) 
		     ((= i spark.fltk::FL-BUTTON3) 'button3) 
		     ((= i spark.fltk::FL-BUTTONS) 'buttons) 
		     (else i))))))

	  (define (string->key s)
	    (let ((k (string-downcase s)))
	      (cond
	       ((string=? k "button") 'button)
	       ((string=? k "backspace") 'backspace)
	       ((string=? k "tab") 'tab)
	       ((string=? k "enter") 'enter)
	       ((string=? k "pause") 'pause)
	       ((string=? k "scroll-lock-f") 'Scroll-Lock)
	       ((string=? k "escape") 'escape)
	       ((string=? k "home") 'home)
	       ((string=? k "left") 'left)
	       ((string=? k "up") 'up)
	       ((string=? k "right") 'right)
	       ((string=? k "down") 'down)
	       ((string=? k "page-up") 'page-up)
	       ((string=? k "page-down") 'page-down)
	       ((string=? k "end") 'end)
	       ((string=? k "print") 'print)
	       ((string=? k "insert") 'insert)
	       ((string=? k "menu") 'menu)
	       ((string=? k "help") 'help)
	       ((string=? k "num-lock-f") 'Num-Lock)
	       ((string=? k "kp") 'kp)
	       ((string=? k "kp-enter") 'kp-enter)
	       ((string=? k "kp-last") 'kp-last)
	       ((string=? k "f") 'f)
	       ((string=? k "shift-l") 'shift-l)
	       ((string=? k "shift-r") 'shift-r)
	       ((string=? k "control-l") 'control-l)
	       ((string=? k "control-r") 'control-r)
	       ((string=? k "caps-lock-f") 'Caps-Lock)
	       ((string=? k "meta-l") 'meta-l)
	       ((string=? k "meta-r") 'meta-r)
	       ((string=? k "alt-l") 'alt-l)
	       ((string=? k "alt-r") 'alt-r)
	       ((string=? k "delete") 'delete)
	       ((string=? k "shift") 'SHIFT)
	       ((string=? k "caps-lock") 'CAPS-LOCK)
	       ((string=? k "ctrl") 'ctrl)
	       ((string=? k "alt") 'alt)
	       ((string=? k "num-lock") 'NUM-LOCK)
	       ((string=? k "meta") 'meta)
	       ((string=? k "scroll-lock") 'SCROLL-LOCK)
	       ((string=? k "button1") 'button-1)
	       ((string=? k "button2") 'button-2)
	       ((string=? k "button3") 'button-3)
	       ((string=? k "buttons") 'buttons)
	       (else (raise-exception "string->key"
				      "String cannot be mapped to a symbol." null)))))

	  (define (menuflag->integer f)
	    (if (integer? f)
		f
		(begin
		  (case f
		    ((inactive) spark.fltk::FL-MENU-INACTIVE)
		    ((toggle) spark.fltk::FL-MENU-TOGGLE)
		    ((value) spark.fltk::FL-MENU-VALUE)
		    ((radio) spark.fltk::FL-MENU-RADIO)
		    ((invisible) spark.fltk::FL-MENU-INVISIBLE)
		    ((submenu-pointer) spark.fltk::FL-SUBMENU-POINTER)
		    ((submenu) spark.fltk::FL-SUBMENU)
		    ((divider) spark.fltk::FL-MENU-DIVIDER)
		    ((horizontal) spark.fltk::FL-MENU-HORIZONTAL)
		    (else (raise-exception "maenuflag->integer"
					   "Not a supported symbol." null))))))

	  (define (cursorstyle->integer s)
	    (case s
	      ((normal) spark.fltk::NORMAL-CURSOR)
	      ((caret) spark.fltk::CARET-CURSOR)
	      ((dim) spark.fltk::DIM-CURSOR)
	      ((block) spark.fltk::BLOCK-CURSOR)
	      ((heavy) spark.fltk::HEAVY-CURSOR)
	      (else spark.fltk::NORMAL-CURSOR)))

	  (define (state->integer s)
	    (case s
	      ((shift) spark.fltk::FL-SHIFT)
	      ((caps-lock) spark.fltk::FL-CAPS-LOCK)
	      ((ctrl) spark.fltk::FL-CTRL)
	      ((alt) spark.fltk::FL-ALT)
	      ((command) spark.fltk::FL-COMMAND)
	      ((num-lock) spark.fltk::FL-NUM-LOCK)
	      ((meta) spark.fltk::FL-META)
	      ((scroll-lock) spark.fltk::FL-SCROLL-LOCK)
	      ((button1) spark.fltk::FL-BUTTON1)
	      ((button2) spark.fltk::FL-BUTTON2)
	      ((button3) spark.fltk::FL-BUTTON3)
	      ((buttons) spark.fltk::FL-BUTTONS)
	      ((any) spark.fltk::ANY-STATE)
	      (else (raise-exception "state->integer"
				     "Not a valid state symbol." null))))

	  (define (integer->state i)
	    (cond
	      ((= i spark.fltk::FL-SHIFT) 'shift)
	      ((= i spark.fltk::FL-CAPS-LOCK) 'caps-lock) 
	      ((= i spark.fltk::FL-CTRL) 'ctrl) 
	      ((= i spark.fltk::FL-ALT) 'alt) 
	      ((= i spark.fltk::FL-COMMAND) 'command) 
	      ((= i spark.fltk::FL-NUM-LOCK) 'num-lock) 
	      ((= i spark.fltk::FL-META) 'meta) 
	      ((= i spark.fltk::FL-SCROLL-LOCK) 'scroll-lock) 
	      ((= i spark.fltk::FL-BUTTON1) 'button1) 
	      ((= i spark.fltk::FL-BUTTON2) 'button2) 
	      ((= i spark.fltk::FL-BUTTON3) 'button3) 
	      ((= i spark.fltk::FL-BUTTONS) 'buttons) 
	      ((= i spark.fltk::ANY-STATE) 'any) 
	      (else (raise-exception "integer->integer"
				     "Not a valid state." null))))
	  
	(export integer->color
		color->integer
		integer->boxtype
		boxtype->integer
		integer->align 
		align->integer
		integer->font
		font->integer
		make-font
		integer->labeltype
		labeltype->integer
		integer->when
		when->integer
		integer->cursor
		cursor->integer
		key->integer
		integer->key
		menuflag->integer
		string->key
		list->align
		align->list
		cursorstyle->integer
		state->integer
		integer->state
		widget-handle
		set-widget-handle!
		struct-name
		new-widget
		widget-class
		set-widget-class!
		widget-type
		set-widget-type!
		widget-data 
		set-widget-data!))
