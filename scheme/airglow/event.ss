;; Event handling.
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

(library airglow-event

	 (import (airglow-util)
		 ((prefix spark.fltk:: #%spark-fltk)))

	 (export event-button event-clicks
		 event-clicks! event-scroll-x
		 event-scroll-y event-screen-x
		 event-screen-y event-x
		 event-y event-inside? 
		 event-click? event-click!
		 event-key event-key?
		 event-original-key event-state
		 event-state! event-text
		 event-shift? event-ctrl?
		 event-last)

	 (define (event-button)
	   (spark.fltk::fl-event-button))		

	 (define (event-clicks)
	   (spark.fltk::fl-event-clicks))

	 (define (event-clicks! n)
	   (spark.fltk::fl-event-clicks n))

	 (define (event-scroll-x)
	   (spark.fltk::fl-event-dx))

	 (define (event-scroll-y)
	   (spark.fltk::fl-event-dy))

	 (define (event-screen-x)
	   (spark.fltk::fl-event-x-root))

	 (define (event-screen-y)
	   (spark.fltk::fl-event-y-root))

	 (define (event-x)
	   (spark.fltk::fl-event-x))

	 (define (event-y)
	   (spark.fltk::fl-event-y))

	 (define (event-inside? . args)
	   (if (= (length args) 1)
	       (begin
		 (let ((w (widget-handle (car args))))
		   (spark.fltk::fl-event-inside w)))
	       (begin
		 (let ((x 0) (y 0) (w 0) (h 0))
		   (set! x (car args))
		   (set! args (cdr args))
		   (set! y (car args))
		   (set! args (cdr args))
		   (set! w (car args))
		   (set! args (cdr args))
		   (set! h (car args))
		   (spark.fltk::fl-event-inside x y w h)))))

	 (define (event-click?)
	   (spark.fltk::fl-event-is-click))

	 (define (event-click!)
	   (spark.fltk::fl-event-is-click 0))

	 (define (event-key)
	   (integer->key (spark.fltk::fl-event-key)))

	 ;; Returns true if the key was held down during the
	 ;; last event.
	 (define (event-key? k)
	   (spark.fltk::fl-event-key (key->integer k)))

	 (define (event-original-key)
	   (integer->key (spark.fltk::fl-event-original-key)))

	 (define (event-state)
	   (intlist->eventstates (spark.fltk::fl-event-state)))
	 
	 (define (event-state! s)
	   (spark.fltk::fl-event-state (eventstates->intlist s)))

	 (define (event-text)
	   (spark.fltk::fl-event-text))

	 (define (event-shift?)
	   (spark.fltk::fl-event-shift))

	 (define (event-ctrl?)
	   (spark.fltk::fl-event-ctrl))

	 (define (event-last)
	   (spark.fltk::fl-last-event))

	 (define (intlist->eventstates args)
	   (let ((ret (list))
		 (i 0))
	     (let loop ()
	       (if (not (eqv? args null))
		   (begin
		     (set! i (car args))
		     (if (not (eqv? i null))
			 (set! ret (append ret (list (integer->state i)))))
		     (set! args (cdr args))
		     (loop))))
	     ret))		  

	 (define (eventstates->intlist args)
	   (let ((ret (list))
		 (s null))
	     (let loop ()
	       (if (not (eqv? args null))
		   (begin
		     (set! s (car args))
		     (if (not (eqv? s null))
			 (set! ret (append ret (list (state->integer s)))))
		     (set! args (cdr args))
		     (loop))))
	     ret)))


