;; Message box functions.
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

(library airglow-ask

	 (import (airglow-util)
		 ((prefix spark.fltk:: #%spark-fltk)))
	 
	 (export ask-choice ask-message ask-beep ask-input
		 ask-password)

	 (define (ask-choice label . args)
	   (let ((b0 null) (b1 null) (b2 null))
	     (if (not (eqv? args null))
		 (begin
		   (set! b0 (car args))
		   (set! args (cdr args))))
	     (if (not (eqv? args null))
		 (begin
		   (set! b1 (car args))
		   (set! args (cdr args))))
	     (if (not (eqv? args null))
		 (begin
		   (set! b2 (car args))
		   (set! args (cdr args))))
	     (spark.fltk::choices label b0 b1 b2)))

	 (define (ask-message msg . args)
	   (let ((type 'info))
	     (if (not (eqv? args null))
		 (set! type (car args)))
	     (case type 
	       ((info) (spark.fltk::message msg))
	       ((alert) (spark.fltk::alert msg))
	       (else (spark.fltk::message msg)))))

	 (define (ask-beep . args)
	   (let ((type 'default))
	     (if (not (eqv? args null))
		 (set! type (car args)))
	     (spark.fltk::beep (beep->integer type))))

	 (define (ask-input label . args)
	   (let ((defval null))
	     (if (not (eqv? args null))
		 (set! defval (car args)))
	     (spark.fltk::input-dialog label defval)))

	 (define (ask-password label . args)
	   (let ((defval null))
	     (if (not (eqv? args null))
		 (set! defval (car args)))
	     (spark.fltk::password-dialog label defval)))

	 (define (beep->integer b)
	   (if (integer? b)
	       b)
	   (begin
	     (case b
	       ((message) spark.fltk::FL-BEEP-MESSAGE)
	       ((error) spark.fltk::FL-BEEP-ERROR)
	       ((question) spark.fltk::FL-BEEP-QUESTION)
	       ((password) spark.fltk::FL-BEEP-PASSWORD)
	       ((notification) spark.fltk::FL-BEEP-NOTIFICATION)
	       (else spark.fltk::FL-BEEP-DEFAULT)))))

