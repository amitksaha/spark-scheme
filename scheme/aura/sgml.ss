;; Scheme like syntax for SGML documents.
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

(library aura-sgml

	 (require-for-syntax mzscheme)

	 (export html? css tag <> : sgml)

	 (define (html? obj)
	   (if (procedure? obj)
	       (begin
		 (let ((t ((obj 'type))))
		   (or (eq? t 'css)
		       (eq? t 'tag))))
	       #f))

	 (define (css n args)
	   (let ((props null)
		 (name n)
		 (self null)
		 ;; private message handlers
		 (f-add-prop null)
		 (f-get-prop null)
		 (f-to-text null)
		 (f-type null)
		 (f-props null)
		 (f-name null))
	     (if (null? args)
		 (set! props (list))
		 (set! props args))
	     (set! self 
		   (lambda (msg)
		     (case msg
		       ((prop!) f-add-prop)
		       ((prop) f-get-prop)
		       ((text) f-to-text)
		       ((type) f-type)
		       ((props) f-props)
		       ((name) f-name)
		       (else (error "Cannot handle this message.")))))
	     
	     (set! f-get-prop
		   (lambda (key)
		     (assq key props)))

	     (set! f-add-prop
		   (lambda (key val)
		     (set! props (append props (list (cons key val))))))

	     (set! f-to-text
		   (lambda ()
		     (let ((out (open-output-string)))
		       (fprintf out "~a {~n" name)
		       (for prop in props
			    (fprintf out "~a: ~a;~n" (car prop) (cdr prop)))
		       (fprintf out "}~n")
		       (get-output-string out))))

	     (set! f-type
		   (lambda ()
		     'css))	

	     (set! f-props
		   (lambda ()
		     props))

	     (set! f-name
		   (lambda ()
		     name))

	     self))

	 (define (tag n args c)
	   (let ((super (css n args))
		 (self null)
		 (content c)
		 (f-type null)
		 (f-add-content null)
		 (f-to-text null)
		 (print-content null))

	     (if (null? content)
		 (set! content (list)))

	     (set! self
		   (lambda (msg)
		     (case msg
		       ((text) f-to-text)
		       ((content!) f-add-content)
		       ((type) f-type)
		       (else (super msg)))))

	     (set! f-type
		   (lambda ()
		     'tag))

	     (set! f-add-content
		   (lambda (c)
		     (set! content (append content (list c)))))

	     (set! f-to-text
		   (lambda ()
		     (let ((out (open-output-string))
			   (props ((super 'props)))
			   (name ((super 'name))))
		       (fprintf out "<~a" name)
		       (if (not (null? props))
			   (begin
			     (fprintf out " ")
			     (for prop in props
				  (fprintf out "~a=\"~a\" "
					   (car prop) (cdr prop)))))
		       (fprintf out ">~n")
		       (if (list? content)
			   (begin
			     (for c in content
				  (print-content c out)))
			   (print-content c out))
			     
		       (fprintf out "</~a>~n" name)
		       (get-output-string out))))

	     (set! print-content
		   (lambda (c out)
		     (if (string? c)
			 (fprintf out "~a" c)
			 (fprintf out ((c 'text))))))		     
	     self))
	 
	 ;; Write any SGML document in Scheme syntax.
	 (define (sgml sgml-doc)
	   (let ((s (stack))
		 (root null)
		 (process-sgml null)
		 (process-list null))
	     (set! process-sgml
		   (lambda (sgml-doc)
		     (for elem in sgml-doc
			  (cond
			   ((symbol? elem)
			    (let ((t (tag elem () ()))
				  (top null))
			      (if (not (stack-empty? s))
				  (begin
				    (set! top (stack-top s))
				    ((top 'content!) t))
				  (set! root t))
			      (stack-push! s t)))
			   ((list? elem)
			    (process-list elem))
			   ((or (string? elem)
				(html? elem)
				(number? elem))
			    (((stack-top s) 'content!) elem))
			   (else 
			    (if (not (void? elem))
				(begin
				  (let ((out (open-output-string)))
				    (fprintf out "Invalid type for ~a." elem)
				    (error (get-output-string out))))))))
		     (stack-pop! s)))

	     (set! process-list
		   (lambda (elem)
		     (let ((t (stack-top s)))
		       (if (list? (car elem)) ;; attributes
			   (begin
			     (for a in elem
			      ((t 'prop!) (car a) (car (cdr a)))))
			   (process-sgml elem)))))
	     (process-sgml sgml-doc)
	     root))
	     
	 (define-syntax :
	   (syntax-rules ()
	     ((_ (k v) ...)
	      (list (cons k v) ...))))

	 (define-syntax <>
	   (syntax-rules ()
	     ((_ n a c ...)
	      (tag n a (list c ...))))))