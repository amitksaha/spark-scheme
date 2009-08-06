;; A wrapper over low-level expat parser functions.
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

(library xml

	(import (exception) (asserts)
		((prefix spark.expat:: #%spark-expat)))

	(export xml-parser xml-parser-dispose xml-parser-element-handlers!
		 xml-parser-special-handler! xml-parser-user-data!
		 xml-parser-parse xml-parser-last-error)

	;; An XML parser object.
	(define-struct xml-parser-s (handle)) ;; expat parser handle
	
	;; Creates and initializes a XML parser object.
	;; Accepts 7 optional arguments:
	;; 1. Encoding name.
	;; 2. A boolean that denotes whether the parser should
	;; do namespace processing.
	;; 3. A separator character. Valid only if namespace 
	;; processing is turned on.
	;; 4. start-element-handler. A function object. 
	;; The function should have the signature:
	;; (define (start user-data element attributes))
	;; 5. end-element-handler. A function object.
	;; The function should have the signature:
	;; (define (end user-data element))
	;; 6. character-handler. A function object.
	;; It should have the signature:
	;; (define (cdata user-data text))
	;; 7. user-data. An object that is passed to each callback.
	;; User data can be used to pass values between handlers without
	;; using globals.
	;; Returns the new parser object on success.
	(define (xml-parser . args)
	  (let ((self null) 
		(handle null)
		(encoding null)
		(namespace-processing #f)
		(sep null)
		(len 0) 
		(tmp null)
		(seh null)
		(eeh null))
	    (if (not (eqv? args null))
		(begin
		  (set! encoding (car args))
		  (assert-string encoding)
		  (set! args (cdr args))))
	    (if (not (eqv? args null))
		(begin
		  (set! namespace-processing (car args))
		  (assert-boolean namespace-processing)
		  (set! args (cdr args))))
	    (if (not (eqv? args null))
		(begin 
		  (set! sep (car args))
		  (assert-string sep)
		  (set! args (cdr args))))
	    
	    (set! handle (spark.expat::open encoding 
					    namespace-processing
					    sep))

	    (if (eqv? handle null)
		(raise-exception "xml-parser"
				 "Null handle to XML parser."
				 null))

	    (set! self (make-xml-parser-s handle))

	    (if (not (eqv? args null))
		(begin
		  (set! seh (car args))
		  (set! args (cdr args))))
	    (if (not (eqv? args null))
		(begin
		  (set! eeh (car args))
		  (set! args (cdr args))))
	    (xml-parser-element-handlers! self seh eeh)
	    (if (not (eqv? args null))
		(begin
		  (set! tmp (car args))
		  (xml-parser-special-handler! self tmp 'character-data)
		  (set! args (cdr args))))
	    (if (not (eqv? args null))
		(begin
		  (set! tmp (car args))
		  (xml-parser-user-data! self tmp)))
	    self))
	
	;; Frees the resources held by the expat parser.
	(define (xml-parser-dispose self)
	  (spark.expat::close (xml-parser-s-handle self))
	  (set-xml-parser-s-handle! self null))

	;; sets start element and end element handlers.
	;; one or both of the handlers can be null.
	(define (xml-parser-element-handlers! self start-handler end-handler)
	  (if (not (eqv? start-handler null))
	      (assert-procedure start-handler))
	  (if (not (eqv? end-handler null))
	      (assert-procedure end-handler))
	  (spark.expat::set-element-handlers! (xml-parser-s-handle self) 
					      start-handler
					      end-handler))

	;; Sets a special handler. 
	(define (xml-parser-special-handler! self handler type)
	  (if (not (eqv? handler null))
	      (assert-procedure handler))
	  (let ((handle (xml-parser-s-handle self)))
	    (case type
	      ((character-data) 
	       (spark.expat::set-character-data-handler! handle handler))
	      ((comment)
	       (spark.expat::set-comment-handler! handle handler))
	      ((start-cdata-section)
	       (spark.expat::set-start-cdata-section-handler! handle handler))
	      ((end-cdata-section)
	       (spark.expat::set-start-cdata-section-handler! handle handler))
	      ((default)
	       (spark.expat::set-default-handler! handle handler))
	      ((external-entity-ref)
	       (spark.expat::set-external-entity-ref-handler! handle handler))
	      ((skipped-entity)
	       (spark.expat::set-skipped-entity-handler! handle handler))
	      ((start-namespace-decl)
	       (spark.expat::set-start-namespace-decl-handler! handle handler))
	      ((end-namespace-decl)
	       (spark.expat::set-end-namespace-decl-handler! handle handler))
	      ((xml-decl)
	       (spark.expat::set-xml-decl-handler! handle handler))
	      ((start-doctype-decl)
	       (spark.expat::set-start-doctype-decl-handler! handle handler))
	      ((end-doctype-decl)
	       (spark.expat::set-end-doctype-decl-handler! handle handler))
	      ;; ((element-decl)
	      ;; (spark.expat::set-element-decl-handler! handle handler))
	      ((attlist-decl)
	       (spark.expat::set-attlist-decl-handler! handle handler))
	      ((entity-decl)
	       (spark.expat::set-entity-decl-handler! handle handler))
	      ((unparsed-entity-decl)
	       (spark.expat::set-unparsed-entity-decl-handler! handle handler))
	      ((notation-decl)
	       (spark.expat::set-notation-decl-handler! handle handler))
	      ((not-standalone)
	       (spark.expat::set-not-standalone-handler! handle handler))
	      (else (raise-exception "set-special-handler!"
				     "Invalid handler type.")))))

	(define (xml-parser-user-data! self data)
	  (spark.expat::set-user-data! (xml-parser-s-handle self) data))

	;; Parses an XML document and executes the appropriate
	;; callbacks. xml should be null to denote the end of the
	;; document. Returns true ons succes, null on error.
	;; Call (last-error) to get a description of the last error.
	;; Takes a boolean optional argument, which if true will
	;; call (free) on this parser if xml is null.
	;; This argument is #t by default. So in most cases the user
	;; don't have to explixitly call (free), if he ended the parsing
	;; by passing null.
	(define (xml-parser-parse self xml . args)
	  (if (not (eqv? xml null))
	      (assert-string xml))
	  (spark.expat::parse (xml-parser-s-handle self) xml)
	  (if (eqv? xml null)
	      (begin
		(let ((call-free #t))
		  (if (not (eqv? args null))
		      (begin
			(set! call-free (car args))
			(assert-boolean call-free)))
		  (if call-free
		      (xml-parser-dispose self))))))

	(define (xml-parser-last-error self)
	  (spark.expat::last-error (xml-parser-s-handle self))))


