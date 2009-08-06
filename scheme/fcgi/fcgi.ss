;; The Spark FastCGI API.
;; Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

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

(library fcgi
	 
	 (import ((prefix spark.fcgi:: #%spark-fcgi)))

	 (import (net-socket) (util))

	 (export fcgi-recv fcgi-send
		 fcgi-request-id
		 fcgi-request-role
		 fcgi-request-params
		 fcgi-request-keep-connection?
		 fcgi-request-aborted?
		 fcgi-request->string
		 fcgi-request-param-value
		 fcgi-request-parameter
		 fcgi-request-method
		 fcgi-request-post-data
		 fcgi-request-data)

	 (define-struct fcgi-request-s (handle 
					id role params
					keep-connection 
					aborted
					post-data
					data))

	 (define (fcgi-recv sock)
	   (let ((req (spark.fcgi::recv (socket-handle sock))))
	     (cond
	      ((null? req)
	       (error "fcgi-recv failed. null request object."))
	      ((eq? req 'error)
	       (let ((out (open-output-string)))
		 (fprintf out "fcgi-recv failed. ~a" (spark.fcgi::get-last-error))
		 (error (get-output-string out))))
	      (else
	       (make-fcgi-request-s 
		req
		(spark.fcgi::fcgi-request-id req)
		(spark.fcgi::fcgi-request-role req)
		(make-request-params req)
		(spark.fcgi::fcgi-request-keep-connection req)
		(spark.fcgi::fcgi-request-aborted req)
		(spark.fcgi::fcgi-request-stdin-stream req)
		(spark.fcgi::fcgi-request-data-stream req))))))

	 (define (fcgi-send req resp . args)
	   (let ((status 'complete)
		 (err #f)
		 (content-type "text/html")
		 (i 0))
	     (for a in args
		  (case i
		    ((0) (set! status a))
		    ((1) (set! err a))
		    ((2) (set! content-type a))
		    (else (error "Invalid number of arguments.")))
		  (set! i (add1 i)))
	     (let ((r (spark.fcgi::send (fcgi-request-s-handle req)
					resp
					(status->integer status)
					err
					content-type)))
	       (cond
		((null? r)
		 (error "fcgi-send failed with null result."))
		((eq? r 'error)
		 (let ((out (open-output-string)))
		   (fprintf out "fcgi-send failed. ~a" (spark.fcgi::get-last-error))
		   (error (get-output-string out))))
		(else
		 r)))))

	 (define (fcgi-request-id req)
	   (fcgi-request-s-id req))

	 (define (fcgi-request-role req)
	   (role->symbol (fcgi-request-s-role req)))

	 (define (fcgi-request-params req)
	   (fcgi-request-s-params req))

	 (define (fcgi-request-keep-connection? req)
	   (fcgi-request-s-keep-connection req))

	 (define (fcgi-request-aborted? req)
	   (fcgi-request-s-aborted req))

	 (define (fcgi-request->string req)
	   (let ((out (open-output-string)))
	     (fprintf out 
		      "id ~a~nrole ~a~nparams ~a~nkeep-connection? 
~a~naborted? ~a~npost-data ~a~ndata ~a~n"
		      (fcgi-request-id req)
		      (fcgi-request-role req)
		      (fcgi-request-params req)
		      (fcgi-request-keep-connection? req)
		      (fcgi-request-aborted? req)
		      (fcgi-request-post-data req)
		      (fcgi-request-data req))
	     (get-output-string out)))

	 (define (fcgi-request-param-value req param)
	   (if (symbol? param)
	       (set! param (symbol->string param)))
	   (cdr (assoc param (fcgi-request-params req))))

	 (define (fcgi-request-parameter req query-param)
	   (if (symbol? query-param)
	       (set! query-param (symbol->string query-param)))
	   (let ((method (fcgi-request-method req))
		 (query null) (params null))
	     (case method
	       ((GET) (set! query (fcgi-request-param-value req "QUERY_STRING")))
	       (else (set! query (fcgi-request-post-data req))))
	     (set! params (parse-query-string query))
	     (if (not (null? params))
		 (cdr (assoc query-param params))
		 "")))

	 (define (fcgi-request-post-data req)
	   (fcgi-request-s-post-data req))

	 (define (fcgi-request-data req)
	   (fcgi-request-s-data req))

	 (define (fcgi-request-method req)
	   (let ((m (string-upcase (fcgi-request-param-value req "REQUEST_METHOD"))))
	     (string->symbol m)))
 
	 (define (make-request-params req)
	   (let ((params (list))
		 (keys (spark.fcgi::fcgi-request-param-keys req))
		 (key null))
	     (while (not (null? keys))
		    (set! key (car keys))
		    (set! params (append params
					 (list 
					  (cons key 
						(spark.fcgi::fcgi-request-param-value req key)))))
		    (set! keys (cdr keys)))
	     params))

	 (define (status->integer s)
	   (if (integer? s)
	       s
	       (begin
		 (case s
		   ((complete)
		    spark.fcgi::FCGI-REQUEST-COMPLETE)
		   ((cant-mpx-conn)
		    spark.fcgi::FCGI-CANT-MPX-CONN)
		   ((overloaded)
		    spark.fcgi::FCGI-OVERLOADED)
		   ((unknown-role)
		    spark.fcgi::FCGI-UNKNOWN-ROLE)
		   (else
		    (error "Invalid request status."))))))
	 
	 (define (role->symbol r)
	   (case r
	     ((spark.fcgi::FCGI-RESPONDER)
	      'responder)
	     ((spark.fcgi::FCGI-FILTER)
	      'filter)
	     ((spark.fcgi::FCGI-AUTHORIZER)
	      'authorizer)
	     (else
	      'unknown))))
