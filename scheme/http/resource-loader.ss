;; Loads resources to be send to HTTP clients.
;; Copyright (C) 2007, 2008, 2009 Vijay Mathew Pandyalakal
 
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

;;(load "session.ss")

(library http-resource-loader

	 (export resource-create
		 resource-load)

	 (import (http-session) (http-globals))

	 (define-struct resource-loader-s (cache))
	 
	 (define (resource-create)
	   (make-resource-loader-s (make-hash-table 'equal)))
	 
	 (define (resource-load res-loader web-server-conf
				uri 
				http-data session)
	   (let* ((uri-data (parse-uri uri))
		  (root-uri (list-ref uri-data 0))
		  (sess-info (list-ref uri-data 1)))
	     (let* ((type (find-res-type root-uri
					 (hash-table-get web-server-conf 'script-ext)))
		    (res (find-resource-data res-loader root-uri)))
	       (if (null? res)
		   (set! res (read-fresh res-loader root-uri type)))
	       (if (eq? type 'script)
		   (let ((ids (parse-session-info sess-info)))
		     (execute-resource res root-uri 
				     (list-ref ids 0)
				     (list-ref ids 1)
				     http-data session))
		   res))))

	 (define (find-resource-data res-loader uri)
	   (let ((res (hash-table-get (resource-loader-s-cache res-loader) uri null)))
	     (if (not (null? res))
		 (if (resource-modified? res) null res)
		 res)))

	 (define (resource-modified? res)
	   (let* ((last-modified (car res))
		  (curr-modified (file-or-directory-modify-seconds (cdr res)))
		  (res-modified (not (= last-modified curr-modified))))
	     res-modified))

	 (define (read-fresh res-loader uri type)
	   (case type
	     ((file) (read-fresh-file res-loader uri))
	     ((script) (read-fresh-script res-loader uri))))

	 (define (read-fresh-file res-loader uri)
	   (let* ((sz (file-size uri))
		  (file (open-input-file uri))
		  (res (cons (file-or-directory-modify-seconds uri)
			     (read-bytes sz file)))
		  (cache (resource-loader-s-cache res-loader)))
	     (atomic
	      (hash-table-put! cache uri res))
	     res))
	 
	 (define (read-fresh-script res-loader uri)
	   (let ((res (cons (file-or-directory-modify-seconds uri)
			    (load uri)))
		 (cache (resource-loader-s-cache res-loader)))
	     (atomic
	      (hash-table-put! cache uri res))
	     res))

	 (define (find-res-type uri script-ext)
	   (let ((ext (filename-extension uri)))
	     (if (bytes=? ext script-ext)
		 'script
		 'file)))

	 (define (parse-uri uri)
	   (let ((idx (string-find uri *sess-id-sep*)))
	     (if (= idx -1) 
		 (list uri null)
		 (list (substring uri 0 idx) (find-session-info uri (add1 idx))))))

	 (define (find-session-info uri start-idx)
	   (let ((idx (string-find uri *sess-id-sep* start-idx)))
	     (if (= idx -1) null
		 (substring uri start-idx idx))))

	 (define (parse-session-info sess-info)
	   (if (null? sess-info) (list -1 0)	       
	       (let ((idx (string-find sess-info ".")))
		 (if (= idx -1) (list -1 0)
		     (let ((num1 (string->number (substring sess-info 0 idx)))
		       (num2 (string->number (substring sess-info (add1 idx)))))
		       (list num1 num2))))))

	 (define (execute-resource res uri 
				   sess-id proc-count 
				   http-data session)
	   (let ((content 
		  (session-execute-procedure 
		   uri (cdr res) sess-id 
		   proc-count http-data session)))
	     (cons (car res) (cdr content)))))


