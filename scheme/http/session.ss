;; HTTP session.
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

(library http-session
	 
	 (export session-execute-procedure
		 session-destroy
		 session-last-access)

	 (import (http-globals))

	 (define-struct session-s (id 
				   url 
				   state
				   last-access))
	 (define *session-id* 0)

	 (define *vars-sep* "?")

	 (define (session-last-access session)
	   (session-s-last-access session))

	 (define (session-create script-url sessions) 
	   (let* ((sess-id (next-session-id))
		  (sess (make-session-s sess-id
					(make-session-url script-url sess-id 0)
					(make-hash-table 'equal)
					(current-seconds)))
		  (id (session-s-id sess)))
	     (atomic
	      (hash-table-put! sessions id sess))
	     id))

	 (define (session-destroy id sessions)
	   (atomic
	    (hash-table-remove! sessions id)))
	 
	 (define (session-execute-procedure url procs 
					    sess-id
					    p-count
					    state-to-add
					    sessions)
	   (let* ((sess (find-session sess-id url sessions))
		  (id (session-s-id sess))
		  (procs-len (length procs)))
	     (let ((proc-count (add1 p-count)))
	       (let ((state (session-s-state sess)) 
		     (res-html null))
		 (hash-table-map state-to-add 
				 (lambda (k v) (hash-table-put! state k v)))

		 (set! res-html ((list-ref procs (sub1 proc-count))
				 (make-session-url url id proc-count) 
				 state))
		 (if (>= proc-count procs-len)
		     (cons 'done res-html))
		 (cons 'next res-html)))))

	 (define (next-session-id)
	   (let ((id 0))
	     (atomic "*session-id*" 
		     (begin
		       (set! *session-id* (add1 *session-id*))
		       (set! id *session-id*)))
	     id))

	 (define (find-session id url sessions)
	   (if (= id -1) 
	       (set! id (session-create url sessions)))
	   (let ((sess (hash-table-get sessions id null)))
	     (if (null? sess) 
		 (raise "null session")
		 (begin
		  (set-session-s-last-access! sess (current-seconds))
		  sess))))

	 (define (make-session-url url sess-id proc-count)
	   (let ((out (open-output-string)))
	     ;; TODO: need to find a way to recycle old sessions-ids.
	     (set! url (normalize-url url))
	     (fprintf out "~a~a.~a~a.ss" 
		      *sess-id-sep* sess-id proc-count *sess-id-sep*)
	     (string-append url (get-output-string out))))

	 (define (find-session-id url)
	   (let ((idx (string-find url *sess-id-sep*)))
	     (if (= idx -1) -1
		 (let ((end-idx (string-find url (+ idx 1) *sess-id-sep*)))
		   (if (> idx 0)
		       (string->number (substring url (+ idx 1) end-idx))
		       -1)))))

	 (define (normalize-url url)
	   (let ((idx (string-rfind url "/")))
	     (if (= idx -1) url
		 (substring url (add1 idx)))))

	 (define (remove-vars url)
	   (let ((idx (string-find url *vars-sep*)))
	     (if (>= idx 0) (substring url 0 idx) url))))
