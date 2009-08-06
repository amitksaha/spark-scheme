;; Some procedures to ease raising exceptions.
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

(library exception

	 (import ((prefix spark.sysinfo:: #%spark-sysinfo)))

	 (export raise-exception 
		 raise-sys-exception)

	 ;; raises a general error.
	 (define (raise-exception method message type)
	   (define out (open-output-string))
	   (fprintf out "~a: error: ~a" method message)
	   (case type
	     ((contract)
	      (raise (make-exn:fail:contract (get-output-string out) 
					     (current-continuation-marks))))
	     (else (raise (make-exn:fail (get-output-string out) 
					 (current-continuation-marks))))))
	 

	 (define (raise-sys-exception method)
	   (define out (open-output-string))
	   (fprintf out "~a: system-error: ~a" method 
		    (spark.sysinfo::last-strerror))
	   (raise (make-exn:fail (get-output-string out) 
				 (current-continuation-marks)))))


