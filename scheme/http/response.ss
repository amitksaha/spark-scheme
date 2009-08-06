;; HTTP response wrapper.
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

(library http-response
	 
	 (export make-response make-error-response)
	 
	 (import (mime-types))

	 (define (make-response resource-uri version 
				content-length content-mod) 
	   (let ((out (open-output-string)))
	     (fprintf out "~a 200 OK~a" 
		      version 
		      (crlf))
	     (fprintf out "Date: ~a~a" 
		      (date-str (gmt-date (current-seconds))) 
		      (crlf))
	     (fprintf out "Content-Type: ~a~a" 
		      (content-type resource-uri)
		      (crlf))
	     (fprintf out "Content-Length: ~a~a" 
		      content-length
		      (crlf))
	     (fprintf out "Last-Modified: ~a~a"
		      (date-str (gmt-date content-mod))
		      (crlf))
	     (fprintf out "~a" (crlf))
	     (flush-output out)
	     (get-output-string out)))

	 (define (make-error-response error-message 
				      error-code 
				      version)
	   (let ((out (open-output-string)))
	     (fprintf out "~a ~a ~a ~a ~a" 
		      version
		      error-code
		      (error->string error-code)
		      error-message
		      (crlf))
	     (get-output-string out)))

	 (define (crlf) (string #\return #\linefeed))

	 (define (date-str d) 
	   (let ((out (open-output-string)))
	     (fprintf out "~a, ~a ~a ~a ~a:~a:~a GMT"
		      (week-day->string (date-week-day d))
		      (date-day d)
		      (month->string (date-month d))
		      (date-year d)
		      (date-hour d)
		      (date-minute d)
		      (date-second d))
	     (get-output-string out)))

	 (define (gmt-date secs)
	   (let ((d (seconds->date secs)))
	     (seconds->date (- secs (date-time-zone-offset d)))))

	 (define (content-type uri) 
	   (let ((mt (find-mime-type uri)))
	     (if (not mt) "text/html"
		 (cdr mt))))

	 (define (week-day->string wd)
	   (case wd
	     ((0) "Sun")
	     ((1) "Mon")
	     ((2) "Tue")
	     ((3) "Wed")
	     ((4) "Thu")
	     ((5) "Fri")
	     (else "Sat")))

	 (define (month->string mon)
	   (case mon
	     ((1) "Jan")
	     ((2) "Feb")
	     ((3) "Mar")
	     ((4) "Apr")
	     ((5) "May")
	     ((6) "Jun")
	     ((7) "Jul")
	     ((8) "Aug")
	     ((9) "Sep")
	     ((10) "Oct")
	     ((11) "Nov")
	     (else "Dec")))

	 (define (error->string error-code)
	   (case error-code
	     ((200) "OK")
	     ((401) "Not Found")
	     ((301) "Moved Permanently")
	     ((302) "Moved Temporarily")
	     ((303) "See Other")
	     (else "Server Error"))))

	     