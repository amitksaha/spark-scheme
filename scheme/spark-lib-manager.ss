;; Library manager. 
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

(module library-manager mzscheme
	
	(define (find-module-name v)
	  (let ((c (car v)))
	    (case c
	      ((prefix) (list-ref v 2))
	      ((only) (list-ref v 1))
	      ((all-except) (list-ref v 1))
	      ((prefix-all-except) (list-ref v 2))
	      ((rename) (list-ref v 1))
	      (else c))))

	(define loaded-libraries (make-hash-table 'equal))

	(define spark-home (getenv "SPARK_LIBPATH"))
	(define folder "")
	(define ext ".ss")
	(define sema (make-semaphore 1))

	(define (load-compiled! f)
	  (if (boolean? f)
	      (begin
		(if f
		    (begin
		      (set! folder "compiled/")
		      (set! ext ".zo"))
		    (begin
		      (set! folder "")
		      (set! ext ".ss"))))))		

	(define (load-net-libraries)
	  (load (string-append spark-home "/scheme/" "net/" folder "address" ext))
	  (load (string-append spark-home "/scheme/" "net/" folder "socket" ext))
	  (load (string-append spark-home "/scheme/" "net/" folder "net" ext)))

	(define (load-http-libraries)
	  (load (string-append spark-home "/scheme/" "http/" folder "url-encode" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "globals" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "session" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "request-parser" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "resource-loader" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "mime-types" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "response" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "web-server" ext))
	  (load (string-append spark-home "/scheme/" "http/" folder "http" ext)))

	(define (load-net-curl-libraries)
	  (load (string-append spark-home "/scheme/" "net/" folder "curl" ext)))

	(define (load-reactor-libraries)
	  (load (string-append spark-home "/scheme/" "reactor/" folder "handler" ext))
	  (load (string-append spark-home "/scheme/" "reactor/" folder "select-reactor" ext))
	  (load (string-append spark-home "/scheme/" "reactor/" folder "socket-acceptor" ext))
	  (load (string-append spark-home "/scheme/" "reactor/" folder "socket-connector" ext))
	  (load (string-append spark-home "/scheme/" "reactor/" folder "reactor" ext)))

	(define (load-sqlite-libraries)
	  (load (string-append spark-home "/scheme/" "sql/sqlite/" folder "db" ext)))

	(define (load-odbc-libraries)
	  (load (string-append spark-home "/scheme/" "sql/odbc/" folder "db" ext)))

	(define (load-xml-libraries)
	  (load (string-append spark-home "/scheme/" "xml/" folder "xml-parser" ext)))

	(define (load-airglow-libraries)
	  (load (string-append spark-home "/scheme/" "airglow/" folder "util" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "image" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "airglow-base" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "widget" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "group" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "window" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "button" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "border" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "browser" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "pack" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "input-field" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "color-chooser" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "menu" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "chart" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "scroll" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "progress" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "tabs" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "wizard" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "text-buffer" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "text-editor" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "input-choice" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "file-chooser" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "help-view" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "help-dialog" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "event" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "dnd" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "graphics" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "valuator" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "clock" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "ask" ext))
	  (load (string-append spark-home "/scheme/" "airglow/" folder "airglow" ext)))

	(define (load-airglow-3d-libraries)
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "util" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "rendering" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "transform" ext))
	  (load (string-append spark-home "/scheme/" "airglow/3d/" folder "geometry" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "shaders" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "extras" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "camera" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "texturing" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "lighting" ext))
	  (load (string-append spark-home "/scheme/"  "airglow/3d/" folder "airglow-3d" ext)))

	(define (load-sunit-libraries)
	  (load (string-append spark-home "/scheme/"  "sunit/" folder "sunit" ext)))

	(define (load-pregexp-libraries)
	  (load (string-append spark-home "/scheme/"  "ext/" folder "spark-pregexp" ext)))

	(define (load-match-libraries)
	  (load (string-append spark-home "/scheme/"  "ext/" folder "spark-match" ext)))

	(define (load-compile-libraries)
	  (load (string-append spark-home "/scheme/"  "ext/" folder "spark-compile" ext)))

	(define (load-class-libraries)
	  (load (string-append spark-home "/scheme/"  "ext/" folder "spark-class" ext)))

	(define (load-ffi-libraries)
	  (load (string-append spark-home "/scheme/"  "ext/" folder "spark-ffi" ext)))

	(define (load-fcgi-libraries)
	  (load (string-append spark-home "/scheme/"  "fcgi/" folder "fcgi" ext)))

	(define (load-aura-libraries)
	  (load (string-append spark-home "/scheme/"  "aura/" folder "sgml" ext))
	  (load (string-append spark-home "/scheme/"  "aura/" folder "aura" ext)))

	(define (load-new-library lib-name)
	  (let ((loaded (hash-table-get loaded-libraries lib-name #f))
		(put-in-store #t))
	    (if (not loaded)
		(begin
		  (case lib-name
		    ((net) (load-net-libraries))
		    ((net-curl) (load-net-curl-libraries))
		    ((reactor) (load-reactor-libraries))
		    ((sql-sqlite) (load-sqlite-libraries))
		    ((sql-odbc) (load-odbc-libraries))
		    ((xml) (load-xml-libraries))
		    ((airglow) (load-airglow-libraries))
		    ((airglow-3d) (load-airglow-3d-libraries))
		    ((sunit) (load-sunit-libraries))
		    ((pregexp) (load-pregexp-libraries))
		    ((compile) (load-compile-libraries))
		    ((class) (load-class-libraries))
		    ((ffi) (load-ffi-libraries))
		    ((fcgi) (load-fcgi-libraries))
		    ((aura) (load-aura-libraries))
		    ((http) (load-http-libraries))
		    ((match) (load-match-libraries))
		    (else (set! put-in-store #f)))
		  (if put-in-store
		      (hash-table-put! loaded-libraries lib-name #t))))))

	(define (load-library . args)
	  (semaphore-wait sema)
	  (let loop ()
	    (if (not (null? args))
		(begin
		  (let ((v (car args)))
		    (if (list? v)
			(set! v (find-module-name v)))
		    (if (symbol? v)
			(begin
			  (load-new-library v))))
		  (set! args (cdr args))
		  (loop))))
	  (semaphore-post sema))

	(provide load-library
		 load-compiled!))