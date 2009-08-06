;; Loads all spark Scheme modules.
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

(define spark-home (getenv "SPARK_LIBPATH"))
(if (not (string? spark-home))
    (begin
      (printf "The environment variable SPARK_LIBPATH is not set.")
      (newline)
      (exit)))
(define collections-path (string-append spark-home "/collects"))

(current-library-collection-paths (list (string->path collections-path)))
(set! current-load-relative-directory spark-home)

(load-relative "spark-global.ss")
(load-relative "spark-lib-manager.ss")
(load-relative "ext/spark-lang-ext.ss")
(load-relative "ext/spark-util-ext.ss")
(load-relative "ext/spark-string-ext.ss")
(load-relative "ext/spark-numeric-ext.ss")
(load-relative "ext/spark-list-ext.ss")
;;(load-relative "ext/spark-hash-map.ss")
(load-relative "ext/spark-stack.ss")
(load-relative "ext/spark-async.ss")
(load-relative "ext/spark-process.ss")
(load-relative "spark-global-provide.ss")

(require library-manager)
(load-compiled! #f)

(load-relative "common/exception.ss")
(load-relative "common/asserts.ss")
(load-relative "common/util.ss")

