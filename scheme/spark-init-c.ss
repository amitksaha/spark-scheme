;; Loads all compiled spark Scheme modules.
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

(load-relative "compiled/spark-global.zo")
(load-relative "compiled/spark-lib-manager.zo")
(load-relative "ext/compiled/spark-lang-ext.zo")
(load-relative "ext/compiled/spark-util-ext.zo")
(load-relative "ext/compiled/spark-string-ext.zo")
(load-relative "ext/compiled/spark-numeric-ext.zo")
(load-relative "ext/compiled/spark-list-ext.zo")
;;(load-relative "ext/compiled/spark-hash-map.zo")
(load-relative "ext/compiled/spark-stack.zo")
(load-relative "ext/compiled/spark-async.zo")
(load-relative "ext/compiled/spark-process.zo")
(load-relative "compiled/spark-global-provide.zo")

(require library-manager)
(load-compiled! #t)

(load-relative "common/compiled/exception.zo")
(load-relative "common/compiled/asserts.zo")
(load-relative "common/compiled/util.zo")

