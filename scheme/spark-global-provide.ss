;; Exports all MzScheme special forms and spark extentions.
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

(module spark
	mzscheme

	(require spark-lang-ext)
	(require spark-util-ext)
	(require spark-string-ext)
	(require spark-list-ext)
	(require spark-numeric-ext)
	;;(require spark-hash-map)
	(require spark-stack)
	(require spark-async)
	(require spark-process)
	
	(provide (all-from mzscheme)
		 (all-from spark-lang-ext)
		 (all-from spark-util-ext)
		 (all-from spark-list-ext)
		 (all-from spark-string-ext)
		 (all-from spark-numeric-ext)
		 ;;(all-from spark-hash-map)
		 (all-from spark-stack)
		 (all-from spark-async)
		 (all-from spark-process)))

;; spark extensions
	 
(begin
  (namespace-require/copy 'spark)
  (require-for-syntax spark))

(current-module-name-resolver 
 ((dynamic-require '#%misc 'make-standard-module-name-resolver) (current-namespace)))

