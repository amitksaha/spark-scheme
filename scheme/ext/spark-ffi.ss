;; Simple wrapper for MzScheme ffi module. 
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

(module ffi mzscheme

	(require (lib "foreign.ss"))
	
	(provide ffi-lib ffi-lib?
		 ffi-obj-ref
		 make-ctype ctype?
		 ctype-sizeof ctype-alignof
		 compiler-sizeof
		 unsafe!
		 ;; types
		 _int8 _sint8 _uint8 _int16 
		 _sint16 _uint16 _int32 _sint32 
		 _uint32 _int64 _sint64 _uint64
		 _byte _ubyte _sbyte
		 _word _uword _sword
		 _ushort _short
		 _int _uint _sint
		 _long _ulong _slong
		 _float _double _double*
		 _bool _void
		 ;; string types
		 _bytes _string/ucs-4
		 _string/utf-16
		 _string/utf-8 _string/locale
		 _string/latin-1 
		 _string*/utf-8 _string*/locale
		 _string*/latin-1
		 _string default-_string-type
		 _file _bytes/eof _string/eof
		 _path _symbol
		 ;; pointer types
		 _pointer _scheme _fpointer
		 ;; function types
		 _cprocedure _fun
		 define-fun-syntax
		 _ptr _box _list _vector
		 ;; c-struct types
		 make-cstruct-type
		 _list-struct define-cstruct
		 _enum _bitmask
		 cpointer?
		 ptr-equal? ptr-add
		 ;; memory management
		 end-stubborn-change 
		 register-finalizer make-sized-byte-string
		 _cpointer/null
		 ;; safe c vectors
		 define-cpointer-type make-cvector
		 cvector? cvector cvector-length
		 cvector-type cvector-ref cvector-set!
		 cvector->list list->cvector 
		 _cvector
		 list->cblock
		 vector->cblock))
		 
		 

		 