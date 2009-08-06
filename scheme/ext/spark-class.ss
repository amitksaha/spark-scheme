;; Simple wrapper for MzScheme class. 
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

(module class mzscheme

	(require (lib "class.ss"))
	
	(provide class class* class/derived
		 define-serializable-class define-serializable-class*
		 class?
		 mixin
		 interface interface?
		 object% object? externalizable<%>
		 object=?
		 new make-object instantiate
		 send send/apply send* class-field-accessor class-field-mutator with-method
		 get-field field-bound? field-names
		 private* public*  pubment*
		 override* overment*
		 augride* augment*
		 public-final* override-final* augment-final*
		 define/private define/public define/pubment
		 define/override define/overment
		 define/augride define/augment
		 define/public-final define/override-final define/augment-final
		 define-local-member-name define-member-name 
		 member-name-key generate-member-key 
		 member-name-key? member-name-key=? member-name-key-hash-code
		 generic make-generic send-generic
		 is-a? subclass? implementation? interface-extension?
		 object-interface object-info object->vector
		 object-method-arity-includes?
		 method-in-interface? interface->method-names class->interface class-info
		 (struct exn:fail:object ())
		 make-primitive-class

		 ;; "keywords":
		 private public override augment
		 pubment overment augride
		 public-final override-final augment-final
		 field init init-field
		 rename-super rename-inner inherit inherit/super inherit/inner inherit-field
		 this super inner
		 super-make-object super-instantiate super-new
		 inspect))