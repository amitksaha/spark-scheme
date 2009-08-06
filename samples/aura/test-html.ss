(import (aura))

(define s (css '.divclass null))
((s 'prop!) 'color "mediumseagreen")
((s 'prop!) 'width "500px")
((s 'prop!) 'height "500px")
((s 'text))

(define msg "hello, world")

(define html-doc (sgml `(html 
			 (head 
			  (title "Test01")
			  (style ,s))
			 (body 
			  (div ((class "divclass")
				(id "messageDiv"))
			       (b ,msg))))))
(printf "~a~n" ((html-doc 'text)))

;; Complex, low-level way to generate html.
; (define html-doc (<> 'html ()
; 		     (<> 'head ()
; 			 (<> 'title () "Test01")
; 			 (<> 'style () s))
; 		     (<> 'body ()
; 			 (<> 'div 
; 			     (: ('class "divclass")
; 				('id "messageDiv"))
; 			     (<> 'b () "hello, world")
; 			     (<> 'br ())
; 			     (<> 'i () "bye.")))))
; (printf "~a~n" ((html-doc 'text)))

