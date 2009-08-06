(import (airglow))

(define (menu-cb f a)
  (collect-garbage)
  (let ((lsm (menu-last-selected f)))
    (print (menu-label lsm)) (newline)))

(define (quit-cb f a)
  (exit))

(define (about-cb f a)
  (print "Airglow menus sample.")
  (newline))

(define WIDTH 600)
(define main-window (window 'w 600 'h 400 
			    'title "Menus Sample"))
(define my-menu (menu 'w WIDTH 'h 30))

; (define menus-table #(#("&File" null 'submenu)
; 		      #(#("&New" "Ctrl+n" 'divider)
; 			#("&Quit" "Ctrl+q"))
; 		      #("&Build" null 'submenu)
; 		      #(#("&Configuration" null 'submenu)
; 			#(#("Debug" null 'radio)
; 			  #("Release" null 'radio)))
; 		      #(#("&Help" null 'submenu)
; 			#("&About..."))))

; (menu! my-menu menus-table)

(menu-add my-menu "&File" 
	  'flags 'submenu)
(menu-add my-menu "&File/&New" 
	  'shortcut "Ctrl+n" 
	  'flags 'divider)
(menu-add my-menu "&File/&Quit"
	  'shortcut "Ctrl+q")
(menu-add my-menu "&Build"
	  'flags 'submenu)
(menu-add my-menu "&Build/&Configuration"
	  'flags 'submenu)
(menu-add my-menu "&Build/&Configuration/&Release" 'flags 'radio)
(menu-add my-menu "&Build/&Configuration/&Debug" 'flags 'radio)
(menu-add my-menu "&Help"
	  'flags 'submenu)
(menu-add my-menu "&Help/&About..."
	  'callback about-cb)

(menu-callback! my-menu "&File/&New" menu-cb)
(menu-callback! my-menu "&File/&Quit" quit-cb)
(menu-callback! my-menu "&Help/&About..." about-cb)

(define popup-menu (menu 'w WIDTH 'h 400 'title "&popup" 'type 'popup3))
(menu-add popup-menu "&Edit" 
	  'flags 'submenu)
(menu-add popup-menu "&Edit/C&ut" 
	  'shortcut "Ctrl+x" 
	  'callback menu-cb)
(menu-add popup-menu "&Edit/&Copy" 
	  'shortcut "Ctrl+c" 
	  'callback menu-cb)
(menu-add popup-menu "&Edit/&Paste" 
	  'shortcut "Ctrl+v" 
	  'callback menu-cb)
(menu-add popup-menu "&View" 
	  'flags 'submenu)
(menu-add popup-menu "&View/&List" 
	  'shortcut "Ctrl+Alt+l" 
	  'callback menu-cb)
(menu-add popup-menu "&View/&Thumbnails" 
	  'shortcut "Ctrl+Alt+t" 
	  'callback menu-cb)

;(define pulldown-menu #(#("&Red" "Alt+r")
	;		#("&Green" "Alt+g")
 		;	#("&Blue" "Alt+b")))

(define (colors-cb m c)
  (widget-bg-color! pld-m c))

(define pld-m (menu 'x 100 'y 100 'w 120 'h 25 
		    'title "&Colors" 'type 'button))
(menu-add pld-m "&Red" 
	  'shortcut "Alt+r")
(menu-add pld-m "&Green"
	  'shortcut "Alt+g")
(menu-add pld-m "&Blue"
	  'shortcut "Alt+b")
; (menu! pld-m pulldown-menu)
(widget-tooltip! pld-m "This is a pulldown menu.")

(menu-callback! pld-m "&Red" colors-cb 'red)
(menu-callback! pld-m "&Green" colors-cb 'green)
(menu-callback! pld-m "&Blue" colors-cb 'blue)

(group-finish main-window)
(window-show main-window)
(airglow-run)
