(import (airglow))

(define main-window (window 'w 220 'h 220 
			    'title "Clock"))
(define cl (clock 'w 220 'h 220))
(group-resizable main-window cl)
(group-finish main-window)
(window-xclass! main-window "Fl_Clock")
(window-show main-window)

(set! main-window (window 'x 20 'y 20 
			  'w 220 'h 220 
			  'title "Round-Clock"))
(set! cl (clock 'w 220 'h 220 'type 'round))
(group-resizable main-window cl)
(group-finish main-window)
(window-xclass! main-window "Fl_Clock")
(window-show main-window)

(airglow-run)
