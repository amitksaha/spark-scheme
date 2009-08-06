(import (airglow))

(define main-window (window 'x 300 'y 400 
			    'w 130 'h 100 
			    'title "Spark" 
			    'type 'double))
(border 'x 10 'y 20 'w 100 'h 50 'title "hello, world" 'type 'up-frame)

(group-finish main-window)
(window-show main-window)
(airglow-run)

