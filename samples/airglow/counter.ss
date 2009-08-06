(import (airglow))

(define (count btn arg)
  (let ((c (string->number (widget-label btn))))
    (set! c (+ c 1))
    (widget-label! btn (number->string c)))
  (collect-garbage))

(define main-window (window 'x 300 'y 400 
			    'w 200 'h 100 
			    'title "Counter"))
(border 'x 50 'y 5 'w 100 'h 20 'title "Click the button to count.")
(define count-button (button 'x 50 'y 30 
			     'w 100 'h 50 
			     'title "0"))
(widget-callback! count-button count)
(collect-garbage)
(group-finish main-window)
(window-show main-window)
(airglow-run)
