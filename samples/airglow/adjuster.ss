(import (airglow))

(define main-window (window 'x 300 'y 400 
			    'w 369 'h 214 
			    'title "Adjustor" 
			    'type 'double))

(define (adjcb a b)
  (collect-garbage)
  (widget-label! b (valuator-display-format a))
  (widget-redraw a))

(define b1 (border 'x 20 'y 30 'w 80 'h 25 'type 'down))
(widget-bg-color! b1 'white)
(define a1 (valuator 'x (+ 20 80) 'y 30 
		     'w (* 3 25) 'h 25 
		     'type 'adjuster))
(widget-callback! a1 adjcb b1)
(adjcb a1 b1)

(define b2 (border 'x (+ 20 80 (* 4 25)) 
		   'y 30 'w 80 'h 25 'type 'down))
(widget-bg-color! b2 'white)
(define a2 (valuator 'x (+ (widget-x b2) (widget-width b2)) 
		     'y 10 'w 25 'h (* 3 25) 'type 'adjuster))
(widget-callback! a2 adjcb b2)
(adjcb a2 b2)

(group-resizable main-window main-window)
(group-finish main-window)
(window-show main-window)
(airglow-run)