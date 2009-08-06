(import (airglow))

(define w (window 'x 300 'y 400 
		  'w 369 'h 214 
		  'title "Valuators" 
		  'type 'double))

(define y 25)

(define v (valuator 'title "Slider" 'x 15 'y y 'w 20 'h 100 'type 'slider))
(widget-bg-color! v 'dark2)

(set! v (valuator 'title "Dial" 'x 55 'y y 'w 100 'h 100 'type 'dial))
(widget-bg-color! v 'green)

(set! v (valuator 'title "Roller" 'x 160 'y y 'w 50 'h 100 'type 'roller))
(widget-bg-color! v 'yellow)

(set! v (valuator 'title "Horizontal Nice Slider" 'x 240 'y y 'w 100 'h 30 'type 'slider))
(widget-bg-color! v 'dark-green)
(valuator-slider-type! v 'hor-nice)

(set! v (valuator 'title "Horizontal Fill Slider" 'x 240 'y (+ y 50) 'w 100 'h 30 'type 'slider))
(widget-bg-color! v 'dark-green)
(valuator-slider-type! v 'hor-fill)

(set! v (valuator 'title "Horizontal Slider" 'x 240 'y (+ y (* 50 2)) 'w 100 'h 30 'type 'slider))
(widget-bg-color! v 'dark-green)
(valuator-slider-type! v 'horizontal)

(group-finish w)
(window-show w)
(airglow-run)