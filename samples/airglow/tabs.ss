(import (airglow))

(define w (window 'x 300 'y 400 'w 500 'h 200 'title "Tabs" 'type 'double))

(define tabs (tabs 'x 10 'y 10 'w (- 500 20) 'h (- 200 20)))
(define aaa (group 'x 10 'y 35 'w (- 500 20) 'h (- 200 45) 'title "Aaa"))
(define b (button 'x 50 'y 60 'w 90 'h 25 'title "Button A1"))
(widget-bg-color! b 89)
(set! b (button 'x 50 'y 90 'w 90 'h 25 'title "Button A2"))
(widget-bg-color! b 90)
(set! b (button 'x 50 'y 120 'w 90 'h 25 'title "Button A3"))
(widget-bg-color! b 91)
(group-finish aaa)

(define bbb (group 'x 10 'y 35 'w (- 500 10) 'h (- 200 35) 'title "Bbb"))
(define b (button 'x 50 'y 60 'w 90 'h 25 'title "Button B1"))
(widget-bg-color! b 92)
(set! b (button 'x 150 'y 60 'w 90 'h 25 'title "Button B2"))
(widget-bg-color! b 93)
(set! b (button 'x 250 'y 60 'w 90 'h 25 'title "Button B3"))
(widget-bg-color! b 94)
(group-finish bbb)

(group-finish tabs)

(group-finish w)
(window-show w)
(airglow-run)