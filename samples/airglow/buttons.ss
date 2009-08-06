(import (airglow))

(define main-window (window 'x 300 'y 400 
			    'w 369 'h 214 
			    'title "Buttons" 
			    'type 'double))
(button 'x 20 'y 10 'w 160 'h 30 'title "Button")
(button 'x 20 'y 50 'w 160 'h 30 'title "Return_Button" 'type 'return)
(button 'x 20 'y 90 'w 160 'h 30 'title "Light_Button" 'type 'light)
(button-down-border! (button 'x 20 'y 170 'w 160 'h 30 'title "Round_Button" 'type'round)
		     'round-down)

(define g01 (group 'x 190 'y 10 'w 70 'h 120))
(widget-border! g01 'thin-up-frame)
(define count 0)

(let loop ((p 10))
  (button-down-border! 
   (button 'x 190 'y p 'w 70 'h 30 'title "radio" 'type 'radio) 'round-down)
  (set! p (+ p 30))
  (set! count (+ count 1))
  (if (not (= count 4))
      (loop p)))

(group-finish g01)

(define g02 (group 'x 270 'y 10 'w 90 'h 115))
(widget-border! g01 'thin-up)
(set! count 0)
(define b null)
(let loop ((pos 20))
  (set! b (button 'x 280 'y pos 'w 20 'h 20 'title "radio"))
  (button-type! b 'radio)
  (widget-selection-color! b 'red)
  (widget-label-align! b 'right)
  (set! pos (+ pos 25))
  (set! count (+ count 1))
  (if (not (= count 4))
      (loop pos)))
(group-finish g02)

(group-finish main-window)
(window-show main-window)
(airglow-run)