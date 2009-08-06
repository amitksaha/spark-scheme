(import (airglow))

(define w (window 'x 300 'y 400 'w 400 'h 300 'title "Wizard" 'type 'double))

(define wiz (wizard 'w 400 'h 300))

;; page 1
(define g (group 'w 400 'h 300))
(define nb (button 'x 290 'y 265 'w 100 'h 25 'title "Next"))
(widget-callback! nb (lambda (b a) (wizard-next wiz)))
(border 'x 10 'y 30 'w (- 400 30) 'h 50 'title "First page")
(group-finish g)

;; page 2
(set! g (group 'w 400 'h 300))
(set! nb (button 'x 290 'y 265 'w 100 'h 25 'title "Next"))
(define bb (button 'x 180 'y 265 'w 100 'h 25 'title "Back"))
(widget-callback! nb (lambda (b a) (wizard-next wiz)))
(widget-callback! bb (lambda (b a) (wizard-prev wiz)))
(border 'x 10 'y 30 'w (- 400 30) 'h 50 'title "Second page")
(group-finish g)

;; page 3
(set! g (group 'w 400 'h 300))
(set! nb (button 'x 290 'y 265 'w 100 'h 25 'title "Finish"))
(set! bb (button 'x 180 'y 265 'w 100 'h 25 'title "Back"))
(widget-callback! nb (lambda (b a) (exit)))
(widget-callback! bb (lambda (b a) (wizard-prev wiz)))
(border 'x 10 'y 30 'w (- 400 30) 'h 50 'title "Last page")
(group-finish g)

(group-finish w)
(window-show w)
(airglow-run)