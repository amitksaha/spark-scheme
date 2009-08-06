(import (airglow))

(define WIDTH 600)
(define window (window 'w 600 'h 400 
		       'title "Scrolls Sample"))

(define scroll (scroll 'w 300 'h 400 'title "Scroll view" 'type 'both-always))
(define i 0)
(define x 0)
(let loop ()
  (button 'x x 'y 5 'w 100 'h 20 'title "hello")
  (set! x (+ x 110))
  (set! i (+ i 1))
  (if (< i 100)
      (loop)))
(group-finish scroll)

(group-finish window)
(window-show window)
(airglow-run)
