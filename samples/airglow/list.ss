(import (airglow))

(define w (window 'h 300 'w 200 'title "ListView"))

(define b (browser 'x 25 'y 5 'w 150 'h 250 'title "Colors"))
(browser-type! b 'multi)

;; add items
(define colors '("Red" "Green" "Blue" "White" "Black" "Magenta"))
(while (not (null? colors))
       (browser-add b (car colors))
       (set! colors (cdr colors)))

(group-finish w)
(window-show w)
(airglow-run)
