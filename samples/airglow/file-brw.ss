(import (airglow))

(define w (window 'w 400 'h 400 'title "File Browser"))

(define b (browser 'x 5 'y 5 'w 350 'h 350 'title "Files" 'type 'file))
(browser-load-directory b ".")

(group-finish w)
(window-show w)
(airglow-run)