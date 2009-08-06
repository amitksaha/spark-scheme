(import (airglow))

(define (sender-events w e d)
  (case e
    ((mouse-push)
     (dnd "Message"))))

(define (receiver-events w e d)
  (case e
    ((paste)
     (widget-label! w (event-text)))))

(define w (window 'w 150 'h 80 'title "Dnd"))

(define s (widget 'super 'border 'x 5 'y 5 'w 70 'h 50 'title "Sender" 'events sender-events))
(widget-border! s 'up)
(widget-bg-color! s 'red)

(define r (widget 'super 'border 'x 80 'y 5 'w 70 'h 50 'title "Receiver" 'events receiver-events))
(widget-border! r 'up)
(widget-bg-color! r 'green)

(group-finish w)
(window-show w)
(airglow-run)