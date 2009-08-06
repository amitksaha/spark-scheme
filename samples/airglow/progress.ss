(import (airglow))

(define called #f)

(define count-button null)
(define pbar null)

(define (thread-cb)
  (let ((max 10) (v 0))
    (let loop ()
      (if (< v max)
	  (begin
	    (set! v (add1 v))
	    (progress-value! pbar v)
	    (sleep 1)
	    (loop))))
    (set! called #f)
    (airglow-thread-manager-stop)))

(define (button-cb btn arg)
  (if (not called)
      (begin
	(progress-value! pbar 0)
	(airglow-thread-manager-start)
	(thread thread-cb)
	(set! called #t))))

(define main-window (window 'x 300 'y 400 'w 250 'h 160 'title "Progress"))
(define count-button (button 'x 5 'y 30 'w 100 'h 50 'title "Start"))
(widget-callback! count-button button-cb)
(set! pbar (progress 'x 5 'y 90 'w 250 'h 30))
(progress-minimum! pbar 0)
(progress-maximum! pbar 10)
(widget-selection-color! pbar 'blue)
(widget-border! pbar 'plastic-down)
(group-finish main-window)
(window-show main-window)
(airglow-run)
