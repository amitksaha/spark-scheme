(import (airglow))

(define draw-mode 'off)
(define prev-x -1)
(define prev-y -1)
(define points (list))

(define (add-points x1 y1 x2 y2)
  (set! points (append points (list (list x1 y1 x2 y2)))))

(define (draw-points)
  (graphics-color! 'black)
  (graphics-line-style 'solid 5)
  (for p in points
       (let ((x1 (car p)) (y1 0) (x2 0) (y2 0))
	    (set! p (cdr p))
	    (set! y1 (car p))
	    (set! p (cdr p))
	    (set! x2 (car p))
	    (set! p (cdr p))
	    (set! y2 (car p))
	    (graphics-draw-line x1 y1 x2 y2))))

(define (canvas-draw canvas user-arg)
  (case draw-mode
    ((on) 
     (let ((x (event-x)) (y (event-y)))
       (if (= prev-x -1)
	   (set! prev-x x))
       (if (= prev-y -1)
	   (set! prev-y y))
       (add-points prev-x prev-y x y)
       (draw-points)
       (set! prev-x x)
       (set! prev-y y)))))

(define m-down #f)

(define (canvas-events canvas event user-arg)
  (case event
    ((mouse-push)
     (if (= (event-button) 1)
	 (set! m-down #t)))
    ((mouse-drag)
     (if m-down
	 (begin
	   (set! draw-mode 'on)
	   (widget-redraw canvas))))
    ((mouse-release)
     (if (= (event-button) 1)
	 (begin
	   (set! m-down #f)
	   (set! prev-x -1)
	   (set! prev-y -1)
	   (set! draw-mode 'off))))))

(define WIDTH 300)
(define HEIGHT 300)

(define w (window 'w WIDTH 'h HEIGHT 'title "FreeDraw" 'type 'double))

(define c (widget 'super 'border 'x 5 'y 5 'w (- WIDTH 5) 'h (- HEIGHT 5)
		  'draw canvas-draw 'events canvas-events))
(widget-border! c 'thin-down-frame)

(group-resizable w c)
(group-finish w)
(window-show w)
(airglow-run)
