(import (airglow) (airglow-3d))

(define first-time #t)
(define rot-a 0)
(define rot-y 0)
(define tex01 null)

(define (initialize-3d)
  (3d-enable 'depth-test))

(define (draw-cube)
  (3d-quad 
   (list
    ;; front
    (list 'color 1 0 0)
    '(-1 1 1)
    '(-1 -1 1)
    '(1 -1 1)
    '(1 1 1)
    
    ;; back
    (list 'color 0 1 0)
    '(-1 1 -1)
    '(1 1 -1)
    '(1 -1 -1)
    '(-1 -1 -1)
    
    ;; top
    (list 'color 0 0 1)
    '(-1 1 -1)
    '(-1 1 1)
    '(1 1 1)
    '(1 1 -1)

    ;; bottom
    (list 'color 1 1 0)
    '(-1 -1 -1)
    '(1 -1 -1)
    '(1 -1 1)
    '(-1 -1 1)
    
    ;; left
    (list 'color 0 1 1)
    '(-1 1 -1)
    '(-1 -1 -1)
    '(-1 -1 1)
    '(-1 1 1)
    
    ;; right
    (list 'color 1 0 1)
    '(1 1 1)
    '(1 -1 1)
    '(1 -1 -1)
    '(1 1 -1))))
  
(define (draw w p)
  (if first-time
      (begin
	(initialize-3d)
	(set! first-time #f)))
  (3d-color-clear (list 0.1 0.1 0.1 1))
  (3d-depth-clear)
  (3d-matrix-mode 'projection)
  (3d-identity)
  (3d-frustum -1 1 -1 1 1 100)
  (3d-matrix-mode 'modelview)
  (3d-identity)
  (3d-lookat (list 0 0 3)
	     null
	     (list 0 1 0))
  ; rotate
  (3d-rotate rot-a 1 rot-y 0)
  (draw-cube))

(define rot-factor .5)

(define (events w event p)
  (case event 
    ((key-down)
     (let ((redraw #t))
       (case (event-key)
	 ((up) (set! rot-a (- rot-a rot-factor)))	
	 ((down) (set! rot-a (+ rot-a rot-factor)))
	 ((space)
	  (if (= rot-y 0)
	      (set! rot-y 3.2)
	      (set! rot-y 0)))
	 (else (set! redraw #f)))
       (if redraw
	   (widget-redraw w))))))

(define 3d-window (widget 'x 100 'y 100 
			  'w 500 'h 500 
			  'title "Spark 3d" 
			  'super 'gl-window
			  'draw draw
			  'events events))

(group-finish 3d-window)
(window-show 3d-window)

(display "Use 'up' and 'down' keys to rotate the cube. Tap the 'space' key switch direction.")
(newline)

(airglow-run)
