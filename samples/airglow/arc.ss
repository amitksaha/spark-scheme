(import (airglow))

(define args #(140 140 50 0 360 0))
(define name #("X" "Y" "R" "start" "end" "rotate"))

(define (draw-cb self p)
  (let ((x (widget-x self)) 
	(y (widget-y self))
	(w (widget-width self))
	(h (widget-height self))
	(n 0))	   
  (graphics-clip x y w h)
  (graphics-color! 'dark3)
  (graphics-draw-rectf x y w h)
  (graphics-2d-push-matrix)
  (set! n (vector-ref args 5))
  (if (not (= n 0))
      (begin
	(graphics-2d-translate (+(/ w 2.0) x) 
				(+(/ h 2.0) y))
	(graphics-2d-rotate (vector-ref args 5))
	(graphics-2d-translate (* -1 (+(/ w 2.0) x))
				(* -1 (+(/ h 2.0) y)))))

  (graphics-color! 'white)
  (graphics-2d-translate x y)
  (graphics-2d-begin 'complex-polygon)
  (graphics-2d-arc (vector-ref args 0)
		      (vector-ref args 1)
		      (vector-ref args 2)
		      (vector-ref args 3)
		      (vector-ref args 4))
  (graphics-2d-gap)
  (graphics-2d-arc 140 140 20 0 -360)
  (graphics-2d-end 'complex-polygon)
  (graphics-color! 'red)
  (graphics-2d-begin 'line)
  (graphics-2d-arc (vector-ref args 0)
		       (vector-ref args 1)
		       (vector-ref args 2)
		       (vector-ref args 3)
		       (vector-ref args 4))
  (graphics-2d-end 'line)
  (graphics-2d-pop-matrix)
  (graphics-unclip)))

(define main-window (window 'x 300 'y 400 
			    'w 300 'h 500 
			    'title "Arc" 
			    'type 'double))
(define drawing (widget 'x 10 'y 10 
			'w 280 'h 280
			'super 'widget
			'draw draw-cb))

(define (slider-cb s n)
  (vector-set! args n (valuator-value s))
  (widget-redraw drawing))

(define y 300)
(define n 0)
(let loop ()
  (let ((s null))
    (if (< n 6)
	(begin
	  (set! s (valuator 'x 50 'y y 'w 240 'h 25 
			    'title (vector-ref name n)
			    'type 'value-slider))
	  (valuator-slider-type! s 'hor)
	  (set! y (+ y 25))
	  (cond
	   ((< n 3)
	    (valuator-minimum! s 0)
	    (valuator-maximum! s 300))
	   ((= n 5)
	    (valuator-minimum! s 0)
	    (valuator-maximum! s 360))
	   (else
	    (valuator-minimum! s -360)
	    (valuator-maximum! s 360)))
	  (valuator-step! s 1)
	  (valuator-value! s (vector-ref args n))
	  (widget-label-align! s 'left)
	  (widget-callback! s slider-cb n)
	  (set! n (+ n 1))
	  (loop)))))

(group-finish main-window)
(window-show main-window)
(airglow-run)
