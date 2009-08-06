(import (airglow) (airglow-3d))

;; check command-line arguments
(define argv-len 0)
(if (not (null? (argv)))
    (set! argv-len (vector-length (argv))))
(if (< argv-len 1)
    (begin
      (printf "Usage: spark tex-cube.ss <texture-file> <texture-width-height>~n")
      (printf "Example: spark tex-cube.ss tex01.raw 64~n")
      (exit)))
;; :~

(define first-time #t)
(define xrot 2)
(define yrot 0)
(define zrot 0)
(define tex01 null)
(define light 'light0)
(define enable-light #f)

(define (initialize-3d)
  (3d-enable 'depth-test)
  (3d-enable 'texture-2d)
  (3d-shade-model 'smooth)
  (3d-color-clear (list 0 0 0 0))
  (3d-depth-clear 1.0)
  (3d-enable 'depth-test)
  (3d-depth-func 'lequal)
  (3d-hint 'perspective-correction 'nicest)
  ;; lighting
  (3d-light-ambient light
		    'red 0.5 'green 0.5 
		    'blue 0.5 'alpha 1.0)
  (3d-light-diffuse light
		    'red 1.0 'green 1.0
		    'blue 1.0 'alpha 1.0)
  (3d-light-position light
		     (list 0.0 0.0
			   2.0 1.0))
  (3d-enable-light light)
  ;; :~
		     
  (load-texture))

(define wh 64)

(define image-file (vector-ref (argv) 0))
(if (> argv-len 1)
    (set! wh (string->number (vector-ref (argv) 1))))

(define (load-texture)
  (set! tex01 (3d-texture 'image-file image-file
			  'width wh
			  'height wh)))

(define (draw-cube)
  (3d-translate 0.0 0.0 -5.0)
  (3d-rotate xrot 1.0 0.0 0.0)
  (3d-rotate yrot 0.0 1.0 0.0)
  (3d-rotate zrot 0.0 0.0 1.0)
  (3d-bind-texture tex01)

  (3d-enable-lighting enable-light)

  (3d-quad 
   (list
    ;; front
    (list 'texture 0 0)
    '(-1 -1 1)
    (list 'texture 1 0)
    '(1 -1 1)
    (list 'texture 1 1)
    '(1 1 1)
    (list 'texture 0 1)
    '(-1 1 1)
    
    ;; back
    (list 'texture 1 0)
    '(-1 -1 -1)
    (list 'texture 1 1)
    '(-1 1 -1)
    (list 'texture 0 1)
    '(1 1 -1)
    (list 'texture 0 0)
    '(1 -1 -1)
    
    ;; top
    (list 'texture 0 1)
    '(-1 1 -1)
    (list 'texture 0 0)
    '(-1 1 1)
    (list 'texture 1 0)
    '(1 1 1)
    (list 'texture 1 1)
    '(1 1 -1)

    ;; bottom
    (list 'texture 1 1)
    '(-1 -1 -1)
    (list 'texture 0 1)
    '(1 -1 -1)
    (list 'texture 0 0)
    '(1 -1 1)
    (list 'texture 1 0)
    '(-1 -1 1)
    
    ;; right
    (list 'texture 1 0)
    '(1 -1 -1)
    (list 'texture 1 1)
    '(1 1 -1)
    (list 'texture 0 1)
    '(1 1 1)
    (list 'texture 0 0)
    '(1 -1 1)

    ;; left
    (list 'texture 0 0)
    '(-1 -1 -1)
    (list 'texture 1 0)
    '(-1 -1 1)
    (list 'texture 1 1)
    '(-1 1 1)
    (list 'texture 0 1)
    '(-1 1 -1))))
  
(define (draw w p)
  (if first-time
      (begin
	(initialize-3d)
	(set! first-time #f)))
  (3d-color-clear (list 0.0 0.0 0.0 0.0))
  (3d-depth-clear)
  (3d-matrix-mode 'projection)
  (3d-identity)
  (3d-frustum -1 1 -1 1 3 100)
  (3d-matrix-mode 'modelview)
  (3d-identity)
  (3d-lookat (list 0 0 3)
	     null
	     (list 0 1 0))
  ; rotate
  ;;(3d-rotate xrot 1 zrot 0)
  (draw-cube))

(define rot-factor .5)

(define (events w event p)
  (case event 
    ((key-down)
     (let ((redraw #t))
       (case (event-key)
	 ((up) 
	  (set! xrot (+ xrot 2.3))
	  (set! yrot (+ yrot 2.2))
	  (set! zrot (+ yrot 5.4)))
	 ((down) 
	  (set! xrot (- xrot 2.3))
	  (set! yrot (- yrot 2.2))
	  (set! zrot (- yrot 5.4)))
	 ((space)
	  (set! enable-light (not enable-light)))
	 (else (set! redraw #f)))
       (if redraw
	   (widget-redraw w))))))

(define 3d-window (widget 'x 100 'y 100 
			  'w 500 'h 500 
			  'title "Spark 3d - Texturing & Lighting" 
			  'super 'gl-window
			  'draw draw
			  'events events))

(group-finish 3d-window)
(window-show 3d-window)

(display "Use 'up' and 'down' keys to rotate the cube. Tap 'space' to turn on/off lighting.")
(newline)

(airglow-run)
