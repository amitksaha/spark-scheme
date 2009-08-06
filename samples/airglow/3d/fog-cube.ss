;; Thanks to Tom Stanis and Jeff Molofee (http://nehe.gamedev.net/) for 
;; the blending tutorial and image file.

(import (airglow) (airglow-3d))

(define first-time #t)
(define xrot 2)
(define yrot 0)
(define zrot 0)
(define tex01 null)
(define light 'light0)
(define enable-light #f)
(define enable-blend #f)
;; fog
(define enable-fog #f)
(define fog-mode #('exp 'exp-2 'linear)) 
(define fog-filter 0)
(define fog-color #(0.5 0.5 0.5 1.0))
;; :~

(define (initialize-3d)
  (3d-enable 'depth-test)
  (3d-enable 'texture-2d)
  (3d-shade-model 'smooth)
  (3d-color-clear (list 0 0 0 0))
  (3d-depth-clear 1.0)
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

  (3d-color 1.0 1.0 1.0 0.5)
  (3d-blend-f 'src-alpha 'one)
  ;; :~
		     
  (load-texture))

(define wh 128)
(define image-file "data/Glass.raw")

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
  (3d-enable-blending enable-blend)

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
  (3d-color-clear (list (vector-ref fog-color 0)
			(vector-ref fog-color 1)
			(vector-ref fog-color 2)
			(vector-ref fog-color 3)))
  (3d-depth-clear)
  (3d-matrix-mode 'projection)
  (3d-identity)
  (3d-frustum -1 1 -1 1 2 100)
  (3d-matrix-mode 'modelview)
  (3d-identity)
  (3d-lookat (list 0 0 3)
	     null
	     (list 0 1 0))

  (3d-fog 'fog-mode (vector-ref fog-mode fog-filter))
  (3d-fog 'fog-color (vector->list fog-color))
  (3d-fog 'fog-density 0.35)
  (3d-hint 'fog 'dont-care)
  (3d-fog 'fog-start 1.0)
  (3d-fog 'fog-end 5.0)
  (3d-enable-fog #t)
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
	 ((b)
	  (set! enable-blend (not enable-blend)))
	 ((f)
	  (set! fog-filter (add1 fog-filter))
	  (if (> fog-filter 2)
	      (set! fog-filter 0))
	  (3d-fog 'fog-mode (vector-ref fog-mode fog-filter)))
	 (else (set! redraw #f)))
       (if redraw
	   (widget-redraw w))))))

(define 3d-window (widget 'x 100 'y 100 
			  'w 500 'h 500 
			  'title "Spark 3d - Blending with Fog" 
			  'super 'gl-window
			  'draw draw
			  'events events))

(group-finish 3d-window)
(window-show 3d-window)

(display "Use 'up' and 'down' keys to rotate the cube. Tap 'space' to turn on/off lighting. Press 'b' to switch on/off blending. Use the 'f' key to change fog mode.")
(newline)

(airglow-run)
