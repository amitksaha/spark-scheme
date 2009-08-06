;; Thanks to Tom Stanis and Jeff Molofee (http://nehe.gamedev.net/) for 
;; the original tutorial and image file.

(import (airglow) (airglow-3d))

(define first-time #t)
(define texture null)
(define box null)
(define top 0)
(define yloop 1)
(define xloop 0)
(define xrot 0)
(define yrot 0)

(define boxcol (vector #(1.0 0.0 0.0)
		       #(1.0 0.5 0.0)
		       #(1.0 1.0 0.0)
		       #(0.0 1.0 0.0)
		       #(0.0 1.0 1.0)))

(define topcol (vector #(0.5 0.0 0.0)
		       #(0.5 0.25 0.0)
		       #(0.5 0.5 0.0)
		       #(0.0 0.5 0.0)
		       #(0.0 0.5 0.5)))

(define wh 128)
(define image-file "data/Cube.raw")

(define (load-texture)
  (set! texture (3d-texture 'image-file image-file
			    'width wh
			    'height wh)))

(define (build-lists)
  (set! box (3d-gen-lists 2))
  (3d-new-list box 'compile)
  (3d-quad
   (list ;;Bottom Face
    (list 'texture 1.0 1.0)
    '(-1.0 -1.0 -1.0)
    (list 'texture 0.0 1.0)
    '( 1.0 -1.0 -1.0)
    (list 'texture 0.0 0.0)
    '( 1.0 -1.0  1.0)
    (list 'texture 1.0 0.0)
    '(-1.0 -1.0  1.0)
    ;; Front Face
    (list 'texture 0.0 0.0)
    '(-1.0 -1.0  1.0)
    (list 'texture 1.0 0.0)
    '( 1.0 -1.0  1.0)
    (list 'texture 1.0 1.0)
    '( 1.0  1.0  1.0)
    (list 'texture 0.0 1.0)
    '(-1.0  1.0  1.0)
    ;; Back Face
    (list 'texture 1.0 0.0)
    '(-1.0 -1.0 -1.0)
    (list 'texture 1.0 1.0)
    '(-1.0  1.0 -1.0)
    (list 'texture 0.0 1.0)
    '( 1.0  1.0 -1.0)
    (list 'texture 0.0 0.0)
    '( 1.0 -1.0 -1.0)
    ;; Right face
    (list 'texture 1.0 0.0)
    '( 1.0 -1.0 -1.0)
    (list 'texture 1.0 1.0)
    '( 1.0  1.0 -1.0)
    (list 'texture 0.0 1.0)
    '( 1.0  1.0  1.0)
    (list 'texture 0.0 0.0)
    '( 1.0 -1.0  1.0)
    ;; Left Face
    (list 'texture 0.0 0.0)
    '(-1.0 -1.0 -1.0)
    (list 'texture 1.0 0.0)
    '(-1.0 -1.0  1.0)
    (list 'texture 1.0 1.0)
    '(-1.0  1.0  1.0)
    (list 'texture 0.0 1.0)
    '(-1.0  1.0 -1.0)))
  
  (3d-end-list)
  
  (set! top (+ box 1))
  
  (3d-new-list top 'compile)

  (3d-quad
   (list
    ;; Top Face
    (list 'texture 0.0 1.0) 
    '(-1.0  1.0 -1.0)
    (list 'texture 0.0 0.0) 
    '(-1.0  1.0  1.0)
    (list 'texture 1.0 0.0) 
    '( 1.0  1.0  1.0)
    (list 'texture 1.0 1.0) 
    '( 1.0  1.0 -1.0)))
  
  (3d-end-list))

(define (init-3d)
  (load-texture)
  (build-lists)
  (3d-enable 'texture-2d)
  (3d-shade-model 'smooth)
  (3d-color-clear (list 0.0 0.0 0.0 0.5))
  (3d-depth-clear 1.0)
  (3d-enable 'depth-test)
  (3d-depth-func 'lequal)
  (3d-enable-light 'light0)
  (3d-enable-lighting #t)
  (3d-enable 'color-material)
  (3d-hint 'perspective-correction 'nicest))

(define (draw w p)
  (if first-time
      (begin
	(init-3d)
	(set! first-time #f)))

  (3d-color-clear)
  (3d-depth-clear)
  (3d-bind-texture texture)
  
  (while (< yloop 6)
       (set! xloop 0)
       (while (< xloop yloop)
	      (3d-identity)
	      (3d-translate (- (+ (* xloop 2.8) 1.4) (* yloop 1.4))
			    (- (- (* 2.4 yloop) 6.0) 7.0)
			    -20.0)
	      (3d-rotate (+ (- (* yloop 2.0) 45.0) xrot)
			 1.0 0.0
			 0.0)
	      (3d-rotate (+ 45.0 yrot)
			 0.0 1.0 0.0)
	      (let ((colors (vector-ref boxcol (- yloop 1))))
		(3d-color (vector-ref colors 0) 
			  (vector-ref colors 1)
			  (vector-ref colors 2) 
			  0.0))
	      (3d-call-list box)
	      (let ((colors (vector-ref topcol (- yloop 1))))
		(3d-color (vector-ref colors 0) 
			  (vector-ref colors 1)
			  (vector-ref colors 2)
			  0.0))
	      (3d-call-list top)
	      (set! xloop (add1 xloop)))
       (set! yloop (add1 yloop))))

(define 3d-window (widget 'x 100 'y 100 
			  'w 500 'h 500 
			  'title "Spark 3d - Display lists" 
			  'super 'gl-window
			  'draw draw
			  'events null))

(group-finish 3d-window)
(window-show 3d-window)

(display "Use 'up' and 'down' keys to rotate the cube. Tap 'space' to turn on/off lighting. Press 'b' to switch on/off blending.")
(newline)

(airglow-run)
	      
			    

