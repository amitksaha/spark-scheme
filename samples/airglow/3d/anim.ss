;; Thanks to Tom Stanis and Jeff Molofee (http://nehe.gamedev.net/) for 
;; the animation tutorial and image file.

(require airglow)
(require airglow-3d)

(define first-time #t)
(define xrot 2)
(define yrot 0)
(define zrot 0)
(define tex01 null)
(define twinkle #t)
(define num 50)

(define-struct star (r g b dist angle))
(define stars null)

(define zoom -15.0)
(define tilt 90.0)
(define spin 0.0)

(define (init-stars)
  (if (null? stars)
      (begin
	(set! stars (make-vector num))
	(for i in (range num)
	     (let ((s (make-star (random 256)
				 (random 256)
				 (random 256)
				 (* (/ i num) 5.0)
				 0.0)))
	       (vector-set! stars i s))))))

(define (initialize-3d)
  (3d-enable 'depth-test)
  (3d-enable 'texture-2d)
  (3d-shade-model 'smooth)
  (3d-color-clear (list 0 0 0 0))
  (3d-depth-clear 1.0)
;  (3d-depth-func 'lequal)
  (3d-hint 'perspective-correction 'nicest)
  (3d-color 1.0 1.0 1.0 0.5)
  (3d-blend-f 'src-alpha 'one)
  (3d-enable 'blend)
  (load-texture)
  
  (init-stars))

(define wh 128)
(define image-file "data/Star.raw")

(define (load-texture)
  (set! tex01 (3d-texture 'image-file image-file
			  'width wh
			  'height wh)))

(define (draw w p)
  (if first-time
      (begin
	(initialize-3d)
	(set! first-time #f)))
  (3d-color-clear (list 0.0 0.0 0.0 0.0))
  (3d-depth-clear)
  (3d-bind-texture tex01)
  (for i in (range num)
       (let ((star (vector-ref stars i)))
	 (3d-identity)
	 (3d-translate 0.0 0.0 zoom)
	 (3d-rotate tilt 1.0 0.0 0.0)
	 (3d-rotate (star-angle star) 0.0 1.0 0.0)
	 (3d-translate (star-dist star) 0.0 0.0)
	 (3d-rotate (* (star-angle star) -1) 0.0 1.0 0.0)
	 (3d-rotate (* tilt -1) 1.0 0.0 0.0)
	 (if twinkle
	     (begin
	       (3d-color-ub (star-r star) (star-g star) (star-b star) 255)
	       (3d-quad
		(list
		 (list 'texture 0.0 0.0)
		 '(-1.0 -1.0 0.0)
		 (list 'texture 1.0 0.0)
		 '(1.0 -1.0 0.0)
		 (list 'texture 1.0 1.0)
		 '(1.0 1.0 0.0)
		 (list 'texture 0.0 1.0)
		 '(-1.0 1.0 0.0)))))
	 (3d-rotate spin 0.0 0.0 1.0)
	 (3d-color-ub (star-r star) (star-g star) (star-b star) 255)
	 (3d-quad
	  (list
	   (list 'texture 0.0 0.0)
	   '(-1.0 -1.0 0.0)
	   (list 'texture 1.0 0.0)
	   '(1.0 -1.0 0.0)
	   (list 'texture 1.0 1.0)
	   '(1.0 1.0 0.0)
	   (list 'texture 0.0 1.0)
	   '(-1.0 1.0 0.0)))
       (set! spin (+ spin 0.01))
       (set-star-angle! star (+ (star-angle star) (/ i num)))
       (set-star-dist! star (- (star-dist star) 0.01))
       (if (< (star-dist star) 0.0)
	   (begin
	     (set-star-dist! star (+ (star-dist star) 5.0))
	     (set-star-r! star (random 256))
	     (set-star-g! star (random 256))
	     (set-star-b! star (random 256)))))))

(define (events w event p)
  (case event 
    ((key-down)
     (case (event-key)
       ((t) 
	(set! twinkle (not twinkle)))
       ((up)
	(set! tilt (- tilt 0.5)))
       ((down)
	(set! tilt (+ tilt 0.5)))
       ((left)
	(set! zoom (- zoom 0.2)))
       ((right)
	(set! zoom (+ zoom 0.2)))))))

(define 3d-window (widget 'x 100 'y 100 
			  'w 500 'h 500 
			  'title "Spark 3d - Animation" 
			  'super 'gl-window
			  'draw draw
			  'events events))

(group-finish 3d-window)
(window-show 3d-window)

(display "Use 'up' and 'down' keys to change tilting, 'left' and 'right' to zoom out/in. Press 't' to switch on/off twinkling.")
(newline)

(airglow-run)
