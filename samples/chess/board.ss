(library board
	 
	 (import (airglow) (chess-utils) (chess-consts))

	 (define chess-board null)
	 (define add-dimensions #t)
	 (define images-map null)
	 (define prev-square null)

	 (define (board-init)
	   (init-notation-map)
	   (set! chess-board null)
	   (set! images-map null)
	   (set! images-map (load-images))
	   (set! chess-board (init-chess-board 'normal)))

	 (define (board-draw self p)
	   (let* ((x (widget-x self)) 
		  (y (widget-y self))
		  (w (widget-width self))
		  (h (widget-height self))
		  (box-w (quotient w FILES-COUNT))
		  (box-h (quotient h RANKS-COUNT))
		  (current-img null)
		  (i 0)
		  (current-x x)
		  (current-y y)
		  (box-count 0)
		  (sqr null))
	     (let loop ()
	       (if (< box-count SQUARE-COUNT)
		   (begin
		     (let inner-loop ()
		       (if (< i 8)
			   (begin
			     (set! sqr (vector-ref chess-board box-count))
			     (set! current-img (get-piece-image images-map sqr))
			     (image-draw current-img current-x current-y)
			     (if (square-selected sqr)
				 (graphics-draw-rect current-x current-y box-w box-h 'red)
				 (graphics-draw-rect current-x current-y box-w box-h 'black))
			     (if add-dimensions
				 (begin
				   (set-square-dim! sqr 
						    (make-dimension current-x 
								    current-y
								    box-w
								    box-h))))
			     (set! current-x (+ current-x box-w))
			     (set! i (+ i 1))
			     (set! box-count (+ box-count 1))
			     (inner-loop))))
		     (set! i 0)
		     (set! current-x x)
		     (set! current-y (+ current-y box-h))
		     (loop)))))
	   (if add-dimensions
	       (set! add-dimensions #f)))
	 
	 (define (find-square)
	   (let ((i 0)
		 (len SQUARE-COUNT)
		 (d null)
		 (break #f)
		 (s -1))
	     (let loop ()
	       (if (< i len)
		   (begin
		     (set! d (square-dim (vector-ref chess-board i)))
		     (if (not (null? d))
			 (if (event-inside? (dimension-x d)
					    (dimension-y d)
					    (dimension-w d)
					    (dimension-h d))			
			     (begin
			       (set! s i)
			       (set! break #t))))
		     (if (not break)
			 (begin
			   (set! i (add1 i))
			   (loop))))))
	     s))
	 
	 (define (board-events self event p)
	   (case event 
	     ((mouse-push)
	      (if (= (event-button) 1)		 
		  (begin
		    (if (get-move-mode)
			(begin
			  (let ((s (find-square)))
			    (set-move-from-square! s)
			    (set-selection! s)
			    (widget-redraw self)
			    (set-move-mode! #f)))
			(begin
			  (let ((s (find-square)))
			    (set-move-to-square! s)
			    (set-selection! null)
			    (widget-redraw self)
			    (make-move self chess-board)))))))))

	 (define (set-selection! s)
	   (if (eqv? s null)
	       (begin
		 (if (not (eqv? prev-square null))
		     (set-square-selected! prev-square #f)))
	       (begin
		 (let ((sqr (vector-ref chess-board s)))
		   (set-square-selected! sqr #t)
		   (if (not (eqv? prev-square null))
		       (set-square-selected! prev-square #f))
		   (set! prev-square sqr)))))

	 (define (board-move self from-to)
	   (let ((pos (get-positions from-to))
		 (from -1) (to -1))
	     (if (not (eqv? pos null))
		 (begin
		   (set! from (car pos))
		   (set! to (car (cdr pos)))))
	     (move-by-squares chess-board 
			      (get-square-by-notation from)
			      (get-square-by-notation to))
	     (widget-redraw self)))

	 (export board-draw
		 board-events
		 board-init
		 board-move))
