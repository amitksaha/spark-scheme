(library chess-utils

	 (import (airglow) (chess-consts))

	 (define-struct chess-piece (value color))
	 (define-struct dimension (x y w h))
	 (define-struct square (color piece dim selected))

	 (define reverse-mode #f)
	 (define move-mode #t)
	 (define move-from-sqr-num -1)
	 (define move-to-sqr-num -1)
	 (define notation-map null)
	 (define notations (make-vector 64))
	 (define moves (list))

	 (define (find-row current-square)
	   (cond 
	    ((and (>= current-square 0)
		  (<= current-square 7))
	     0)
	    ((and (>= current-square 8)
		  (<= current-square 15))
	     1)
	    ((and (>= current-square 16)
		  (<= current-square 23))
	     2)
	    ((and (>= current-square 24)
		  (<= current-square 31))
	     3)
	    ((and (>= current-square 32)
		  (<= current-square 39))
	     4)
	    ((and (>= current-square 40)
		  (<= current-square 47))
	     5)
	    ((and (>= current-square 48)
		  (<= current-square 55))
	     6)
	    ((and (>= current-square 56)
		  (<= current-square 63))
	     7)))

	 (define (load-images)
	   (let ((images-map null))
	     (set! images-map (make-hash-table 'equal))
	     ;; images
	     (hash-table-put! images-map WHITE
			      (image "./images/w.png"))
	     (hash-table-put! images-map BLACK
			      (image "./images/b.png"))
	     ;; black pieces
	     (hash-table-put! images-map (* (+ BLACK PAWN WHITE) -1)
			      (image "./images/bpw.png"))
	     (hash-table-put! images-map (* (+ BLACK PAWN BLACK) -1)
			      (image "./images/bpb.png"))
	     (hash-table-put! images-map (* (+ BLACK ROOK WHITE) -1)
			      (image "./images/brw.png"))
	     (hash-table-put! images-map (* (+ BLACK ROOK BLACK) -1)
			      (image "./images/brb.png"))
	     (hash-table-put! images-map (* (+ BLACK KNIGHT WHITE) -1)
			      (image "./images/bnw.png"))
	     (hash-table-put! images-map (* (+ BLACK KNIGHT BLACK) -1)
			      (image "./images/bnb.png"))
	     (hash-table-put! images-map (* (+ BLACK BISHOP WHITE) -1)
			      (image "./images/bbw.png"))
	     (hash-table-put! images-map (* (+ BLACK BISHOP BLACK) -1)
			      (image "./images/bbb.png"))
	     (hash-table-put! images-map (* (+ BLACK QUEEN WHITE) -1)
			      (image "./images/bqw.png"))
	     (hash-table-put! images-map (* (+ BLACK QUEEN BLACK) -1)
			      (image "./images/bqb.png"))
	     (hash-table-put! images-map (* (+ BLACK KING WHITE) -1)
			      (image "./images/bkw.png"))
	     (hash-table-put! images-map (* (+ BLACK KING BLACK) -1)
			      (image "./images/bkb.png"))
	     ;; white pieces
	     (hash-table-put! images-map (+ WHITE PAWN WHITE)
			      (image "./images/wpw.png"))
	     (hash-table-put! images-map (+ WHITE PAWN BLACK)
			      (image "./images/wpb.png"))
	     (hash-table-put! images-map (+ WHITE ROOK WHITE) 
			      (image "./images/wrw.png"))
	     (hash-table-put! images-map (+ WHITE ROOK BLACK)
			      (image "./images/wrb.png"))
	     (hash-table-put! images-map (+ WHITE KNIGHT WHITE)
			      (image "./images/wnw.png"))
	     (hash-table-put! images-map (+ WHITE KNIGHT BLACK)
			      (image "./images/wnb.png"))
	     (hash-table-put! images-map (+ WHITE BISHOP WHITE)
			      (image "./images/wbw.png"))
	     (hash-table-put! images-map (+ WHITE BISHOP BLACK)
			      (image "./images/wbb.png"))
	     (hash-table-put! images-map (+ WHITE QUEEN WHITE)
			      (image "./images/wqw.png"))
	     (hash-table-put! images-map (+ WHITE QUEEN BLACK)
			      (image "./images/wqb.png"))
	     (hash-table-put! images-map (+ WHITE KING WHITE)
			      (image "./images/wkw.png"))
	     (hash-table-put! images-map (+ WHITE KING BLACK)
			      (image "./images/wkb.png"))
	     images-map))
	 
	 (define (init-chess-board mode)
	   (let ((chess-board null))
	     (set! chess-board (make-vector SQUARE-COUNT))
	     (let ((i 0) (s-color WHITE) 
		   (p-value EMPTY) 
		   (p-color WHITE)
		   (top-p-color BLACK)
		   (bot-p-color WHITE)
		   (p 0)
		   (col-count 0))
	       (if (not (eqv? mode 'normal))
		   (begin
		     (set! top-p-color WHITE)
		     (set! bot-p-color BLACK)
		     (set! reverse-mode #t)))
	       (let loop ()
		 (if (< i SQUARE-COUNT)
		     (begin
		       (cond
			((and (>= i 16)
			      (<= i 47))
			 (set! p null))
			((and (>= i 8)
			      (<= i 15))
			 (set! p-value PAWN)
			 (set! p-color top-p-color))
			((and (>= i 48)
			      (<= i 55))
			 (set! p-value PAWN)
			 (set! p-color bot-p-color))
			((or (= i 0) 
			     (= i 7))
			 (set! p-value ROOK)
			 (set! p-color top-p-color))
			((or (= i 56)
			     (= i 63))
			 (set! p-value ROOK)
			 (set! p-color bot-p-color))
			((or (= i 1)
			     (= i 6))
			 (set! p-value KNIGHT)
			 (set! p-color top-p-color))
			((or (= i 57)
			     (= i 62))
			 (set! p-value KNIGHT)
			 (set! p-color bot-p-color))
			((or (= i 2)
			     (= i 5))
			 (set! p-value BISHOP)
			 (set! p-color top-p-color))
			((or (= i 58)
			     (= i 61))
			 (set! p-value BISHOP)
			 (set! p-color bot-p-color))
			((= i 3)
			 (set! p-value QUEEN)
			 (set! p-color top-p-color))
			((= i 59)		      
			 (set! p-value QUEEN)
			 (set! p-color bot-p-color))
			((= i 4)
			 (set! p-value KING)
			 (set! p-color top-p-color))
			((= i 60)		      
			 (set! p-value KING)
			 (set! p-color bot-p-color)))
		       (if (not (eqv? p null))
			   (set! p (make-chess-piece p-value p-color)))			  
		       (vector-set! chess-board i 
				    (make-square s-color p null #f))
		       (set! p 0)
		       (if (= col-count 7)
			   (begin
			     (set! col-count 0))
			   (begin
			     (if (= s-color WHITE)
				 (set! s-color BLACK)
				 (set! s-color WHITE))
			     (set! col-count (add1 col-count))))
		       (set! i (add1 i))
		       (loop)))))
	     chess-board))

	 (define (get-valid-squares chess-board piece-from)
	   (let ((piece-value (chess-piece-value piece-from))
		 (piece-color (chess-piece-color piece-from))
		 (from-top #f))
	     (cond
	      ((and reverse-mode (= piece-color WHITE))
	       (set! from-top #t))
	      ((= piece-color BLACK)
	       (set! from-top #t)))
	     (cond
	      ((= piece-value PAWN)
	       (find-possible-moves-for-pawn chess-board
					     move-from-sqr-num 
					     from-top))
	      ((= piece-value ROOK)
	       (find-possible-moves-for-rook chess-board
					     move-from-sqr-num #f))
	      ((= piece-value KNIGHT)
	       (find-possible-moves-for-knight chess-board
					       move-from-sqr-num))
	      ((= piece-value BISHOP)
	       (find-possible-moves-for-bishop chess-board
					       move-from-sqr-num #f))
	      ((= piece-value QUEEN)
	       (find-possible-moves-for-queen chess-board
					      move-from-sqr-num #f))
	      ((= piece-value KING)
	       (find-possible-moves-for-king chess-board
					     move-from-sqr-num)))))

	 (define (square-in-list valid-squares)
	   (let ((ret #f))
	     (if (eqv? valid-squares null)
		 ret
		 (begin
		   (let ((v null))
		     (let loop ()
		       (if (not (eqv? valid-squares null))
			   (begin
			     (set! v (car valid-squares))
			     (if (= v move-to-sqr-num)
				 (begin
				   (set! ret #t)
				   (set! valid-squares null))
				 (begin
				   (set! valid-squares (cdr valid-squares))
				   (loop)))))))))
	     ret))
	 
	 (define (get-piece-image images-map s)
	   (let*((p (square-piece s))
		 (sc (square-color s))
		 (i 0))
	     (set! i (+ i sc))
	     (if (not (eqv? p null))
		 (begin
		   (let ((pc (chess-piece-color p)))
		     (set! i (+ i (chess-piece-value p) pc))
		     (if (=  pc BLACK)
			 (set! i (* i -1))))))
	     (hash-table-get images-map i)))

	 (define (move-by-squares chess-board from to)
	   (let ((sqr-from (get-square chess-board from))
		 (sqr-to (get-square chess-board to))
		 (piece-from null))
	     (if (not (eqv? sqr-from null))
		 (set! piece-from (square-piece sqr-from)))
	     (if (not (eqv? piece-from null))
		 (begin
		   (set-square-piece! sqr-to piece-from)
		   (set-square-piece! sqr-from null)))))

	 (define (print-move from to)
	   (print (get-notation from)) (print (get-notation to))
	   (newline))

	 (define (add-move from to)
	   (let ((out (open-output-string)))
	     (fprintf out "~a~a" 
		      (get-notation from)
		      (get-notation to))
	     (set! moves (append moves (list (get-output-string out))))))

	 (define (add-move-by-notation m)
	   (set! moves (append moves (list m))))

	 (define (reset-moves)
	   (set! moves null)
	   (set! moves (list)))

	 (define (get-moves)
	   moves)

	 (define (make-move self chess-board)
	   (if (not (= move-from-sqr-num move-to-sqr-num))
	       (begin
		 (let ((sqr-from (get-square chess-board move-from-sqr-num))
		       (sqr-to (get-square chess-board move-to-sqr-num))
		       (piece-from null)
		       (piece-to null)
		       (valid-squares null)
		       (move-made #f)
		       (can-move #t))
		   (if (not (eqv? sqr-from null))
		       (set! piece-from (square-piece sqr-from)))
		   (if (not (eqv? sqr-to null))
		       (set! piece-to (square-piece sqr-to)))
		   (if (not (eqv? piece-from null))
		       (begin
			 (if (not (eqv? piece-to null))
			     (begin
			       (if (= (chess-piece-color piece-from)
				      (chess-piece-color piece-to))
				   (set! can-move #f))))
			 (if can-move
			     (begin		
			       (set! valid-squares (get-valid-squares chess-board
								      piece-from))
			       (if (square-in-list valid-squares)
				   (begin
				     (set-square-piece! sqr-to piece-from)
				     (set-square-piece! sqr-from null)
				     (set! move-made #t)))
			       (if (= (chess-piece-value piece-from) KING)
				   (begin
				     (set! valid-squares (find-castle-moves
							  chess-board
							  move-from-sqr-num))
				     (if (square-in-list valid-squares)
					 (begin
					   (let ((sqr 0) (opr -))
					     (set-square-piece! sqr-to piece-from)
					     (set-square-piece! sqr-from null)
					     (set! move-made #t)
					     (set! sqr move-to-sqr-num)
					     (if (or (= move-to-sqr-num 62)
						     (= move-to-sqr-num 6))
						 (set! sqr-to (get-square chess-board (+ sqr 1)))
						 (begin
						   (set! sqr-to (get-square chess-board (- sqr 2)))
						   (set! opr +)))
					     (set! piece-from (square-piece sqr-to))
					     (set-square-piece! sqr-to null)
					     (set! sqr-to (get-square chess-board (opr sqr 1)))
					     (set-square-piece! sqr-to piece-from))))))
			       (if move-made
				   (begin
				     (add-move move-from-sqr-num
					       move-to-sqr-num)
				     (widget-redraw self)))))))
		   (if (not move-made)
		       (ask-beep)))))
	   (set! move-from-sqr-num -1)
	   (set! move-to-sqr-num -1)
	   (set! move-mode #t))

	 (define (find-possible-moves-for-pawn chess-board 
					       current-square 
					       from-top)
	   (let ((current-row (find-row current-square))
		 (opr -) (max-rows 7) (next-sqr 0)
		 (valid-squares null) (tmp 0) (sqr null))
	     (if from-top
		 (begin
		   (set! opr +)
		   (set! max-rows 0)))
	     (if (= current-row max-rows)
		 null
		 (begin
		   (set! valid-squares (list))
		   (set! next-sqr (opr current-square 8))
		   (if (can-pawn-move chess-board
				      next-sqr)
		       (set! valid-squares (append valid-squares
						   (list next-sqr))))
		   (set! tmp (+ next-sqr 1))
		   (if (can-pawn-capture chess-board 
					 current-square
					 tmp)
		       (set! valid-squares (append valid-squares
						   (list tmp))))
		   (set! tmp (- next-sqr 1))
		   (if (can-pawn-capture chess-board
					 current-square
					 tmp)
		       (set! valid-squares (append valid-squares
						   (list tmp))))
		   (if (or (= current-row 1)
			   (= current-row 6))
		       (begin
			 (set! next-sqr (opr current-square 16))
			 (if (can-pawn-move chess-board
					    next-sqr)
			     (set! valid-squares (append valid-squares 
							 (list next-sqr))))
			 (set! next-sqr (opr current-square 8))
			 (set! tmp (+ next-sqr 1))
			 (if (can-pawn-capture chess-board
					       current-square
					       tmp)
			     (set! valid-squares (append valid-squares
							 (list tmp))))
			 (set! tmp (- next-sqr 1))
			 (if (can-pawn-capture chess-board
					       current-square
					       tmp)
			     (set! valid-squares (append valid-squares
							 (list tmp))))))
		   valid-squares))))

	 (define (can-pawn-capture chess-board
				   current
				   to)
	   (let ((current-square (get-square chess-board current))
		 (to-square (get-square chess-board to))
		 (to-piece null)
		 (current-piece null))
	     (if (not (eqv? current-square null))
		 (set! current-piece (square-piece current-square)))
	     (if (not (eqv? to-square null))
		 (set! to-piece (square-piece to-square)))
	     (if (not (eqv? to-piece null))
		 (begin
		   (let ((to-color (chess-piece-color to-piece))
			 (current-color (chess-piece-color current-piece)))
		     (not (= to-color current-color))))
		 #f)))

	 (define (can-pawn-move chess-board
				to)
	   (let ((to-square (get-square chess-board to))
		 (to-piece null))
	     (if (not (eqv? to-square null))
		 (set! to-piece (square-piece to-square)))
	     (eqv? to-piece null)))

	 (define (find-possible-moves-for-bishop chess-board 
						 current-square
						 for-king)
	   (let ((ret (list)))
	     (set! ret (append ret (find-diagonal chess-board
						  current-square
						  'up
						  'right 
						  for-king)))
	     (set! ret (append ret (find-diagonal chess-board
						  current-square
						  'up
						  'left 
						  for-king)))
	     (set! ret (append ret (find-diagonal chess-board
						  current-square
						  'down
						  'right 
						  for-king)))
	     (set! ret (append ret (find-diagonal chess-board
						  current-square
						  'down
						  'left 
						  for-king)))
	     ret))

	 (define (find-possible-moves-for-rook chess-board 
					       current-square 
					       for-king)
	   (let ((ret (list)))
	     (set! ret (append ret (find-up-down chess-board
						 current-square
						 'up 
						 for-king)))
	     (set! ret (append ret (find-up-down chess-board
						 current-square
						 'down 
						 for-king)))
	     (set! ret (append ret (find-left-right chess-board
						    current-square
						    'left 
						    for-king)))
	     (set! ret (append ret (find-left-right chess-board
						    current-square
						    'right 
						    for-king)))
	     ret))

	 (define (find-possible-moves-for-knight chess-board 
						 current-square)
	   (let ((ret (list))
		 (gallops 2)
		 (side-ways 1)
		 (current-piece (get-square chess-board current-square))
		 (from-piece null)
		 (to-piece null)
		 (from-color null)
		 (to-color null)
		 (next-sqr 0)
		 (start-from-sqr 0)
		 (min-sqr 0)
		 (max-sqr 0)
		 (move-status null)
		 (order 1)
		 (opr -)
		 (current-row (find-row current-square)))
	     (if (not (eqv? current-piece null))
		 (set! from-piece (square-piece current-piece)))
	     (if (not (eqv? from-piece null))
		 (set! from-color (chess-piece-color from-piece)))
	     (set! current-row (add1 current-row))
	     (set! max-sqr (* current-row 8))
	     (set! max-sqr (sub1 max-sqr))
	     (set! min-sqr (- max-sqr 7))	    
	     (if (and (>= current-row 0)
		      (<= current-row 8))
		 (begin
		   (let loop ()
		     (if (<= order 2)
			 (begin
			   (if (= order 2)
			       (set! opr +))
			   (set! start-from-sqr (opr current-square (* gallops 8)))
			   (set! next-sqr (add1 start-from-sqr))
			   (set! move-status (can-move-or-capture chess-board
								  from-color 
								  next-sqr))  
			   (if (not (eqv? move-status null))
			       (begin
				 (set! ret (append ret (list next-sqr)))
				 (set! move-status null)))
			   (set! next-sqr (sub1 start-from-sqr))
			   (set! move-status (can-move-or-capture chess-board
								  from-color 
								  next-sqr))
			   (if (not (eqv? move-status null))
			       (begin
				 (set! ret (append ret (list next-sqr)))
				 (set! move-status null)))
			   (set! gallops 1)
			   (set! start-from-sqr (opr current-square (* gallops 8)))
			   (set! next-sqr (+ start-from-sqr 2))
			   (set! move-status (can-move-or-capture chess-board
								  from-color 
								  next-sqr))
			   (if (not (eqv? move-status null))
			       (begin
				 (set! ret (append ret (list next-sqr)))
				 (set! move-status null)))	
			   (set! next-sqr (- start-from-sqr 2))
			   (set! move-status (can-move-or-capture chess-board
								  from-color 
								  next-sqr))
			   (if (not (eqv? move-status null))
			       (begin
				 (set! ret (append ret (list next-sqr)))
				 (set! move-status null)))
			   (set! gallops 2)
			   (set! order (add1 order))
			   (loop))))))
	     ret))
	 
	 (define (find-possible-moves-for-queen chess-board
						current-square 
						for-king)
	   (let ((ret (list))
		 (tmp null))
	     (set! tmp (find-possible-moves-for-bishop chess-board
						       current-square 
						       for-king))
	     (if (> (length tmp) 0)
		 (set! ret (append ret tmp)))
	     (set! tmp null)
	     (set! tmp (find-possible-moves-for-rook chess-board
						     current-square
						     for-king))
	     (if (> (length tmp) 0)
		 (set! ret (append ret tmp)))
	     ret))

	 (define (find-possible-moves-for-king chess-board
					       current-square)
	   (find-possible-moves-for-queen chess-board
					  current-square
					  #t))

	 (define (find-diagonal chess-board 
				current-square
				row-direction
				direction 
				for-king)
	   (let ((current-row (find-row current-square))
		 (current-piece (get-square chess-board current-square))
		 (from-piece null)
		 (to-piece null)
		 (from-color null)
		 (to-color null)
		 (ret (list))
		 (next-sqr 0)
		 (next-row 0)
		 (row-opr +)
		 (row-cmp-opr <=)
		 (row-incr-func add1)
		 (move-status null)
		 (final-row 8)
		 (last-sqr-on-row 0)
		 (subtract-factor 7))
	     (if (not (eqv? current-piece null))		
		 (set! from-piece (square-piece current-piece)))
	     (if (not (eqv? from-piece null))
		 (set! from-color (chess-piece-color from-piece)))
	     (if (and (eqv? row-direction 'up)
		      (eqv? direction 'left))
		 (set! subtract-factor 9))
	     (if (and (eqv? row-direction 'down)
		      (eqv? direction 'right))
		 (set! subtract-factor 9))
	     (if (eqv? row-direction 'up)
		 (begin
		   (set! final-row 1)
		   (set! row-cmp-opr >=)
		   (set! row-opr -)
		   (set! row-incr-func sub1)))
	     (set! current-row (add1 current-row))
	     (set! next-row (row-opr current-row 1))
	     (let loop ()
	       (if (row-cmp-opr next-row final-row)
		   (begin	
		     (set! move-status null)
		     (set! next-sqr (row-opr current-square subtract-factor))
		     (set! last-sqr-on-row (* next-row 8))
		     (set! last-sqr-on-row (- last-sqr-on-row 1))
		     (if (and (>= next-sqr 0)
			      (<= next-sqr last-sqr-on-row))			     
			 (begin	
			   (set! move-status (can-move-or-capture chess-board
								  from-color 
								  next-sqr))
			   (if (not (eqv? move-status null))
			       (begin
				 (set! ret (append ret (list next-sqr)))
				 (set! current-row next-row)
				 (set! next-row (row-opr current-row 1))
				 (set! current-square next-sqr)				
				 (if (and (eqv? move-status 'move)
					  (not for-king))
				     (loop)))))))))
	     ret))

	 (define (find-up-down chess-board 
			       current-square
			       direction 
			       for-king)
	   (let ((current-piece (get-square chess-board current-square))
		 (from-piece null)
		 (to-piece null)
		 (from-color null)
		 (to-color null)
		 (next-sqr 0)
		 (ret (list))
		 (opr +)
		 (move-status null)
		 (min-sqr 0)
		 (max-sqr 63))
	     (if (not (eqv? current-piece null))
		 (set! from-piece (square-piece current-piece)))
	     (if (not (eqv? from-piece null))
		 (set! from-color (chess-piece-color from-piece)))
	     (if (eqv? direction 'up)
		 (set! opr -))
	     (let loop ()
	       (set! move-status null)
	       (set! next-sqr (opr current-square 8))
	       (if (and (>= next-sqr 0)
			(<= next-sqr 63))
		   (begin	
		     (set! move-status (can-move-or-capture chess-board
							    from-color 
							    next-sqr))
		     (if (not (eqv? move-status null))
			 (begin
			   (set! ret (append ret (list next-sqr)))
			   (set! current-square next-sqr)
			   (if (and (eqv? move-status 'move)
				    (not for-king))
			       (loop)))))))
	     ret))

	 (define (find-left-right chess-board 
				  current-square
				  direction 
				  for-king)	      
	   (let ((current-row (find-row current-square))
		 (current-piece (get-square chess-board current-square))
		 (from-piece null)			      
		 (from-color null)
		 (next-sqr 0)
		 (ret (list))
		 (opr +)
		 (last-sqr-on-row 0)
		 (first-sqr-on-row 0)
		 (min-sqr 0)
		 (max-sqr 63)
		 (move-status null))
	     (if (not (eqv? current-piece null))
		 (set! from-piece (square-piece current-piece)))
	     (if (not (eqv? from-piece null))
		 (set! from-color (chess-piece-color from-piece)))
	     (if (eqv? direction 'left)
		 (set! opr -))
	     (set! current-row (add1 current-row))
	     (set! last-sqr-on-row (* current-row 8))
	     (set! last-sqr-on-row (sub1 last-sqr-on-row))
	     (set! first-sqr-on-row (- last-sqr-on-row 7))
	     (let loop ()
	       (set! move-status null)
	       (set! next-sqr (opr current-square 1))
	       (if (and (>= next-sqr first-sqr-on-row)
			(<= next-sqr last-sqr-on-row))
		   (begin	
		     (set! move-status (can-move-or-capture chess-board
							    from-color 
							    next-sqr))
		     (if (not (eqv? move-status null))
			 (begin
			   (set! ret (append ret (list next-sqr)))
			   (set! current-square next-sqr)
			   (if (and (eqv? move-status 'move)
				    (not for-king))
			       (loop)))))))
	     ret))

	 (define (can-move-or-capture chess-board 
				      from-color 
				      to-square)
	   (if (and (>= to-square 0)
		    (<= to-square 63))
	       (begin
		 (let ((current-piece (get-square chess-board to-square))
		       (to-piece null)			   
		       (to-color null)
		       (ret null))
		   (if (not (eqv? current-piece null))
		       (set! to-piece (square-piece current-piece)))
		   (if (eqv? to-piece null)
		       (set! ret 'move)
		       (begin
			 (set! to-color (chess-piece-color to-piece))
			 (if (not (= to-color from-color))
			     (set! ret 'capture))))
		   ret))
	       null))

	 (define (find-castle-moves chess-board
				    current-square)
	   (let ((ret (list))
		 (to-square null)
		 (to-piece null)
		 (from-square -1)
		 (opr add1)
		 (cmp-opr <)
		 (sqr-opr +)
		 (i 0)
		 (add-square #f)
		 (counter 1)
		 (col-counter 1)
		 (end-sqr 63)
		 (start-sqr 60))
	     (let loop ()
	       (if (<= counter 2)
		   (begin
		     (if (= current-square start-sqr)
			 (begin			  
			   (let row-loop ()
			     (if (<= col-counter 2)
				 (begin
				   (set! to-square (get-square chess-board end-sqr))
				   (if (not (eqv? to-square null))
				       (begin
					 (set! i (opr start-sqr))
					 (set! to-piece (square-piece to-square))))
				   (if (not (eqv? to-piece null))
				       (begin
					 (if (= (chess-piece-value to-piece) ROOK)
					     (begin
					       (let add-loop ()
						 (set! to-square (get-square chess-board i))
						 (set! to-piece (square-piece to-square))
						 (if (not (eqv? to-piece null))
						     (set! add-square #f)
						     (set! add-square #t))
						 (set! i (opr i))
						 (if (and (cmp-opr i end-sqr)
							  add-square)
						     (add-loop)))
					       (if add-square
						   (begin
						     (set! ret (append ret 
								       (list 
									(sqr-opr start-sqr 2))))
						     (set! add-square #f)))))))
				   (set! to-square null)
				   (set! to-piece null)
				   (set! i 0)
				   (set! opr sub1)
				   (set! cmp-opr >)
				   (set! sqr-opr -)
				   (if (= counter 1)
				       (set! end-sqr 56)
				       (set! end-sqr 0))
				   (set! col-counter (add1 col-counter))
				   (row-loop))))))
		     (set! counter (add1 counter))
		     (set! to-square null)
		     (set! to-piece null)
		     (set! i 0)
		     (set! sqr-opr +)
		     (set! opr add1)
		     (set! cmp-opr <)
		     (set! end-sqr 7)
		     (set! start-sqr 4)
		     (loop))))
	     ret))			  
	 
	 (define (set-move-mode! m)
	   (set! move-mode m))

	 (define (get-move-mode)
	   move-mode)

	 (define (set-move-from-square! s)
	   (set! move-from-sqr-num s))

	 (define (set-move-to-square! s)
	   (set! move-to-sqr-num s))

	 (define (get-square chess-board s)
	   (if (or (< s 0)
		   (> s 63))
	       null
	       (vector-ref chess-board s)))

	 ;; notation-map
	 (define (get-square-by-notation p)
	   (if (or (eqv? notation-map null)
		   (eqv? p null))
	       -1
	       (hash-table-get notation-map p)))
	 
	 (define (get-positions from-to)
	   (let ((len (string-length from-to))
		 (ret null))
	     (if (= len 4)
		 (begin
		   (set! ret (list (substring from-to 0 2)))
		   (set! ret (append ret (list (substring from-to 2 4))))))
	     ret))
	 
	 (define (get-notation s)
	   (vector-ref notations s))
	 
	 (define (init-notation-map)
	   (set! notation-map null)
	   (set! notation-map (make-hash-table 'equal))
	   (let ((row 8)
		 (cols #(#\a #\b #\c #\d #\e #\f #\g #\h))
		 (count 0)
		 (square-count 0)
		 (out null)
		 (key null))
	     (let loop ()
	       (let inner-loop ()
		 (set! out (open-output-string))
		 (fprintf out "~a~a" (vector-ref cols count) row)
		 (set! key (get-output-string out))
		 (hash-table-put! notation-map key square-count)
		 (vector-set! notations square-count key)
		 (set! out null)
		 (set! square-count (add1 square-count))
		 (set! count (add1 count))
		 (if (< count 8)
		     (inner-loop)))
	       (set! count 0)
	       (set! row (sub1 row))
	       (if (>= row 1)
		   (loop)))))

	 (export find-row
		 move-by-squares
		 load-images
		 init-chess-board
		 make-chess-piece
		 chess-piece-value
		 set-chess-piece-value!
		 chess-piece-color
		 set-chess-piece-color!
		 make-dimension
		 dimension-x
		 set-dimension-x!
		 dimension-y
		 set-dimension-y!
		 dimension-w
		 set-dimension-w!
		 dimension-h
		 set-dimension-h!
		 make-square
		 square-color
		 set-square-color!
		 square-piece
		 set-square-piece!
		 square-dim
		 set-square-dim!
		 square-selected
		 set-square-selected!
		 set-move-mode!
		 get-move-mode
		 set-move-from-square!
		 set-move-to-square!
		 get-piece-image
		 make-move
		 init-notation-map
		 get-square-by-notation
		 get-positions
		 add-move
		 add-move-by-notation
		 reset-moves
		 get-moves))