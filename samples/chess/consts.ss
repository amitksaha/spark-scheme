(library chess-consts
	 
	 (define RANKS-COUNT 8)
	 (define FILES-COUNT 8)
	 (define SQUARE-COUNT (* RANKS-COUNT FILES-COUNT))

	 ;; piece ids
	 (define KING 100)
	 (define QUEEN 90)
	 (define ROOK 50)
	 (define BISHOP 40)
	 (define KNIGHT 30)
	 (define PAWN 10)
	 (define EMPTY 0)
	 
	 ;; colors
	 (define BLACK 0)
	 (define WHITE 1)
	 
	 (export RANKS-COUNT
		 FILES-COUNT
		 SQUARE-COUNT
		 KING
		 QUEEN
		 ROOK
		 BISHOP
		 KNIGHT
		 PAWN
		 EMPTY
		 BLACK
		 WHITE))
