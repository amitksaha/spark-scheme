(import (airglow))

(define w (window 'x 300 'y 400 'w 400 'h 300 'title "Spreadsheet" 'type 'double))
(define p null)

(define x 0) 
(define y 0)
(define first-row #t)
(define row-titles (list "A" "B" "C" "D" "E" "F" "G" "H"))
(for i in (range 15)
     (set! p (pack 'y y 'w 400 'h 20))
     (for j in (range 8)
	  (if first-row
	      (begin
		(border 'x x 'y y 'w 50 'h 20 'type 'up 'title (car row-titles))
		(set! row-titles (cdr row-titles)))
	      (begin
		(if (= j 0)
		    (border 'x x 'y y 'w 50 'h 20 'title (number->string (+ i 1)) 'type 'up)
		    (input-field 'x x 'y y 'w 50 'h 20))))
	  (set! x (+ x 50)))
     (if first-row
	 (set! first-row #f))
     (group-finish p)
     (set! x 0)
     (set! y (+ y 20)))

(group-finish w)
(window-show w)
(airglow-run)