(import (airglow) (util))

(define args (argv-list))
(define file-name null)
(if (<= (length args) 0)
    (begin
      (printf "Usage: spark browser.ss file-name~n")
      (exit))
    (set! file-name (car args)))

(define my-browser null)
(define top null)
(define bottom null)
(define middle null)
(define visible null)
(define swap null)
(define line-no null)

(define (scroll b a)
  (cond 
   ((eqv? b top) (browser-scroll my-browser 
				 (string->number 
				  (input-field-value line-no))
				 'top))
   ((eqv? b bottom) (browser-scroll my-browser 
				    (string->number 
				     (input-field-value line-no))
				    'bottom))
   ((eqv? b middle) (browser-scroll my-browser 
				    (string->number 
				     (input-field-value line-no))
				    'middle))
   ((eqv? b visible) (browser-make-line-visible my-browser 
						(string->number 
						 (input-field-value line-no))))))

(define (swap-lines b a)
  (let ((count (browser-count my-browser))
	(line-01 -1) (line-02 -1) (break #f))
    (let loop ((i 0))
      (if (browser-selected? my-browser i)
	  (begin 
	    (if (= line-01 -1)
		(set! line-01 i)
		(begin
		  (set! line-02 i)
		  (set! break #t)))))
      (set! i (+ i 1))
      (if (>= i count)
	  (set! break #t))
      (if (not break)
	  (loop i)
	  (browser-swap my-browser line-01 line-02)))))

(define main-window 
  (window 'w 400 'h 400 'title "Text Browser"))

(set! my-browser (browser 'w 400 'h 350 
			  'type 'select))
(browser-type! my-browser 'multi)
(browser-load-file my-browser file-name)
(browser-vscroll-position! my-browser 0)
(group-resizable main-window my-browser)

(set! line-no (input-field 'x 50 'y 350 'w 350 'h 25 'title "Line #:"))

(set! top (button 'y 375 'w 80 'h 25 'title "Top"))
(widget-callback! top scroll)
(set! bottom (button 'x 80 'y 375 'w 80 'h 25 'title "Bottom"))
(widget-callback! bottom scroll)
(set! middle (button 'x 160 'y 375 'w 80 'h 25 'title "Middle"))
(widget-callback! middle scroll)
(set! visible (button 'x 240 'y 375 'w 80 'h 25 'title "Make Vis."))
(widget-callback! visible scroll)
(set! swap (button 'x 320 'y 375 'w 80 'h 25 'title "Swap"))
(widget-callback! swap swap-lines)

(group-finish main-window)
(window-show main-window)
(airglow-run)
