(import (airglow))

(define w (window 'w 450 'h 80 'title "SpecialDialogs"))

(define b (button 'x 5 'y 5 'w 100 'h 30 'title "File Chooser"))

(widget-callback! b (lambda (b p)
		      (let ((newfile (file-chooser-show "Open File?" "*" "")))
			(if (not (null? newfile))
			    (ask-message newfile 'info)))))

(set! b (button 'x 110 'y 5 'w 100 'h 30 'title "Help Dialog"))
(widget-callback! b (lambda (b p)
		      (let ((help-dlg (help-dialog)))
			(help-dialog-load-url help-dlg "help.html")
			(help-dialog-show help-dlg))))

(define text "")
(set! b (button 'x 215 'y 5 'w 100 'h 30 'title "Ask for text"))
(widget-callback! b (lambda (b p)
		      (set! text (ask-input "Enter some text: " text))
		      (if (not (null? text))
			  (ask-message text 'info))))

(define pswd "")
(set! b (button 'x 330 'y 5 'w 120 'h 30 'title "Ask for password"))
(widget-callback! b (lambda (b p)
		      (set! pswd (ask-password "Enter password: " pswd))
		      (if (not (null? pswd))
			  (ask-message pswd 'info))))

(group-finish w)
(window-show w)
(airglow-run)