(import (airglow))

(load "consts.ss")
(load "utils.ss")
(load "board.ss")

(import ((lib "file.ss")) 
	(sql-sqlite) (util)
	(board) (chess-utils))

(define WIDTH 480)
(define HEIGHT 520)

(define main-window (window 'x 300 'y 400 
 			    'w WIDTH 'h HEIGHT 
 			    'title "Chess" 
 			    'type 'double))
(define main-menu (menu 'w WIDTH 'h 30))

(menu-add main-menu "&File" 'flags 'submenu)
(menu-add main-menu "&File/&New Game" 'shortcut "Ctrl+n")
(menu-add main-menu "&File/&Open Game..." 'shortcut "Ctrl+o" 'flags 'divider)
(menu-add main-menu "&File/&Save Game" 'shortcut "Ctrl+s" 'flags 'divider)
(menu-add main-menu "&File/&Quit" 'shortcut "Ctrl+q")
(menu-add main-menu "&Help" 'flags 'submenu)
(menu-add main-menu "&Help/&Manual" 'shortcut "Ctrl+h" 'flags 'divider)
(menu-add main-menu "&Help/&About...")

(board-init)
(define board (widget 'super 'widget 
  		      'y 30
  		      'w WIDTH 'h WIDTH
  		      'draw board-draw
  		      'events board-events))

(group-finish main-window)
(window-show main-window)

(define file-name "")

(define (save-file)
  (let ((moves (get-moves)))
     (if (> (length moves) 0)
 	(begin
	  (let ((file (db)))
	    (db-open file file-name)
	    (db-execute file "CREATE TABLE moves (move TEXT)")
	    (let loop ()
	      (if (not (eqv? moves null))
		  (begin
		    (let ((out (open-output-string)))
		      (fprintf out "INSERT INTO moves VALUES ('~a')"
			       (car moves))
		      (db-execute file (get-output-string out)))
		    (set! moves (cdr moves))
		    (loop))))		    
	    (db-close file))))))

(define run #f)

(define (play)
  (let ((moves (get-moves)))
    (let loop ()
      (if (not (eqv? moves null))
	  (begin
	    (board-move board (car moves))
	    (sleep 1)
	    (set! moves (cdr moves))
	    (if run
		(loop))))))
    (airglow-thread-manager-stop))

(define (open-file)
  (let ((file (db)) (stmt null))
    (db-open file file-name)
    (set! stmt (db-create-statement file "SELECT * FROM moves"))
    (if (not (eqv? stmt null))
	(begin
	  (reset-moves)
	  (let loop ()
	    (if (result-next stmt)
		(begin
		  (add-move-by-notation (result-string stmt 0))
		  (loop))))))
    (db-close file)
    (airglow-thread-manager-start)
    (set! run #t)
    (thread play)))

;; menu callbacks
(define (quit-cb m p)
  (exit))

(define (new-cb m p)
  (set! run #f)
  (reset-moves)
  (board-init)
  (widget-redraw board))

(define (save-cb m p)
  (let ((newfile null))
    (set! newfile (file-chooser-show "Save File As?" "*"
				     file-name))
    (if (not (eqv? newfile null))
	(begin
	  (set! file-name newfile)
	  (save-file)))))

(define (open-cb m p)
  (let ((newfile (file-chooser-show "Open File?" "*" file-name)))
    (if (not (eqv? newfile null))
	(begin
	  (set! file-name newfile)
	  (open-file)))))

(define help-dlg null)

(define (manual-cb m p)
  (if (eqv? help-dlg null)
      (begin
	(set! help-dlg (help-dialog))
	(help-dialog-load-url help-dlg "./docs/index.html")))
  (help-dialog-show help-dlg))

(define (about-cb m p)
  (let ((out (open-output-string)))
    (fprintf out "A Chess playing program written in Spark.~nCopyright (C) 2008 Vijay Mathew Pandyalakal.")
    (ask-message (get-output-string out))))
    
(menu-callback! main-menu "&File/&Quit" quit-cb)
(menu-callback! main-menu "&File/&Save Game" save-cb)
(menu-callback! main-menu "&File/&Open Game..." open-cb)
(menu-callback! main-menu "&File/&New Game" new-cb)
(menu-callback! main-menu "&Help/&Manual" manual-cb)
(menu-callback! main-menu "&Help/&About..." about-cb)

(airglow-run)
