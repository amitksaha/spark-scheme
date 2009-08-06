(import (pregexp) (airglow))

(define changed #f)
(define filename "")
(define replace-dlg null)
(define replace-find null)
(define replace-with null)
(define replace-all null)
(define replace-next null)
(define replace-cancel null)

;; syntax highlighting
(define style-table (list
		     (list 'black 'courier 14) ;; A - text
		     (list 'red 'courier 14) ;; B - numbers
		     (list 'blue 'courier-italic 14) ;; C - URLs
		     (list 'blue 'courier-bold 14) ;; D - Scheme keywords
		     (list 'green 'courier 14))) ;; E - Quotes


;; List of known Scheme special values...
(define (special-value? v)
  (or (string=? v "#t") (string=? v "#f") (string=? v "null")))

;; '(compare-keywords)' - Compare two keywords...
(define (compare-keywords a b)
  (string=? a b))

(define editor-window (window 'x 300 'y 300 
			      'w 660 'h 400 
			      'title "Editor"))
(define menu (menu 'w 660 'h 30))

(menu-add menu "&File"  'flags 'submenu)
(menu-add menu "&File/&New" 'shortcut "Ctrl+n")
(menu-add menu "&File/&Open..." 'shortcut "Ctrl+o")
(menu-add menu "&File/&Insert..." 'shortcut "Ctrl+i" 'flags 'divider)
(menu-add menu "&File/&Save" 'shortcut "Ctrl+s")
(menu-add menu "&File/Save &As..." 'shortcut "Ctrl+a" 'flags 'divider)
(menu-add menu "&File/&Quit" 'shortcut "Ctrl+q")
(menu-add menu "&Edit" 'flags 'submenu)
(menu-add menu "&Edit/Cu&t" 'shortcut "Ctrl+x")
(menu-add menu "&Edit/&Copy" 'shortcut "Ctrl+c")
(menu-add menu "&Edit/&Paste" 'shortcut "Ctrl+v")
(menu-add menu "&Edit/&Delete" 'shortcut "Ctrl+d")
(menu-add menu "&Search" 'flags 'submenu)
(menu-add menu "&Search/&Find..." 'shortcut "Ctrl+f")
(menu-add menu "&Search/F&ind Again" 'shortcut "Ctrl+g")
(menu-add menu "&Search/&Replace..." 'shortcut "Ctrl+r")
(menu-add menu "&Search/Re&place Again" 'shortcut "Ctrl+t")
(menu-add menu "&Help" 'flags 'submenu)
(menu-add menu "&Help/&About...")


(define (check-save)
  (let ((ret #t))
    (if changed
	(begin
	  (let ((r 0))
	    (set! r (ask-choice "The current file has not been saved.\nWould you like to save it now?"
				 "Cancel" "Save" "Don't Save"))

	    (cond
	     ((= r 1)
	      (save-cb null null)
	      (set! ret (not changed)))
	     ((= r 2)
	      (set! ret #t))))))
    ret))

(define (save-file file-name)
  (if (text-buffer-save-to-file txt-buffer file-name)
    (set! filename file-name)
    (ask-message "Error writing to file." 'alert))
  (set! changed #f)
  (text-buffer-call-modify-callbacks txt-buffer))

(define (load-file newfile pos)
  (let ((insert #f) (r 0))
    (cond
     ((not (= pos -1)) (set! insert #t)))
    (set! changed insert)
    (if (not insert)
	(begin
	  (set! filename "")
	  (set! r (text-buffer-load-from-file txt-buffer newfile)))
	(set! r (text-buffer-insert-file txt-buffer pos newfile)))
    (if r
	(begin
	  (if (not insert)
	      (set! filename newfile)))
	(ask-message "Error reading from file." 'alert))
    (text-buffer-call-modify-callbacks txt-buffer)))

(define (new-cb m p)
  (if (check-save)
      (begin
	(set! filename "")
	(text-buffer-select txt-buffer 0 
			     (text-buffer-size txt-buffer))
	(text-buffer-remove-selection txt-buffer)
	(set! changed #f)
	(text-buffer-call-modify-callbacks txt-buffer))))

(define (open-cb m p)
  (if (check-save)
      (begin
	(let ((newfile (file-chooser-show "Open File?" "*" filename)))
	  (if (not (eqv? newfile null))
	      (load-file newfile -1))))))

(define (save-cb m p)
  (if (string=? filename "")
      (save-as-cb m p)
      (save-file filename)))

(define (save-as-cb m p)
  (let ((newfile ""))
    (set! newfile (file-chooser-show "Save File As?" "*"
				     filename))
    (if (not (eqv? newfile null))
	(save-file newfile))))

(define (insert-cb m p)
    (let ((newfile (file-chooser-show "Insert File?"
				       "*" filename)))
      (if (not (eqv? newfile null))
	  (load-file newfile (text-editor-insert-position editor)))))

(define (quit-cb m p)
  (check-save)
  (exit))

(define (cut-cb m p)
  (text-editor-key-event editor 0 'cut))

(define (copy-cb m p)
  (text-editor-key-event editor 0 'copy))

(define (paste-cb m p)
  (text-editor-key-event editor 0 'paste))

(define (delete-cb m p)
  (text-buffer-remove-selection txt-buffer))

(define search-str "")

(define (find-cb m p)
  (let ((val (ask-input "Search String: " search-str)))
    (if (not (eqv? val null))
	(begin
	  (set! search-str val)
	  (find2-cb m p)))))

(define (find2-cb m p)
  (if (= (string-length search-str) 0)
      (find-cb m p)
      (begin
	(let* ((pos (text-editor-insert-position editor))
	       (found (text-buffer-search txt-buffer search-str
					   pos)))
	  (if (>= found 0)
	      (begin
		(text-buffer-select txt-buffer found
				     (+ found
					(string-length search-str)))
		(text-editor-insert-position! editor 
						(+ found (string-length search-str)))
		(text-editor-show-insert-position editor)))))))

(define (replace-cb m p)
  (if (eqv? replace-dlg null)
      (create-replace-dialog))
  (window-show replace-dlg))

(define (about-cb m p)
  (print "Airglow Editor") (newline))

(define (replace2-cb b p)
  (if (eqv? replace-dlg null)
      (create-replace-dialog))
  (let ((find (input-field-value replace-find))
	(replace (input-field-value replace-with)))
    (if (string=? find "")
	(window-show replace-dlg)
	(begin
	  (window-hide replace-dlg)
	  (text-editor-insert-position! editor 0)
	  (let ((times 0) (found 0) (pos 0))
	    (let loop ()
	      (set! pos (text-editor-insert-position editor))
	      (set! pos (text-buffer-search txt-buffer find pos))
	      (if (> pos 0)
		  (begin
		    (text-buffer-select txt-buffer pos (+ pos (string-length find)))
		    (text-buffer-remove-selection txt-buffer)
		    (text-buffer-insert txt-buffer pos replace)
		    (text-editor-insert-position! editor (+ pos (string-length replace)))
		    (text-editor-show-insert-position editor)
		    (set! times (+ times 1))
		    (set! pos (text-editor-insert-position editor))
		    (set! pos (text-buffer-search txt-buffer find pos))
		    (if (eqv? p 'all)
			(loop)))))
	    (if (> times 0)
		(begin 
		  (let ((s (format "Replaced ~a occurences." times)))
		    (ask-message s)))
		(ask-message "No occurences found.")))))))

(menu-callback! menu "&File/&New" new-cb)
(menu-callback! menu "&File/&Open..." open-cb)
(menu-callback! menu "&File/&Save" save-cb)
(menu-callback! menu "&File/Save &As..." save-as-cb)
(menu-callback! menu "&File/&Insert..." insert-cb)
(menu-callback! menu "&File/&Quit" quit-cb)
(menu-callback! menu "&Edit/Cu&t" cut-cb)
(menu-callback! menu "&Edit/&Copy" copy-cb)
(menu-callback! menu "&Edit/&Paste" paste-cb)
(menu-callback! menu "&Edit/&Delete" paste-cb)
(menu-callback! menu "&Search/&Find..." find-cb)
(menu-callback! menu "&Search/F&ind Again" find2-cb)
(menu-callback! menu "&Search/&Replace..." replace-cb)
(menu-callback! menu "&Search/Re&place Again" replace2-cb)
(menu-callback! menu "&Help/&About..." about-cb)

(define editor (text-editor 'y 30 'w 660 'h 370 'type 'editor))
(define txt-buffer (text-buffer 1024))
(text-editor-buffer! editor txt-buffer)
(define style-buffer (text-editor-create-style-buffer editor))
(text-editor-highlight-data editor 
			      style-buffer 
			      style-table
			      null null)

(define regexs (list
		(list (pregexp "[-+]?[0-9]*\\.?[0-9]+") #\B)
		(list (pregexp "(http|https|ftp)\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(:[a-zA-Z0-9]*)?/?([a-zA-Z0-9\\-\\._\\?\\,\\'/\\\\\\+&amp;%\\$#\\=~])*[^\\.\\,\\)\\(\\s]$") #\C)
		(list (pregexp "define") #\D)
		(list (pregexp "\"((\\\")|[^\"(\\\")])+\"") #\E)))

(define (style-parse text style)
  (let* ((positions null) (pos null)
	 (rs regexs) (r (car rs)))
    (let r-loop ()
      (if (not (eqv? r null))
	  (begin
	    (set! positions (pregexp-match-positions (car r) text))
	    (if (list? positions)
		(begin
		  (set! pos (car positions))
		  (let loop ()
		    (if (and (not (eqv? pos #f))
			     (not (eqv? pos null)))
			(begin
			  (let ((start 0) (end 0))
			    (set! start (car pos))
			    (set! end (cdr pos))
			    (let inner-loop ()
			      (if (> end start)
				  (begin
				    (string-set! style start (car (cdr r)))
				    (set! start (+ start 1))
				    (inner-loop)))))
			  (if (not (eqv? positions null))
			      (begin
				(set! positions (cdr positions))
				(if (not (eqv? positions null))
				    (begin
				      (set! pos (car positions))
				      (loop))))))))))
	    (set! rs (cdr rs))
	    (if (not (eqv? rs null))
		(set! r (car rs))
		(set! r null))
	    (r-loop))))))

(define (changed-cb pos inserted deleted restyled
		    deleted-text cb-arg)
  (if (or (> inserted 0)
	  (> deleted 0))
      (set! changed #t)))

(define (style-update pos inserted deleted restyled
		      deleted-text cb-arg)
   (if (and (= inserted 0)
 	   (= deleted 0))
       (text-buffer-unselect style-buffer)
       (begin
 	(let ((start 0) (end 0) (last 0) (text "") (style ""))
 	  (if (> inserted 0)
 	      (begin
 		(set! style (make-string inserted #\A))
 		(text-buffer-replace style-buffer pos 
 				      (+ pos deleted) style))
 	      (text-buffer-delete style-buffer pos (+ pos deleted)))
 	  (text-buffer-select style-buffer pos
			      (- (+ pos inserted) deleted))
 	  (set! start (text-buffer-line-start txt-buffer pos))
 	  (set! end (text-buffer-line-end txt-buffer 
					  (+ pos inserted)))
 	  (set! text (text-buffer-text txt-buffer start end))
 	  (set! style (string-append "" 
 				     (text-buffer-text style-buffer 
						       start end)))
	  (style-parse text style)
 	  (if (= start end)
 	      (set! last 0)
 	      (set! last (- (- end start) 1)))
 	  (text-buffer-replace style-buffer start end style)
 	  (text-editor-redisplay-range editor start end)))))

(text-buffer-add-modify-callback txt-buffer style-update null)
(text-buffer-add-modify-callback txt-buffer changed-cb null)
(widget-callback! editor-window quit-cb null)

(define (replace-can-cb b p)
  (window-hide replace-dlg))

(define (create-replace-dialog)
  (set! replace-dlg (window 'x 300 'y 400
			    'w 300 'h 105
			    'title "Replace"))
  (set! replace-find (input-field 'x 80 'y 10
				  'w 210 'h 25
				  'title "Find: "))
  (set! replace-with (input-field 'x 80 'y 40
				  'w 210 'h 25
				  'title "Replace: "))
  (set! replace-all (button 'x 10 'y 70
			    'w 90 'h 25
			    'title "Replace All"))
  (set! replace-next (button 'x 105 'y 70
			     'w 120 'h 25
			     'title "Replace Next"
			     'type 'return))
  (set! replace-cancel (button 'x 230 'y 70
			       'w 60 'h 25
			       'title "Cancel"))
  
  (widget-label-align! replace-find 'left)
  (widget-label-align! replace-with 'left)
  (widget-callback! replace-all replace2-cb 'all)
  (widget-callback! replace-next replace2-cb 'single)
  (widget-callback! replace-cancel replace-can-cb)
  (group-finish replace-dlg)
  (window-modal! replace-dlg #f))  

(group-resizable editor-window editor)
(group-finish editor-window)
(window-show editor-window)
(airglow-run)
