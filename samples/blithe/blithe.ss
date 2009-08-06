;; Blithe - A simple, stack-based, concatenative language.
;; Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.

;; Please contact Vijay Mathew Pandyalakal if you need additional 
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

;; TODO:
;;   Docs for default functions.
;;   whileFalse
;;   New name - Nimble ??

(define data-stack (stack))
(define system-words (make-hash-table 'equal))
(define variables (make-hash-table))
(define vocabulary 'system)
(define vocabs (make-hash-table 'equal))
(define docs (make-hash-table))
(define shell-started #f)
(define default-input-port (current-input-port))
(define input-port default-input-port)
(define history (list))
(define default-image-file-name "blithe.image")
(define image-file default-image-file-name)
(define load-mode #f)
(define not-scratch #t)
(define last-word "")

(define (interpret args)
  (let ((str-mode #f) 
	(str null)
	(token null))
    (let loop ()
      (cond 
       ((not (null? args))
	(set! token (car args))
	(set! args (cdr args))
	(cond	 
	 ((= (string-length token) 0) (loop))
	 ((= (string-find token "\"") 0)
	  (if (string-end token "\"")
	      (stack-push! data-stack (trim-string token))
	      (begin
		(set! str-mode #t)
		(set! str token))))
	 (str-mode
	  (set! str (string-append str " "))
	  (set! str (string-append str token))
	  (if (string-end token "\"")
	      (begin
		(stack-push! data-stack (trim-string str))
		(set! str null)
		(set! str-mode #f))))
	 ((not (handle-value token))
	  (set! token (string-downcase token))
	  (cond 
	   ((or (string=? token "iftrue")
		(string=? token "iffalse"))
	    (if (check-condition (string->symbol token))
		(begin
		  (interpret (list (car args)))
		  (set! args (cdr args))
		  (if (not (null? args))
		      (if (string=? (string-downcase (car args)) "else")
			  (set! args (cddr args)))))
		
		(begin
		  (set! args (handle-else args)))))
	   ((string=? token "times")
	    (set! token (string-downcase (car args)))
	    (for i in (range (stack-pop! data-stack))
		 (interpret (list token)))
	    (set! args (cdr args)))
	   ((string=? token "whiletrue")
	    (set! token (string-downcase (car args)))
	    (while (stack-pop! data-stack)
		   (interpret (list token)))
	    (set! args (cdr args)))
	   ((string=? token "null")
	    (stack-push! data-stack null))
	   (else
	    (execute token)))))
	(loop))))))

(define (interpret-word-def args)
  (let ((word (car args))
	(args (cdr args)))
    (let loop ((s (car args))
	       (rest (cdr args))
	       (body (list)))
      (if (not (string=? s ";"))
	  (loop (car rest)
		(cdr rest)
		(cons s body))
	  (begin
	    (hash-table-put! (current-words-hash)
			     (string-downcase word)
			     (reverse body))
	    (say-ok))))))
    
(define (execute word)
  (if (not (execute-word-from-vocabulary word))
  (cond 
    ((string=? word "+") 
     (if (or (string? (stack-ref data-stack 0))
	     (string? (stack-ref data-stack 1)))
	 (strcat)
	 (do-arith +)))
    ((string=? word "-") (do-arith -))
    ((string=? word "*") (do-arith *))
    ((string=? word "/") (do-arith /))
    ((string=? word "mod") (do-arith modulo))
    ((string=? word "bw-or") (do-arith bitwise-ior))
    ((string=? word "bw-xor") (do-arith bitwise-xor))
    ((string=? word "bw-and") (do-arith bitwise-and))
    ((string=? word "bw-not") (bw-not))
    ((string=? word "shift") (do-arith arithmetic-shift))
    ((string=? word "=") (do-comp =))
    ((string=? word ">") (do-comp >))
    ((string=? word "<") (do-comp <))
    ((string=? word ">=") (do-comp >=))
    ((string=? word "<=") (do-comp <=))
    ((string=? word "and") (do-and))
    ((string=? word "or") (do-or))
    ((string=? word "not") (do-not))
    ((string=? word "!") (put-variable))
    ((string=? word "@") (get-variable))
    ((string=? word "pair") (pair))
    ((string=? word "first") (first))
    ((string=? word "second") (second))
    ((string=? word "type?") (type?))
    ((string=? word "strlen") (strlen))
    ((string=? word "strref") (strref))
    ((string=? word "substr") (substr))
    ((string=? word "strcat") (strcat))
    ((string=? word "strlcase") (strlcase))
    ((string=? word "strucase") (strucase))
    ((string=? word "depth") (len))
    ((string=? word "pick") (pick))
    ((string=? word "set!") (data-stack-set!))
    ((string=? word ".") (pop-and-print))
    ((string=? word "dup") (dup))
    ((string=? word "2dup") (2dup))
    ((string=? word "swap") (swap))
    ((string=? word "2swap") (2swap))
    ((or (string=? word "^")
	 (string=? word "drop"))
	 (drop))
    ((string=? word "over") (over))
    ((string=? word "2over") (2over))
    ((string=? word "rot") (rot))
    ((string=? word "emit") (emit))
    ((string=? word "vars") (vars))
    ((string=? word "words") (words))
    ((string=? word "forget") (forget))
    ((string=? word "vocabulary") (set! vocabulary (stack-pop! data-stack)))
    ((string=? word ".s") (show-stack))
    ((string=? word "doc") (doc))
    ((string=? word "help") (help))
    ((string=? word "import") (import-file (stack-pop! data-stack)))
    ((string=? word "save") (save))
    ((string=? word "load") (load))
    ((string=? word "phrase") (show-def))
    ((string=? word "call") (exec-word))
    ((string=? word "abort") (abort-repl))
    ((string=? word "<<") (set! not-scratch #f))
    ((string=? word ">>") (set! not-scratch #t))
    ((string=? word "sleep") (sleep (stack-pop! data-stack)))
    ((string=? word "read") (stack-push! data-stack (read input-port)))
    ((string=? word "clear") (set! data-stack (stack)))
    ((string=? word "bye") (exit))
    (else (report-execute-error word))))
  (set! last-word word))

(define (execute-system-word word)
  (let ((body (hash-table-get system-words word null)))
    (if (not (null? body))
	(begin (interpret body) #t)
	#f)))

(define (execute-word-from-vocabulary word)
  (if (not (eq? vocabulary 'system))
      (let* ((v (hash-table-get vocabs vocabulary (make-hash-table 'equal)))
	     (body (hash-table-get v word null)))
	(if (not (null? body))
	    (begin (interpret body) #t)
	    (execute-system-word word)))
      (execute-system-word word)))

(define (current-words-hash)
  (if (eq? vocabulary 'system) 
      system-words
      (let ((v (hash-table-get vocabs vocabulary null)))
	(if (null? v)
	    (begin
	      (set! v (make-hash-table 'equal))	    
	      (hash-table-put! vocabs vocabulary v)))
	v)))

(define (report-execute-error word)
  (let ((err (open-output-string)))
    (fprintf err "~a - word not found." word)
    (error (get-output-string err))))

(define (say-ok)
  (if (and (not load-mode)
	   shell-started
	   (eq? input-port default-input-port))
      (begin
	(display " -> ok") 
	(newline))))

(define (check-condition c)
  (let ((b (stack-pop! data-stack)))
    (if (eq? c 'iftrue) 
	(eq? b #t)
	(eq? b #f))))

(define (trim-string token)
  (if (> (string-length token) 1)
      (substring token 1 (sub1 (string-length token)))
      token))

(define (handle-value token)
  (handle-number token))

(define (handle-number token)
  (let ((num (string->number token)))
    (if (number? num)
	(begin
	  (stack-push! data-stack num)
	  #t)
	(handle-boolean token))))

(define (handle-boolean token)
  (if (or (string=? token "#t")
	  (string=? token "#f"))
      (begin
	(stack-push! data-stack (string->boolean token))
	#t)
      (handle-symbol token)))

(define (handle-symbol token)
  (if (and (> (string-length token) 1)
	   (= (string-find token "'") 0))
      (let ((sym (string->symbol (substring token 1))))
	(stack-push! data-stack sym)
	#t)
      #f))

(define (put-variable)
  (let ((sym (stack-pop! data-stack))
	(val (stack-pop! data-stack)))
    (if (symbol? sym)
	(hash-table-put! variables sym val)
	(error "Not a symbol."))))

(define (get-variable)
  (let ((sym (stack-pop! data-stack)))
    (if (symbol? sym)
	(let ((val (hash-table-get variables sym null)))
	  (if (not (null? val))
	      (stack-push! data-stack val)
	      (error "Variable not defined.")))
	(error "Not a symbol."))))

(define (exec-word)
  (let ((fname (stack-pop! data-stack)))
    (if (symbol? fname)
	(set! fname (symbol->string fname)))
    (execute fname)))

(define (pair)
  (stack-push! data-stack (cons (stack-pop! data-stack)
				(stack-pop! data-stack))))

(define (first)
  (stack-push! data-stack (car (stack-pop! data-stack))))

(define (second)
  (stack-push! data-stack (cdr (stack-pop! data-stack))))

(define (drop)
  (stack-pop! data-stack))

(define (over)
  (let ((first (stack-pop! data-stack))
	(second (stack-pop! data-stack)))
    (stack-push! data-stack second)
    (stack-push! data-stack first)
    (stack-push! data-stack second)))

(define (rot)
  (let ((first (stack-pop! data-stack))
	(second (stack-pop! data-stack))
	(third (stack-pop! data-stack)))
    (stack-push! data-stack second)
    (stack-push! data-stack first)
    (stack-push! data-stack third)))

(define (type?)
  (let ((v (stack-top data-stack))
	(type 'number))
    (cond
     ((integer? v) (set! type 'integer))
     ((real? v) (set! type 'real))
     ((boolean? v) (set! type 'boolean))
     ((symbol? v) (set! type 'symbol))
     ((string? v) (set! type 'string))
     ((pair? v) (set! type 'pair)))
    (stack-push! data-stack type)))

(define (handle-else args)
  (cond 
   ((not (null? args))
    (set! args (cdr args))
    (cond 
     ((not (null? args))
      (cond 
       ((string=? (string-downcase (car args)) "else")
	(set! args (cdr args))
	(execute (string-downcase (car args)))
	(cdr args))
       (else args)))
     (else args)))
   (else args)))

(define (bw-not)
  (stack-push! data-stack (bitwise-not (stack-pop! data-stack))))

(define (do-arith f)
  (let ((n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
  (stack-push! data-stack (f n1 n2))))

(define (do-comp f)
  (let ((n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
  (if (string? n1)
      (do-str-comp f n1 n2)
      (begin
	(if (eq? f =) 
	    (if (or (pair? n1) (list? n1))
		(set! f equal?)
		(set! f eq?)))
	(stack-push! data-stack 
		     (f n1 n2))))))

(define (do-str-comp f s1 s2)
  (cond
    ((eq? f =) (set! f string=?))
    ((eq? f >) (set! f string>?))
    ((eq? f <) (set! f string<?))
    ((eq? f >=) (set! f string>=?))
    ((eq? f <=) (set! f string<=?)))
  (stack-push! data-stack
	       (f s1 s2)))

(define (do-and)
  (let ((n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
  (stack-push! data-stack
	       (and n1 n2))))

(define (do-or)
  (let ((n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
    (stack-push! data-stack
		 (or n1 n2))))

(define (do-not)
  (stack-push! data-stack
	       (not (stack-pop! data-stack))))

(define (strlcase)
  (stack-push! data-stack (string-downcase (stack-pop! data-stack))))

(define (strucase)
  (stack-push! data-stack (string-upcase (stack-pop! data-stack))))

(define (dup)
  (stack-push! data-stack (stack-top data-stack)))

(define (2dup)
  (let ((n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
    (stack-push! data-stack n1)
    (stack-push! data-stack n2)
    (stack-push! data-stack n1)
    (stack-push! data-stack n2)))

(define (swap)
  (let ((a (stack-pop! data-stack))
	(b (stack-pop! data-stack)))
    (stack-push! data-stack a)
    (stack-push! data-stack b)))

(define (2swap)
  (let ((n4 (stack-pop! data-stack))
	(n3 (stack-pop! data-stack))
	(n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
    (stack-push! data-stack n2)
    (stack-push! data-stack n1)
    (stack-push! data-stack n4)
    (stack-push! data-stack n3)))

(define (2over)
  (let ((n4 (stack-pop! data-stack))
	(n3 (stack-pop! data-stack))
	(n2 (stack-pop! data-stack))
	(n1 (stack-pop! data-stack)))
    (stack-push! data-stack n1)
    (stack-push! data-stack n2)
    (stack-push! data-stack n3)
    (stack-push! data-stack n4)
    (stack-push! data-stack n1)
    (stack-push! data-stack n2)))

(define (len)
  (stack-push! data-stack (stack-length data-stack)))

(define (pick)
  (let ((index (stack-pop! data-stack)))
    (stack-push! data-stack (stack-ref data-stack index))))

(define (pop-and-print)
  (if (not load-mode)
      (begin
	(printf "~a " (stack-pop! data-stack))
	(flush-output))))

(define (data-stack-set!)
  (let ((item (stack-pop! data-stack))
	(index (stack-pop! data-stack)))
    (stack-set! data-stack index item)))

(define (vars)
  (hash-table-for-each variables
		       (lambda (k v)
			 (printf "~a: ~v~n" k v))))

(define (words)
  (hash-table-for-each (current-words-hash)
		       (lambda (k v)
			 (printf "~a " k)))
  (newline))

(define (forget)
  (hash-table-remove! (current-words-hash)
		      (symbol->string (stack-pop! data-stack)))
  (say-ok))

(define (emit)
  (if (not load-mode)
      (display (integer->char (stack-pop! data-stack)))))

(define (doc)
  (let ((sym (stack-pop! data-stack))
	(d (stack-pop! data-stack)))
    (if (not (symbol? sym))
	(error "Not a symbol."))
    (hash-table-put! docs sym d)))

(define (show-def)
  (let* ((sym (stack-pop! data-stack))
	 (body (hash-table-get (current-words-hash) (symbol->string sym) null)))
    (if (not (null? body))
	(printf "~a~n" body)
	(printf "Word definition not found.~n"))))
    
(define (show-stack)
  (let ((tmp-stack (stack))
	(v null))
    (printf "~a < " (stack-length data-stack))
    (while (not (stack-empty? data-stack))
	   (set! v (stack-pop! data-stack))
	   (printf "~a " v)
	   (stack-push! tmp-stack v))
    (printf ">~n")
    (set! data-stack (stack-reverse tmp-stack))))

(define (help)
  (let ((sym (stack-pop! data-stack)))
    (stack-push! data-stack (hash-table-get docs sym "No help found."))))

(define (abort-repl)
  (printf "abort ~a~n>>>~a<<<~n" last-word (stack-pop! data-stack))
  (set! data-stack (stack))
  (error 'abort))

(define (strlen)
  (stack-push! data-stack 
	       (string-length 
		(stack-pop! data-stack))))

(define (strref)
  (stack-push! data-stack
	       (char->integer
		(string-ref (stack-pop! data-stack)
			    (stack-pop! data-stack)))))

(define (substr)
  (len)
  (case (stack-pop! data-stack)
    ((2)
     (stack-push! data-stack
		  (substring (stack-pop! data-stack)
			     (stack-pop! data-stack))))
    ((3)
     (stack-push! data-stack
		  (substring (stack-pop! data-stack)
			     (stack-pop! data-stack)
			     (stack-pop! data-stack))))
    (else (error "Expects 2 to 3 arguments."))))

(define (strcat)
  (let ((s2 (stack-pop! data-stack))
	(s1 (stack-pop! data-stack)))
    (if (integer? s1) (set! s1 (string (integer->char s1))))
    (if (integer? s2) (set! s2 (string (integer->char s2))))
  (stack-push! data-stack (string-append s1 s2))))			

(define (read-block first-line)
  (let loop ((block first-line))
    (if (>= (string-rfind block ";") 
	    (sub1 (string-length block)))
	block
	(loop (string-append (string-append block " ") 
			     (read-code-line))))))

(define (trim-colon s) (string-ltrim (substring s 1)))

(define (string-end s e)
  (if (string=? s e) 
      #t
      (begin
	(let ((idx-end (string-rfind s e)))
	  (and (not (= idx-end -1))
	       (and (= idx-end (sub1 (string-length s)))
		    (not (char=? (string-ref s (sub1 idx-end)) #\\))))))))

(define (string->boolean s)
  (if (string=? s "#t") 
      #t
      #f))

(define (set-image-file-name)
  (if (and (> (stack-length data-stack) 0)
	   (string? (stack-top data-stack)))
      (set! image-file (stack-pop! data-stack))
      (set! image-file default-image-file-name))
  (backup-file image-file))

(define (backup-file file-name)
  (let ((bak-file (open-output-string)))
    (fprintf bak-file "~a.bak" file-name)
    (try
     (copy-file file-name (get-output-string bak-file))
     (catch (lambda (ex) null)))))

(define (save)
  (if (not load-mode)
      (begin
	(set-image-file-name)
	(let ((out (open-output-file image-file 'text 'append)))
	  (set! history (reverse history))
	  (for-each (lambda (line) (fprintf out "~a~n" line))
		    history)
	  (close-output-port out)
	  (say-ok)
	  (set! history (list))))))

(define (load)
  (if (not load-mode)
      (begin
	(set-image-file-name)
	(set! load-mode #t)
	(try
	 (import-file image-file)
	 (delete-file image-file)
	 (set! load-mode #f)
	 (say-ok)
	 (catch (lambda (ex) (printf "Nothing to load.~n"))))
	(set! load-mode #f))))

(define (read-code-line)
  (let ((s (read-line input-port 'any-one)))
    (if (not (eof-object? s)) 
	(string-trim (remove-comments s))
	s)))

(define (remove-comments s)
  (let ((idx (string-find s "--")))
    (if (>= idx 0)
	(substring s 0 idx)
	s)))

(define (init-words)
  (set! input-port (open-input-file "__init__.bl"))
  (repl)
  (close-input-port input-port)
  (set! input-port default-input-port))

(define (import-file file-name)
  (let* ((old-input-port input-port)
	 (fin (open-input-file file-name))
	 (flen (file-size file-name))
	 (buff (read-string flen fin)))
    (close-input-port fin)
    (set! input-port (open-input-string buff))
    (repl)
    (set! input-port old-input-port)
    (say-ok)))
  
(define line null)

(define (repl)
  (try 
   (let loop ((line (read-code-line)))
     (if (not (eof-object? line))
	 (begin
	   (try 
	    (if (> (string-length line) 0)	      
		(if (char=? (string-ref line 0) #\:)
		    (let ((block (read-block line)))
		      (interpret-word-def (string-split 
					   (trim-colon block)))
		      (append-to-history block))
		    (begin
		      (interpret (string-split line))
		      (append-to-history line))))
	    (catch (lambda (ex) 
		     (if (eq? ex 'abort) 
			 (error ex)
			 (printf "Error: ~a~n" ex)))))
	   (loop (read-code-line)))))
   (catch (lambda (ex)
	    (if (eq? ex 'abort)
		(repl)
		(printf "Error: ~a~n" (exn-message ex)))))))

(define (append-to-history s)
  (if (and not-scratch
       shell-started
       (eq? input-port default-input-port))
      (set! history (cons s history))))
  
(init-words)

(if (and (not (null? (argv)))
	 (= (vector-length (argv)) 1))
    (begin
      (import-file (vector-ref (argv) 0))
      (exit)))
		  
(display "This is the interactive Blithe shell.") (newline)
(display "Type `bye' to exit.") (newline) 
(newline)
(set! shell-started #t)

(repl)


