(define (tok-cb tok)
  (printf "~a~n" tok))

(define (tokenize s sep)
  (let ((t (string-tokenizer s tok-cb sep)))
    (while (t))))  

(tokenize "hello, world" null)
(tokenize "hello, world" (list #\space #\,))