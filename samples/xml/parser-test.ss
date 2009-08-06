(import (xml))

(define (print-ht key value)
  (printf "~a:~a," key value))

;; Callbacks

(define (start-element user-data element attributes)
  (printf "~a <~a>" user-data element)
  (if (> (hash-table-count attributes) 0)
      (begin
	(newline)
	(hash-table-for-each attributes print-ht)))
  (newline))

(define (end-element user-data element)
  (printf "~a </~a>~n" user-data element))

(define (text-handler user-data text)
  (printf "~a ~a~n" user-data text))
  
(define parser (xml-parser))
(xml-parser-element-handlers! parser start-element end-element)
(xml-parser-special-handler! parser text-handler 'character-data)
(xml-parser-user-data! parser "MyParser")
(xml-parser-parse parser "<persons version=\"1\">")
(xml-parser-parse parser "<name>Nickey</name>")
(xml-parser-parse parser "<age>29</age>")
(xml-parser-parse parser "<name>Tony</name>")
(xml-parser-parse parser "<age>18</age>")
(xml-parser-parse parser "</persons>")
(xml-parser-parse parser "")
(xml-parser-parse parser null)
