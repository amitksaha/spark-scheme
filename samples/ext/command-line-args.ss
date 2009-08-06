(import (util))

(define args (argv-list))
(if (<= (length args) 0)
    (printf "Usage: spark command-line-args.ss <arguments>~n")
    args)
