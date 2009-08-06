(import (ffi))

(unsafe!)

(define lib (ffi-lib "/home/vijay/Desktop/spark-samples/libmy"))

(define hello-from-c (get-ffi-obj "hello_from_c" lib (_fun -> _void)
				  (lambda () (error "Library does not provide \"hello_from_c\""))))

(hello-from-c)

(define add-ints (get-ffi-obj "add_ints" lib (_fun _int _int -> _int)
			      (lambda () (error "Library does not provide \"add_ints\""))))
(printf "add_ints(10, 20) = ~a~n" (add-ints 10 20))

(define print-str (get-ffi-obj "print_str" lib (_fun _string -> _uint32)
			       (lambda () (error "Library does not provide \"print_str\""))))

(printf "print_str(\"hello, world\") = ~a~n" (print-str "hello, world"))

(define make-point (get-ffi-obj "make_point" lib (_fun _int _int -> _pointer)
				 (lambda () (error "Library does not provide \"make_point\""))))

(define p (make-point 100 200))
(printf "make_point(100, 200) = ~a~n" (ptr-ref p (_list-struct _int _int)))

(define make-3d-point (get-ffi-obj "make_3d_point" lib (_fun _pointer _int -> _pointer)
				   (lambda () (error "Library does not provide \"make_3d_point\""))))

(printf "make_3d_point(Point(100, 200), 10) = ~a~n" (ptr-ref (make-3d-point p 10) (_list-struct (_list-struct _int _int) _int)))


