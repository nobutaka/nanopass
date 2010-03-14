(add-tests-with-string-output "byte string"
  [(string-size (make-byte-string 0)) => "0\n"]
  [(string-size (make-byte-string 1)) => "1\n"]
  [(string-size (make-byte-string 1000)) => "1000\n"]
  [(let ([s (make-byte-string 1)])
     (string-byte-set! s 0 12)
     (string-byte-ref s 0)) => "12\n"]
  [(let ([s (make-byte-string 1)])
     (string-byte-set! s 0 12)
     (string-byte-set! s 0 13)
     (string-byte-set! s 0 (string-byte-ref s 0))
     (string-byte-ref s 0)) => "13\n"]
  [(let ([s (make-byte-string 2)])
     (string-byte-set! s 1 13)
     (string-byte-set! s 0 12)
     (string-byte-ref s 1)) => "13\n"]
  [(let ([s (make-byte-string 2)])
     (string-byte-set! s 0 12)
     (string-byte-set! s 1 13)
     (string-byte-ref s 0)) => "12\n"]
  [(let ([s (make-byte-string 1)])
     (string-byte-set! s 0 255)
     (string-byte-ref s 0)) => "255\n"]
  [(let ([s (make-byte-string 1000)])
     (string-byte-set! s 500 12)
     (string-byte-ref s 500)) => "12\n"]
  [(string->sz "abc") => "\"abc\\0\"\n"]
)

(add-tests-with-string-output "string-fx-set! and ref"
  [(let ([s (make-byte-string 4)])
     (string-fx-set! s 0 12)
     (string-fx-ref s 0)) => "12\n"]
  [(let ([s (make-byte-string 8)])
     (string-fx-set! s 4 13)
     (string-fx-set! s 0 12)
     (list (string-fx-ref s 0) (string-fx-ref s 4))) => "(12 13)\n"]
  [(let ([s (make-byte-string 4)])
     (string-fx-set! s 0 61183) ; 0xeeff
     (list (string-byte-ref s 0) (string-byte-ref s 1))) => "(255 238)\n"]  ; 0xff 0xee
)

(add-tests-with-string-output "string4"
  [(string4->fx (fx->string4 12)) => "12\n"]
  [(string4? 0) => "#f\n"]
  [(string4? #f) => "#f\n"]
  [(string4? '()) => "#f\n"]
  [(string4? "s") => "#f\n"]
  [(string4? (make-byte-string 4)) => "#f\n"]
  [(let ([s (make-byte-string 4)])
     (mutate-to-string4! s)
     (string4? s)) => "#t\n"]
  [(string4? (fx->string4 12)) => "#t\n"]
)

(add-tests-with-string-output "bytevector"
  [(bytevector? 0) => "#f\n"]
  [(bytevector? #f) => "#f\n"]
  [(bytevector? '()) => "#f\n"]
  [(bytevector? "s") => "#f\n"]
  [(bytevector? (make-byte-string 1)) => "#f\n"]
  [(bytevector? (make-bytevector 1)) => "#t\n"]
)

(add-tests-with-string-output "let*"
  [(let* ([a 100] [b a] [c (cons a b)]) c) => "(100 . 100)\n"]
)
