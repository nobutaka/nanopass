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
)

(add-tests-with-string-output "string-int-set! and ref"
  [(let ([s (make-byte-string 4)])
     (string-int-set! s 0 12)
     (string-int-ref s 0)) => "12\n"]
  [(let ([s (make-byte-string 8)])
     (string-int-set! s 4 13)
     (string-int-set! s 0 12)
     (list (string-int-ref s 0) (string-int-ref s 4))) => "(12 13)\n"]
  [(let ([s (make-byte-string 4)])
     (string-int-set! s 0 61183) ; 0xeeff
     (list (string-byte-ref s 0) (string-byte-ref s 1))) => "(255 238)\n"]  ; 0xff 0xee
)

(add-tests-with-string-output "string-float-set! and ref"
  [(let ([s (make-byte-string 8)])
     (string-float-set! s 3 1.2)
     (string-float-ref s 3)) => "1.200000\n"]
)

(add-tests-with-string-output "box"
  [(box->fx (fx->box 12)) => "12\n"]
  [(box? 0) => "#f\n"]
  [(box? #f) => "#f\n"]
  [(box? '()) => "#f\n"]
  [(box? "s") => "#f\n"]
  [(box? (make-byte-string 4)) => "#f\n"]
  [(box? (make-box)) => "#t\n"]
  [(box? (fx->box 12)) => "#t\n"]
)

(add-tests-with-string-output "bytevector"
  [(bytevector? 0) => "#f\n"]
  [(bytevector? #f) => "#f\n"]
  [(bytevector? '()) => "#f\n"]
  [(bytevector? "s") => "#f\n"]
  [(bytevector? (make-byte-string 1)) => "#f\n"]
  [(bytevector? (make-bytevector 1)) => "#t\n"]
)

(add-tests-with-string-output "asciiz"
  [(string->asciiz "abc") => "\"abc\\0\"\n"]
  [(asciiz-length (string->asciiz "abc")) => "3\n"]
  [(let ([bv (make-bytevector 10)])
     (string-byte-set! bv 0 97)
     (string-byte-set! bv 1 98)
     (string-byte-set! bv 2 99)
     (string-byte-set! bv 3 0)
     (asciiz-length bv)) => "3\n"]
  [(asciiz->string (string->asciiz "abc")) => "\"abc\"\n"]
  [(let ([bv (make-bytevector 10)])
     (string-byte-set! bv 0 97)
     (string-byte-set! bv 1 98)
     (string-byte-set! bv 2 99)
     (string-byte-set! bv 3 0)
     (asciiz->string bv)) => "\"abc\"\n"]
)

(add-tests-with-string-output "let*"
  [(let* ([a 100] [b a] [c (cons a b)]) c) => "(100 . 100)\n"]
)
