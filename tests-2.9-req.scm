(add-tests-with-string-output "foreign-call"
  [(let ([a-b (dlsym (string->sz "a_minus_b"))]
         [a (make-byte-string 4)]
         [b (make-byte-string 4)])
     (string-byte-set! a 0 8)
     (string-byte-set! a 1 0)
     (string-byte-set! a 2 0)
     (string-byte-set! a 3 0)
     (string-byte-set! b 0 5)
     (string-byte-set! b 1 0)
     (string-byte-set! b 2 0)
     (string-byte-set! b 3 0)
     (string-byte-ref (foreign-call a-b (list (cons int-tag b) (cons int-tag a)) 2) 0))
   => "3\n"]
  [(let ([a-b (dlsym (string->sz "a_minus_b"))]
         [args (list (cons int-tag (fx->string4 8)) (cons int-tag (fx->string4 5)))])
     (string4->fx (foreign-call a-b (reverse args) (length args)))) => "3\n"]
  [(let ([data (make-byte-string 5)])
     (string-byte-set! data 4 12)
     (let ([get-byte (dlsym (string->sz "get_byte"))]
           [args (list (cons array-tag data) (cons int-tag (fx->string4 4)))])
       (string4->fx (foreign-call get-byte (reverse args) (length args))))) => "12\n"]
  [(let ([fopen (dlsym (string->sz "fopen"))]
         [fgetc (dlsym (string->sz "fgetc"))]
         [open-args (list (cons array-tag (string->sz "./data.txt")) (cons array-tag (string->sz "r")))])
     (let ([fp (foreign-call fopen (reverse open-args) (length open-args))])
       (string4->fx (foreign-call fgetc (list (cons int-tag fp)) 1)))) => "97\n"]
)
