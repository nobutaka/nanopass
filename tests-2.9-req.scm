(add-tests-with-string-output "foreign-call"
  [(string4? (dlsym (string->sz "twelve"))) => "#t\n"]
  [(string4? (foreign-call (dlsym (string->sz "twelve")) '() 0)) => "#t\n"]
  [(let ([a_minus_b (dlsym (string->sz "a_minus_b"))]
         [a (make-byte-string 4)]
         [b (make-byte-string 4)])
     (string-byte-set! a 0 8)
     (string-byte-set! a 1 0)
     (string-byte-set! a 2 0)
     (string-byte-set! a 3 0)
     (mutate-to-string4! a)
     (string-byte-set! b 0 5)
     (string-byte-set! b 1 0)
     (string-byte-set! b 2 0)
     (string-byte-set! b 3 0)
     (mutate-to-string4! b)
     (string-byte-ref (foreign-call a_minus_b (list b a) 2) 0))
   => "3\n"]
  [(let ([a_minus_b (dlsym (string->sz "a_minus_b"))]
         [args (list (fx->string4 8) (fx->string4 5))])
     (string4->fx (foreign-call a_minus_b (reverse args) (length args)))) => "3\n"]
  [(let ([data (make-byte-string 5)])
     (string-byte-set! data 4 12)
     (let ([get_byte (dlsym (string->sz "get_byte"))]
           [args (list data (fx->string4 4))])
       (string4->fx (foreign-call get_byte (reverse args) (length args))))) => "12\n"]
  [(let ([fopen (dlsym (string->sz "fopen"))]
         [fgetc (dlsym (string->sz "fgetc"))]
         [open-args (list (string->sz "./data.txt") (string->sz "r"))])
     (let ([fp (foreign-call fopen (reverse open-args) (length open-args))])
       (string4->fx (foreign-call fgetc (list fp) 1)))) => "97\n"]
  [(let ([a_minus_b (cproc 'fixnum "a_minus_b")])
     (a_minus_b 8 5)) => "3\n"]
  [(let ([data (make-bytevector 5)])
     (string-byte-set! data 4 12)
     (let ([get_byte (cproc 'fixnum "get_byte")])
       (get_byte data 4))) => "12\n"]
  [(let ([fopen (cproc 'void* "fopen")]
         [fgetc (cproc 'fixnum "fgetc")])
     (let ([fp (fopen "./data.txt" "r")])
       (fgetc fp))) => "97\n"]
  [(let ([printf (cproc 'fixnum "printf")])
     (printf "abc %d %s\n" 12 "def")) => "abc 12 def\n11\n"]
)
