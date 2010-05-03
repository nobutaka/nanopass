(add-tests-with-string-output "low-level ffi"
  [(eq? (begin
          (set-global-refs! (cons 12 13))
          (global-refs))
        (cons 12 13)) => "#f\n"]
  [(let ([pair (cons 12 13)])
     (set-global-refs! pair)
     (eq? pair (global-refs))) => "#t\n"]
)

(add-tests-with-string-output "foreign function call"
  [(string4? (dlsym (string->asciiz "twelve"))) => "#t\n"]
  [(string4? (foreign-call (dlsym (string->asciiz "twelve")) '() 0)) => "#t\n"]
  [(let ([a_minus_b (dlsym (string->asciiz "a_minus_b"))]
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
  [(let ([a_minus_b (dlsym (string->asciiz "a_minus_b"))]
         [args (list (fx->string4 8) (fx->string4 5))])
     (string4->fx (foreign-call a_minus_b (reverse args) (length args)))) => "3\n"]
  [(let ([data (make-byte-string 5)])
     (string-byte-set! data 4 12)
     (let ([get_byte (dlsym (string->asciiz "get_byte"))]
           [args (list data (fx->string4 4))])
       (string4->fx (foreign-call get_byte (reverse args) (length args))))) => "12\n"]
  [(let ([fopen (dlsym (string->asciiz "fopen"))]
         [fgetc (dlsym (string->asciiz "fgetc"))]
         [open-args (list (string->asciiz "./data.txt") (string->asciiz "r"))])
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
  [(let ([bv (make-bytevector 10)]
         [strcpy (cproc 'void* "strcpy")])
     (strcpy bv "abc")
     (asciiz->string bv)) => "\"abc\"\n"]
  [(let ([bv (make-bytevector 10)]
         [sprintf (cproc 'fixnum "sprintf")])
     (sprintf bv "%d %s" 1 "two")
     (asciiz->string bv)) => "\"1 two\"\n"]
  [(let* ([malloc (cproc 'void* "malloc")]
          [free (cproc 'void "free")]
          [ptr (malloc 1024)])
     ;; do something
     (free ptr)
     #f) => "#f\n"]
)

(add-tests-with-string-output "callback"
  [(let ([refs (make-vector 2)]
         [ccc (cproc 'fixnum "call_call_closure")]
         [proc (lambda () 1)])
     (vector-set! refs 0 primordial-continuation)
     (vector-set! refs 1 proc)
     (set-global-refs! refs)
     (ccc)) => "1\n"]
  [(letrec ([refs (make-vector 2)]
            [ccc (cproc 'fixnum "call_call_closure")]
            [counter 1]
            [proc (lambda ()
                    (if (= counter 10)
                        counter
                        (begin
                          (set! counter (+ counter 1))
                          (ccc))))])
     (vector-set! refs 0 primordial-continuation)
     (vector-set! refs 1 proc)
     (set-global-refs! refs)
     (ccc)) => "10\n"]
  [(letrec ([refs (make-vector 2)]
            [ccc (cproc 'fixnum "call_call_closure")]
            [counter 1]
            [proc (lambda ()
                    (if (= counter 10)
                        counter
                        (begin
                          (if (= counter 5) (collect) #f)
                          (set! counter (+ counter 1))
                          (ccc))))]
            [collect (lambda ()
                       (make-vector 5000)
                       (make-vector 5000))])
     (vector-set! refs 0 primordial-continuation)
     (vector-set! refs 1 proc)
     (set-global-refs! refs)
     (ccc)) => "10\n"]
)
