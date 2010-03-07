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
     (string-byte-ref (foreign-call a-b (list (cons 0 b) (cons 0 a)) 2) 0))
   => "3\n"]
)
