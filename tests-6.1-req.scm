(add-tests-with-string-output "gc"
  [(letrec ([fib
              (lambda (n)
                (if (= n 0)
                    n
                    (if (= n 1)
                        n
                        (+ (fib (- n 1)) (fib (- n 2))))))])
     (fib 35)) => "9227465\n"]
  [(letrec ([fib
              (lambda n
                (if (= (car n) 0)
                    (car n)
                    (if (= (car n) 1)
                        (car n)
                        (+ (fib (- (car n) 1)) (fib (- (car n) 2))))))])
     (fib 35)) => "9227465\n"]
  [(letrec ([accum
              (lambda (n pair)
                (if (= n 1000000)
                    pair
                    (accum (+ n 1) (cons (+ n 1) '()))))])
     (accum 0 (cons 0 '()))) => "(1000000)\n"]
  [(letrec ([accum
              (lambda (n str)
                (if (= n 1000000)
                    str
                    (accum (+ n 1) (string #\a))))])
     (accum 0 (string #\a))) => "\"a\"\n"]
  [(letrec ([accum
              (lambda (n sym)
                (if (= n 1000000)
                    sym
                    (accum (+ n 1) (string->uninterned-symbol (string #\a)))))])
     (accum 0 (string->uninterned-symbol (string #\a)))) => "a\n"]
  [(letrec ([a_minus_b (dlsym (string->sz "a_minus_b"))]
            [accum
              (lambda (n x)
                (if (= n 1000000)
                    x
                    (accum (+ n 1) (string4->fx (foreign-call a_minus_b (reverse (list (fx->string4 x) (fx->string4 1))) 2)))))])
     (accum 0 0)) => "-1000000\n"]
)
