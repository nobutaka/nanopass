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
  [(letrec ([loop
              (lambda (n pair)
                (if (= n 10000)
                    pair
                    (loop (+ n 1) (cons (+ n 1) '()))))])
     (loop 0 (cons 0 '()))) => "(10000)\n"]
  [(letrec ([loop
              (lambda (n vec)
                (if (= n 10000)
                    n
                    (loop (+ n 1) (make-vector 12))))])
     (loop 0 (make-vector 12))) => "10000\n"]
  [(letrec ([loop
              (lambda (n)
                (if (= n 10000)
                    (global-refs)
                    (loop (+ n 1))))])
     (set-global-refs! (cons 12 (vector 13)))
     (loop 0)) => "(12 . #(13))\n"]
  [(letrec ([loop
              (lambda (n str)
                (if (= n 10000)
                    str
                    (loop (+ n 1) (string #\a))))])
     (loop 0 (string #\a))) => "\"a\"\n"]
  [(letrec ([loop
              (lambda (n sym)
                (if (= n 10000)
                    sym
                    (loop (+ n 1) (string->uninterned-symbol (string #\a)))))])
     (loop 0 (string->uninterned-symbol (string #\a)))) => "a\n"]
  [(letrec ([a_minus_b (dlsym (string->asciiz "a_minus_b"))]
            [loop
              (lambda (n x)
                (if (= n 10000)
                    x
                    (loop (+ n 1) (string4->fx (foreign-call a_minus_b (reverse (list (fx->string4 x) (fx->string4 1))) 2)))))])
     (loop 0 0)) => "-10000\n"]
)
