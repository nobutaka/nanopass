(add-tests-with-string-output "cons"
  [(if (cons 12 43) 32 43) => "32\n"]
  [(car (cons 1 23)) => "1\n"]
  [(cdr (cons 43 123)) => "123\n"]
  [(car (car (cons (cons 12 3) (cons #t #f)))) => "12\n"]
  [(cdr (car (cons (cons 12 3) (cons #t #f)))) => "3\n"]
  [(car (cdr (cons (cons 12 3) (cons #t #f)))) => "#t\n"]
  [(cdr (cdr (cons (cons 12 3) (cons #t #f)))) => "#f\n"]
  [(let ([t0 (cons 1 2)] [t1 (cons 3 4)])
     (let ([a0 (car t0)] [a1 (car t1)] [d0 (cdr t0)] [d1 (cdr t1)])
       (let ([t0 (cons a0 d1)] [t1 (cons a1 d0)])
         (cons t0 t1))))
   => "((1 . 4) 3 . 2)\n"]
  [(let ([t (cons 1 2)])
     (let ([t t])
       (let ([t t])
         (let ([t t])
           t))))
   => "(1 . 2)\n"]
  [(let ([t (let ([t (let ([t (let ([t (cons 1 2)]) t)]) t)]) t)]) t)
   => "(1 . 2)\n"]
  [(let ([x '()])
     (let ([x (cons x x)])
       (let ([x (cons x x)])
         (let ([x (cons x x)])
           (cons x x)))))
   => "((((()) ()) (()) ()) ((()) ()) (()) ())\n"]
  [(cons (let ([x #t]) (let ([y (cons x x)]) (cons x y)))
         (cons (let ([x #f]) (let ([y (cons x x)]) (cons y x)))
               '()))
   => "((#t #t . #t) ((#f . #f) . #f))\n"]
)

(add-tests-with-string-output "caar, cdar, etc."
  [(caar (cons (cons 1 2) (cons 3 4))) => "1\n"]
  [(cadr (cons (cons 1 2) (cons 3 4))) => "3\n"]
  [(cdar (cons (cons 1 2) (cons 3 4))) => "2\n"]
  [(cddr (cons (cons 1 2) (cons 3 4))) => "4\n"]
  [(caaar (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "1\n"]
  [(caadr (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "5\n"]
  [(cadar (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "3\n"]
  [(caddr (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "7\n"]
  [(cdaar (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "2\n"]
  [(cdadr (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "6\n"]
  [(cddar (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "4\n"]
  [(cdddr (cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))) => "8\n"]
)

(add-tests-with-string-output "list"
  [(list 1 2 3) => "(1 2 3)\n"]
  [(list) => "()\n"]
)

(add-tests-with-string-output "length"
  [(length '()) => "0\n"]
  [(length '(1)) => "1\n"]
  [(length '(1 2)) => "2\n"]
  [(length '(1 2 3)) => "3\n"]
  [(length (cons 1 '())) => "1\n"]
  [(length '(1 (2 3))) => "2\n"]
)

(add-tests-with-string-output "map"
  [(map cadr '((a b) (d e) (g h))) => "(b e h)\n"]
  [(map (lambda (n) (+ n n))
        '(1 2 3 4 5)) => "(2 4 6 8 10)\n"]
  [(let ([count 0])
     (map (lambda (ignored)
            (set! count (+ count 1))
            count)
          '(a b))) => "(1 2)\n"]
)

(add-tests-with-string-output "reverse"
  [(reverse '(a b c d e)) => "(e d c b a)\n"]
  [(reverse '()) => "()\n"]
)
