(add-tests-with-string-output "letrec"
  [(letrec () 12) => "12\n"]
  [(letrec ([f 12]) f) => "12\n"]
  [(letrec ([f 12] [g 13]) (fx+ f g)) => "25\n"]
  [(letrec ([f 12] [g (lambda () f)])
     (g)) => "12\n"]
  [(letrec ([f 12] [g (lambda (n) (set! f n))])
     (g 130)
     f) => "130\n"]
  [(letrec ([f (lambda (g) (set! f g) (f))])
     (f (lambda () 12))) => "12\n"]
  [(letrec ([f (cons (lambda () f)
                     (lambda (x) (set! f x)))])
     (let ([g (car f)])
       ((cdr f) 100)
       (g))) => "100\n"]
)

(add-tests-with-string-output "cond"
  [(cond [1 2] [else 3]) => "2\n"]
  [(cond [1] [else 13]) => "1\n"]
  [(cond [#f #t] [#t #f]) => "#f\n"]
  [(cond [else 17]) => "17\n"]
  [(cond [#f] [#f 12] [12 13]) => "13\n"]
  [(let ([else #t])
     (cond
       [else 1287])) => "1287\n"]
  ;[(let ([else 17])
  ;   (cond
  ;     [else])) => "17\n"]
  ;[(let ([else #f])
  ;   (cond
  ;     [else ((lambda (x) (x x)) (lambda (x) (x x)))])
  ;   else) => "#f\n"]
)

(add-tests-with-string-output "do"
  [(do ((vec (make-vector 5))
        (i 0 (+ i 1)))
       ((= i 5) vec)
     (vector-set! vec i i)) => "#(0 1 2 3 4)\n"]
  [(let ([x '(1 3 5 7 9)])
     (do ((x x (cdr x))
          (sum 0 (+ sum (car x))))
         ((null? x) sum))) => "25\n"]
)
