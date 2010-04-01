(add-tests-with-string-output "begin/implicit-begin"
  [(begin 12) => "12\n"]
  [(begin 13 122) => "122\n"]
  [(begin 123 2343 #t) => "#t\n"]
  [(let ([t (begin 12 (cons 1 2))]) (begin t t)) => "(1 . 2)\n"]
  [(let ([t (begin 13 (cons 1 2))])
     (cons 1 t)
     t) => "(1 . 2)\n"]
;  [(let ([t (cons 1 2)])
;     (if (pair? t)
;         (begin t)
;         12)) => "(1 . 2)\n"]
)

(add-tests-with-string-output "vectors"
;  [(vector? (make-vector 0)) => "#t\n"]
;  [(vector-length (make-vector 12)) => "12\n"]
;  [(vector? (cons 1 2)) => "#f\n"]
;  [(vector? 1287) => "#f\n"]
;  [(vector? ()) => "#f\n"]
;  [(vector? #t) => "#f\n"]
;  [(vector? #f) => "#f\n"]
;  [(pair? (make-vector 12)) => "#f\n"]
  [(null? (make-vector 12)) => "#f\n"]
;  [(boolean? (make-vector 12)) => "#f\n"]
  [(make-vector 0) => "#()\n"]
  [(let ([v (make-vector 2)])
     (vector-set! v 0 #t)
     (vector-set! v 1 #f)
     v) => "#(#t #f)\n"]
  [(let ([v (make-vector 2)])
     (vector-set! v 0 v)
     (vector-set! v 1 v)
     (eq? (vector-ref v 0) (vector-ref v 1))) => "#t\n"]
  [(let ([v (make-vector 1)] [y (cons 1 2)])
     (vector-set! v 0 y)
     (cons y (eq? y (vector-ref v 0)))) => "((1 . 2) . #t)\n"]
  [(let ([v0 (make-vector 2)])
     (let ([v1 (make-vector 2)])
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (cons v0 v1))) => "(#(100 200) . #(300 400))\n"]
  [(let ([v0 (make-vector 3)])
     (let ([v1 (make-vector 3)])
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (vector-set! v0 2 150)
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (vector-set! v1 2 350)
       (cons v0 v1))) => "(#(100 200 150) . #(300 400 350))\n"]
  [(let ([n 2])
    (let ([v0 (make-vector n)])
     (let ([v1 (make-vector n)])
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (vector-set! v1 0 300)
       (vector-set! v1 1 400)
       (cons v0 v1)))) => "(#(100 200) . #(300 400))\n"]
;  [(let ([n 3])
;    (let ([v0 (make-vector n)])
;     (let ([v1 (make-vector (vector-length v0))])
;       (vector-set! v0 (fx- (vector-length v0) 3) 100)
;       (vector-set! v0 (fx- (vector-length v1) 2) 200)
;       (vector-set! v0 (fx- (vector-length v0) 1) 150)
;       (vector-set! v1 (fx- (vector-length v1) 3) 300)
;       (vector-set! v1 (fx- (vector-length v0) 2) 400)
;       (vector-set! v1 (fx- (vector-length v1) 1) 350)
;       (cons v0 v1)))) => "(#(100 200 150) . #(300 400 350))\n"]
)
