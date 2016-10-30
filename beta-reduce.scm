(use util.match)

(define primitive-operator?
  (lambda (rator)
    (and (symbol? rator)
         (memq rator *prim-names*))))

(define atomic?
  (lambda (x)
    (or
      (match x
             [('quote liteal) #t]
             [(f . args)
              (if (primitive-operator? f) #t #f)]
             [_ #f])
      (and (not (pair? x)) (not (null? x))))))

(define (beta-reduce expr)
  (match expr
    [('quote obj)
     expr]
    [(('lambda formals body) . args)
     (if (every atomic? args)  ; symbol? or constant? other than lamnda-expr
         (let ((body-u (subst (map cons formals args) body)))
           (if (equal? body body-u)
               body
               (beta-reduce body-u)))
         `((lambda ,formals ,(beta-reduce body))
           ,@(map beta-reduce args)))]
    [(? symbol?)
     expr]
    [('lambda formals body)
     `(lambda ,formals ,(beta-reduce body))]
    [(f . args)
     (map beta-reduce expr)]
    [_
     expr]))

(define (subst s expr)
  (match expr
    [('quote obj)
     expr]
    [(? symbol?)
     (let ((p (assq expr s)))
       (if p (cdr p) expr))]
    [('lambda formals body)
     (let ((s-u (filter (lambda (p) (not (memq (car p) formals))) s)))
       `(lambda ,formals ,(subst s-u body)))]
    [('set! var e)
     `(set! ,var ,(subst s e))]
    [(f . args)
     (map (lambda (e) (subst s e)) expr)]
    [_
     expr]))
