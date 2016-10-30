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
             [(rator . rands)
              (if (primitive-operator? rator) #t #f)]
             [else #f])
      (and (not (pair? x)) (not (null? x))))))

(define beta-reduce
  (lambda (exp)
    (match exp
      [('quote obj)
       exp]
      [(('lambda formals body) . args)
       (if (every atomic? args)  ; symbol? or constant? other than lamnda-exp
           (let ([body-u (subst (map cons formals args) body)])
             (if (equal? body body-u)
                 body
                 (beta-reduce body-u)))
           `((lambda ,formals ,(beta-reduce body))
             ,@(map beta-reduce args)))]
      [(? symbol?)
       exp]
      [('lambda formals body)
       `(lambda ,formals ,(beta-reduce body))]
      [(rator . rands)
       (map beta-reduce exp)]
      [else
       exp])))

(define subst
  (lambda (s exp)
    (match exp
      [(? symbol?)
       (let ([p (assq exp s)])
         (if p (cdr p) exp))]
      [('quote obj)
       exp]
      [('set! v e)
       `(set! ,v ,(subst s e))]
      [('lambda formals body)
       (let ([s-u (filter (lambda (p) (not (memq (car p) formals))) s)])
         `(lambda ,formals ,(subst s-u body)))]
      [(rator . rands)
       (map (lambda (e) (subst s e)) exp)]
      [else
       exp])))
