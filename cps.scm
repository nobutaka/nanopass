(use util.match)

(define (cps-convert exp)
  (define (cps exp cont-exp)
    (if (not (pair? exp))
        `(,cont-exp ,exp)
        (match exp
          [('quote obj)
           `(,cont-exp (quote ,obj))]
          [('begin a b)
           (let ([b-exp (cps b cont-exp)]
                 [r (new-var 'r)])
             (cps a `(lambda (,r) ,b-exp)))]
          [('lambda formals body)
           (let ([k (new-var 'k)])
             `(,cont-exp (lambda ,(cons k formals) ,(cps body k))))]
          [else
           (cps-list exp cont-exp)])))
  (define (cps-list exp cont-exp)
    (define (body exp args)
      (if (null? exp)
          (cons (car (reverse args))
                (cons cont-exp
                      (cdr (reverse args))))
          (cps (car exp)
               (let ([r (new-var 'r)])
                 `(lambda (,r)
                    ,(body (cdr exp) (cons r args)))))))
    (body exp '()))
  (cps exp '(lambda (r) r)))

(define (new-var id)
  (gensym (symbol->string id)))
