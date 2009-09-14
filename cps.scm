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
          [('if t c a)
           (let ([r (new-var 'r)]
                 [c-exp (cps c cont-exp)]
                 [a-exp (cps a cont-exp)])
             (cps t
                  `(lambda (,r)
                     (if ,r ,c-exp ,a-exp))))]
          [('set! v e)
           (let ([r (new-var 'r)])
             (cps e
                  `(lambda (,r)
                     (,cont-exp (set! ,v ,r)))))]
          [('lambda formals body)
           (let ([k (new-var 'k)])
             `(,cont-exp (lambda ,(cons k formals) ,(cps body k))))]
          [else
           (let ([rator (car exp)]
                 [rands (cdr exp)])
             (if (and (symbol? rator)
                      (memq rator *prim-names*))  ; TODO: refer to env
                 (cps-list rands (lambda (args)
                                   `(,cont-exp (,rator ,@args))))
                 (cps-list exp (lambda (args)
                                 (cons (car args)
                                       (cons cont-exp
                                             (cdr args)))))))])))
  (define (cps-list exp inner)
    (define (body exp args)
      (if (null? exp)
          (inner (reverse args))
          (cps (car exp)
               (let ([r (new-var 'r)])
                 `(lambda (,r)
                    ,(body (cdr exp) (cons r args)))))))
    (body exp '()))
  (cps exp '(lambda (r) r)))

(define (new-var id)
  (gensym (symbol->string id)))
