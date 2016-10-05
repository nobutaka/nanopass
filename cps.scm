(use util.match)

(define cps-form
  (lambda (exp)
    `((lambda (call/cc primordial-continuation)
        ,(cps exp 'primordial-continuation))
      (lambda (k f)
        (f k (lambda (dummy-k result)
               (k result))))
      (lambda (r) r))))

(define cps
  (lambda (exp cont-exp)
    (if (not (pair? exp))
        `(,cont-exp ,exp)
        (match exp
          [('quote obj)
           `(,cont-exp ,exp)]
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
                      (memq rator *prim-names*))  ; It may not necessary to refer to env. Primitives seem terminator of CPS conversion.
                 (cps-list rands (lambda (args)
                                   (if (eq? rator '%apply)
                                       `(,rator ,cont-exp ,@args)
                                       `(,cont-exp (,rator ,@args)))))
                 (cps-list exp (lambda (args)
                                 (cons (car args)
                                       (cons cont-exp
                                             (cdr args)))))))]))))

(define cps-list
  (lambda (exp inner)
    (cps-list-body exp inner '())))

(define cps-list-body
  (lambda (exp inner args)
    (if (null? exp)
        (inner (reverse args))
        (cps (car exp)
             (let ([r (new-var 'r)])
               `(lambda (,r)
                  ,(cps-list-body (cdr exp) inner (cons r args))))))))

(define new-var
  (lambda (id)
    (gensym (symbol->string id))))
