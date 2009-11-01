; From:
;; (begin
;;   (foo)
;;   (define a (lambda () 1))
;;   (define b (lambda () 2))
;;   (a))

; To:
;; ((lambda (a b)
;;    (foo)
;;    (set! a (lambda () 1))
;;    (set! b (lambda () 2))
;;    (a))
;;  #f #f)

(define local-form
  (lambda (exp)
    (if (not (begin-exp? exp))
        exp
        (let ([vars (defined-vars (cdr exp))])
          `((lambda ,vars
              ,@(replace-define (cdr exp)))
            ,@(map (lambda (v) #f) vars))))))

(define defined-vars
  (lambda (exps)
    (fold
      (lambda (exp vars)
        (if (define-exp? exp)
            (cons (cadr exp) vars)
            vars))
      '()
      exps)))

(define replace-define
  (lambda (exps)
    (map
      (lambda (exp)
        (if (define-exp? exp)
            `(set! ,@(cdr exp))
            exp))
      exps)))

(define exp?
  (lambda (exp sym)
    (and (pair? exp)
         (eq? (car exp) sym))))

(define begin-exp?
  (lambda (exp)
    (exp? exp 'begin)))

(define define-exp?
  (lambda (exp)
    (exp? exp 'define)))
