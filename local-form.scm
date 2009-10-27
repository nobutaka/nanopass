; From:
;; (begin
;;   (foo)
;;   (define a (lambda () 1))
;;   (define b (lambda () 2))
;;   (a))

; To:
;; (let ([a #f]
;;       [b #f])
;;   (foo)
;;   (set! a (lambda () 1))
;;   (set! b (lambda () 2))
;;   (a))

(define local-form
  (lambda (exp)
    (if (not (begin-exp? exp))
        exp
        `(let ,(dummy-decls (defined-binds (cdr exp))) ,@(replace-define (cdr exp))))))

(define defined-binds
  (lambda (exps)
    (fold
      (lambda (exp binds)
        (if (define-exp? exp)
            (cons (cadr exp) binds)
            binds))
      '()
      exps)))

(define dummy-decls
  (lambda (binds)
    (map 
      (lambda (bind)
        `[,bind #f])
      binds)))

(define replace-define
  (lambda (exps)
    (map
      (lambda (exp)
        (if (define-exp? exp)
            `(set! ,@(cdr exp))
            exp))
      exps)))

(define begin-exp?
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) 'begin))))

(define define-exp?
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) 'define))))
