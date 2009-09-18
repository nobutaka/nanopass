; From:
;; (foo)
;; (define a (lambda () 1))
;; (define b (lambda () 2))
;; (a)

; To:
;; (let ([a #f]
;;       [b #f])
;;   (foo)
;;   (set! a (lambda () 1))
;;   (set! b (lambda () 2))
;;   (a))

(define local-form
  (lambda (exps)
    `(let ,(dummy-decls (defined-binds exps)) ,@(replace-define exps))))

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

(define define-exp?
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) 'define))))
