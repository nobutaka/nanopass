(use util.match)
(use srfi-1)

(define *keywords*
  '(quote begin if set! lambda))

(define (core-convert exp)
  (if (not (pair? exp))
      (cond
        [(symbol? exp) exp]
        [(number? exp)
         `(quote ,exp)]
        [else
         (error "Bad expression" exp)])
      (match exp
        [('quote obj)
         `(quote ,obj)]
        [('begin e0 . exps)
         (if (null? exps)
             (core-convert e0)
             (let ([new-e0 (core-convert e0)]
                   [new-e1 (core-convert `(begin . ,exps))])
               `(begin ,new-e0 ,new-e1)))]
        [('if t c a)
         (let ([new-t (core-convert t)]
               [new-c (core-convert c)]
               [new-a (core-convert a)])
           `(if ,new-t ,new-c ,new-a))]
        [('set! v e)
         (cond
           [(not (symbol? v))
            (error "Bad expression" exp)]
           [else
            (let ([new-e (core-convert e)])
              `(set! ,v ,new-e))])]
        [('lambda formals . bodies)
         (if (not (and (list? formals)
                       (every symbol? formals)
                       (every (lambda (x) (not (memq x *keywords*)))
                         formals)
                       (set? formals)))
             (errorf "Bad formals ~a in ~a" formals exp)
             (let ([new-body (core-convert `(begin ,@bodies))])
               `(lambda ,formals ,new-body)))]
        [else
         (if (or (null? exp)
                 (not (list? exp))
                 (memq (car exp) *keywords*))
             (error "Bad expression" exp)
             (let ([rator (car exp)]
                   [rands (cdr exp)])
               (let ([new-rator (core-convert rator)]
                     [new-rands (core-convert-list rands)])
                 `(,new-rator . ,new-rands))))])))

(define (core-convert-list ls)
  (map core-convert ls))

(define (set? ls)
  (or (null? ls)
      (and (not (memq (car ls) (cdr ls)))
           (set? (cdr ls)))))
