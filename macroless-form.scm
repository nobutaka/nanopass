(use util.match)

(define library
  '(
    (define-macro let
      (lambda (decls . bodies)
        (if (pair? decls)
            (let ([vars (map car decls)]
                  [vals (map cadr decls)])
              `((lambda ,vars ,@bodies) ,@vals))
            (let ([vars (map car (car bodies))]
                  [vals (map cadr (car bodies))])
              `(letrec ([,decls (lambda ,vars ,@(cdr bodies))])
                 (,decls ,@vals))))))

    (define-macro let*
      (lambda (decls . bodies)
        (if (null? (cdr decls))
            `(let (,(car decls)) ,@bodies)
            `(let (,(car decls)) (let* ,(cdr decls) ,@bodies)))))

    (define-macro letrec
      (lambda (decls . bodies)
        (let ([vars (map car decls)]
              [vals (map cadr decls)])
          (let ([holders (map (lambda (x) #f) vars)]
                [assigns (map (lambda (v e) `(set! ,v ,e)) vars vals)])
            `((lambda ,vars ,@assigns ,@bodies) ,@holders)))))

    ;; wraps primitives
    (define eq? (lambda (x1 x2) (%eq? x1 x2)))
    (define fx+ (lambda (x1 x2) (%fx+ x1 x2)))
    (define fx- (lambda (x1 x2) (%fx- x1 x2)))
    (define fx= eq?)
    (define fl+ (lambda (x1 x2) (%fl+ x1 x2)))
    (define + fx+)
    (define - fx-)
    (define = fx=)
    (define car (lambda (x) (%car x)))
    (define cdr (lambda (x) (%cdr x)))
    (define cons (lambda (x1 x2) (%cons x1 x2)))
    (define null? (lambda (obj) (%null? obj)))
    (define string->uninterned-symbol (lambda (x) (%string->uninterned-symbol x)))
    (define vector-ref (lambda (v k) (%vector-ref v k)))
    (define vector-set! (lambda (v k obj) (%vector-set! v k obj)))
    (define make-byte-string (lambda (k) (%make-byte-string k)))
    (define string-size (lambda (str) (%string-size str)))
    (define string-byte-ref (lambda (str k) (%string-byte-ref str k)))
    (define string-byte-set! (lambda (str k n) (%string-byte-set! str k n)))
    (define string-fx-ref (lambda (str k) (%string-fx-ref str k)))
    (define string-fx-set! (lambda (str k n) (%string-fx-set! str k n)))
    (define dlsym (lambda (sz) (%dlsym sz)))
    (define foreign-call (lambda (fptr args size) (%foreign-call fptr args size)))
    (define apply (lambda (proc args) (%apply proc args)))

    (define caar (lambda (x) (car (car x))))
    (define cadr (lambda (x) (car (cdr x))))
    (define cdar (lambda (x) (cdr (car x))))
    (define cddr (lambda (x) (cdr (cdr x))))
    (define caaar (lambda (x) (car (car (car x)))))
    (define caadr (lambda (x) (car (car (cdr x)))))
    (define cadar (lambda (x) (car (cdr (car x)))))
    (define caddr (lambda (x) (car (cdr (cdr x)))))
    (define cdaar (lambda (x) (cdr (car (car x)))))
    (define cdadr (lambda (x) (cdr (car (cdr x)))))
    (define cddar (lambda (x) (cdr (cdr (car x)))))
    (define cdddr (lambda (x) (cdr (cdr (cdr x)))))

    (define list (lambda x x))

    (define reverse
      (lambda (ls)
        (let loop ([ls ls] [a '()])
          (if (null? ls)
              a
              (loop (cdr ls) (cons (car ls) a))))))

    (define length
      (lambda (ls)
        (if (null? ls)
            0
            (+ 1 (length (cdr ls))))))

    (define string->sz
      (lambda (str)
        (let* ([size (string-size str)]
               [sz (make-byte-string (+ size 1))])
          (let loop ([k 0])
            (if (= k size)
                (begin
                  (string-byte-set! sz k 0)
                  sz)
                (begin
                  (string-byte-set! sz k (string-byte-ref str k))
                  (loop (+ k 1))))))))
))

(define append-library
  (lambda (exp)
    (if (not (begin-exp? exp))
        `(begin ,@library ,exp)
        `(begin ,@library ,@(cdr exp)))))

(define macroless-form
  (lambda (exp)
    (if (not (begin-exp? exp))
        exp
        `(begin ,@(remove define-macro-exp? (expand-top-level (cdr exp)))))))

(define expand-top-level
  (lambda (exps)
    (let ([env (make-module #f)])
      (map
        (lambda (e)
          (match e
            [('define-macro _ _)
             (eval e env)
             e]
            [else
             (expand e env)]))
        exps))))

(define expand
  (lambda (exp env)
    (if (not (pair? exp))
        exp
        (match exp
          [('define var e)
           `(define ,var ,(expand e env))]
          [('quote obj)
           `(quote ,obj)]
          [('begin . exps)
           `(begin ,@(map (lambda (e) (expand e env)) exps))]
          [('if t c a)
           (let ([t-exp (expand t env)]
                 [c-exp (expand c env)]
                 [a-exp (expand a env)])
             `(if ,t-exp ,c-exp ,a-exp))]
          [('set! v e)
           `(set! ,v ,(expand e env))]
          [('lambda formals . bodies)
           `(lambda ,formals ,@(map (lambda (e) (expand e env)) bodies))]
          [else
           (let ([r (eval `(macroexpand ',exp) env)])
             (if (equal? exp r)
                 (map (lambda (e) (expand e env)) r)
                 (expand r env)))]))))

(define define-macro-exp?
  (lambda (exp)
    (exp? exp 'define-macro)))
