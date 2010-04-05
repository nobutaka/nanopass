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

    (define-macro cond
      (lambda args
        (if (null? args)
            #f
            (if (eq? (caar args) 'else)
                `(begin ,@(cdar args))
                (if (null? (cdar args))
                    `(let ([+value+ ,(caar args)])
                       (if +value+ +value+ (cond ,@(cdr args))))
                    `(if ,(caar args)
                         (begin ,@(cdar args))
                         (cond ,@(cdr args))))))))

    (define-macro do
      (lambda (var-form test-form . args)
        (let ([vars (map car var-form)]
              [vals (map cadr var-form)]
              [step (map cddr var-form)])
          `(letrec ([loop (lambda ,vars
                            (if ,(car test-form)
                                (begin ,@(cdr test-form))
                                (begin
                                  ,@args
                                  (loop ,@(map (lambda (x y)
                                                 (if (null? x) y (car x)))
                                               step
                                               vars)))))])
             (loop ,@vals)))))

    (define string4-tag     0)
    (define bytevector-tag  1)

    ;; wraps primitives
    (define eq? (lambda (x1 x2) (%eq? x1 x2)))
    (define fixnum? (lambda (obj) (%fixnum? obj)))
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
    (define string? (lambda (obj) (%string? obj)))
    (define make-vector (lambda (k) (%make-vector k)))
    (define vector-ref (lambda (v k) (%vector-ref v k)))
    (define vector-set! (lambda (v k obj) (%vector-set! v k obj)))
    (define make-byte-string (lambda (k) (%make-byte-string k)))
    (define string-size (lambda (str) (%string-size str)))
    (define string-byte-ref (lambda (str k) (%string-byte-ref str k)))
    (define string-byte-set! (lambda (str k n) (%string-byte-set! str k n)))
    (define string-fx-ref (lambda (str k) (%string-fx-ref str k)))
    (define string-fx-set! (lambda (str k n) (%string-fx-set! str k n)))
    (define object-tag-set! (lambda (obj tag) (%object-tag-set! obj tag)))
    (define object-tag-ref (lambda (obj) (%object-tag-ref obj)))
    (define dlsym (lambda (asciiz) (%dlsym asciiz)))
    (define foreign-call (lambda (fptr args size) (%foreign-call fptr args size)))
    (define set-global-ref! (lambda (obj) (%set-global-ref! obj)))
    (define global-ref (lambda () (%global-ref)))
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

    (define length
      (lambda (ls)
        (if (null? ls)
            0
            (+ 1 (length (cdr ls))))))

    (define map
      (lambda (proc ls)
        (if (null? ls)
            '()
            (cons (proc (car ls)) (map proc (cdr ls))))))

    (define reverse
      (lambda (ls)
        (let loop ([ls ls] [a '()])
          (if (null? ls)
              a
              (loop (cdr ls) (cons (car ls) a))))))

    (define mutate-to-string4!
      (lambda (str)
        (object-tag-set! str string4-tag)))

    (define mutate-to-bytevector!
      (lambda (str)
        (object-tag-set! str bytevector-tag)))

    (define mutated-string?
      (lambda (obj tag)
        (if (string? obj)
            (= (object-tag-ref obj) tag)
            #f)))

    (define string4?
      (lambda (obj)
        (mutated-string? obj string4-tag)))

    (define bytevector?
      (lambda (obj)
        (mutated-string? obj bytevector-tag)))

    (define make-bytevector
      (lambda (k)
        (let ([str (make-byte-string k)])
          (mutate-to-bytevector! str)
          str)))

    (define string->asciiz
      (lambda (str)
        (let* ([size (string-size str)]
               [bv (make-bytevector (+ size 1))])
          (let loop ([k 0])
            (if (= k size)
                (begin
                  (string-byte-set! bv k 0)
                  bv)
                (begin
                  (string-byte-set! bv k (string-byte-ref str k))
                  (loop (+ k 1))))))))

    (define asciiz-length
      (lambda (bv)
        (let ([limit (string-size bv)])
          (let loop ([i 0])
            (cond [(= i limit) limit]
                  [(= (string-byte-ref bv i) 0) i]
                  [else (loop (+ i 1))])))))

    (define asciiz->string
      (lambda (bv)
        (let* ([len (asciiz-length bv)]
               [s (make-byte-string len)])
          (do ((i 0 (+ i 1)))
              ((= i len) s)
            (string-byte-set! s i (string-byte-ref bv i))))))

    (define fx->string4
      (lambda (n)
        (let ([str (make-byte-string 4)])
          (string-fx-set! str 0 n)
          (mutate-to-string4! str)
          str)))

    (define string4->fx
      (lambda (str)
        (string-fx-ref str 0)))

    (define cproc
      (lambda (return-type name)
        (let ([fptr (dlsym (string->asciiz name))]
              [convert-argument
                (lambda (x)
                  (cond [(string4? x) x]
                        [(bytevector? x) x]
                        [(string? x) (string->asciiz x)]
                        [(fixnum? x) (fx->string4 x)]
                        [else (fx->string4 0)]))]
              [convert-return-value
                (cond [(eq? return-type 'fixnum) string4->fx]
                      [(eq? return-type 'void) (lambda (x) #f)]
                      [else (lambda (x) x)])])
          (lambda args
            (let ([converted-args (map convert-argument args)])
              (convert-return-value (foreign-call fptr (reverse converted-args) (length converted-args))))))))
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
