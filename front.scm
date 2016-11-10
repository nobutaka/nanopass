(use util.match)
(use srfi-1)
(use srfi-11)

;; ---------- Core Form

(define *prim-names*
  '(%eq? %lt?
    %fixnum? %fixnum %fx+ %fx- %fx*
    %flonum? %flonum %fl+ %fl- %fl* %fl/ %fl= %fl< %fl<=
    %car %cdr %cons
    %null?
    %string->uninterned-symbol
    string
    %string?
    %make-vector vector %vector-ref
    %vector-set!
    %make-byte-string %string-size
    %string-byte-ref %string-byte-set!
    %string-int-ref %string-int-set!
    %string-float-ref %string-float-set!
    %object-tag-set! %object-tag-ref
    %dlsym %foreign-call %foreign-call-int %foreign-call-float %set-global-refs! %global-refs
    %apply))

(define *keywords*
  '(quote begin if set! lambda))

(define core-form
  (lambda (exp)
    (core-convert exp)))

(define core-convert
  (lambda (exp)
    (if (not (pair? exp))
        (cond
          [(symbol? exp) exp]
          [(or (number? exp) (boolean? exp) (string? exp) (char? exp))
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
           (let ([pformals (improper->proper formals)])
             (if (not (and (every symbol? pformals)
                           (every (lambda (x) (not (memq x *keywords*))) pformals)
                           (set? pformals)))
                 (errorf "Bad formals ~s in ~s" formals exp)
                 (let ([new-body (core-convert `(begin ,@bodies))])
                   `(lambda ,formals ,new-body))))]
          [else
           (if (or (null? exp)
                   (not (list? exp))
                   (memq (car exp) *keywords*))
               (error "Bad expression" exp)
               (let ([rator (car exp)]
                     [rands (cdr exp)])
                 (let ([new-rator (core-convert rator)]
                       [new-rands (core-convert-list rands)])
                   `(,new-rator . ,new-rands))))]))))

(define core-convert-list
  (lambda (ls)
    (map core-convert ls)))

;; ---------- Analyzed Form

(define analyzed-form
  (lambda (exp)
    (let-values ([(exp quotes poked free)
                  (analyze exp '())])
      `(let ,quotes ,exp))))

(define analyze  ;; returns: exp, quote-pairs, assigned, free
  (lambda (exp env)
    (if (not (pair? exp))
        (if (memq exp env)
            (values exp '() '() (unit-set exp))
            (if (memq exp *prim-names*)
                (errorf "Primitive in non-application position ~s"
                  exp)
                (errorf "Unbound variable ~s" exp)))
        (match exp
          [('quote obj)
           (if (or (number? obj) (null? obj) (boolean? obj) (char? obj))
               (values `(quote ,obj) '() '() '())
               (let ([var (gen-qsym)])
                 (values var (list (list var exp)) '() (unit-set var))))]
          [('begin a b)
           (let-values ([(a-exp a-quotes a-poked a-free) (analyze a env)]
                        [(b-exp b-quotes b-poked b-free) (analyze b env)])
             (values `(begin ,a-exp ,b-exp)
               (append a-quotes b-quotes)
               (union a-poked b-poked)
               (union a-free b-free)))]
          [('if t c a)
           (let-values ([(t-exp t-quotes t-poked t-free) (analyze t env)]
                        [(c-exp c-quotes c-poked c-free) (analyze c env)]
                        [(a-exp a-quotes a-poked a-free) (analyze a env)])
             (values `(if ,t-exp ,c-exp ,a-exp)
               (append t-quotes c-quotes a-quotes)
               (union (union t-poked c-poked) a-poked)
               (union (union t-free c-free) a-free)))]
          [('set! v e)
           (if (not (memq v env))
               (if (memq v *prim-names*)
                   (errorf "Attempt to set! a primitive in ~s" exp)
                   (errorf "Attempt to set! a free variable in ~s"
                     exp))
               (let-values ([(e-exp e-quotes e-poked e-free) (analyze e env)])
                 (values `(set! ,v ,e-exp)
                   e-quotes
                   (union (unit-set v) e-poked)
                   (union (unit-set v) e-free))))]
          [('lambda formals body)
           (let ([pformals (improper->proper formals)])
             (let-values ([(body-exp body-quotes body-poked body-free)
                           (analyze body (append pformals env))])
               (let ([poked (intersection body-poked pformals)]
                     [free-poked (difference body-poked pformals)]
                     [free (difference body-free pformals)])
                 (values `(lambda ,pformals
                            '(assigned . ,poked)
                            '(free . ,free)
                            '(,(if (list? formals) 'fixed 'variable))
                            ,body-exp)
                   body-quotes
                   free-poked
                   free))))]
          [else
           (let ([rator (car exp)]
                 [rands (cdr exp)])
             (let-values ([(rand-exps rand-quotes rand-poked rand-free)
                           (analyze-list rands env)])
               (if (and (symbol? rator)
                        (not (memq rator env))
                        (memq rator *prim-names*))
                   (values `(,rator . ,rand-exps)
                     rand-quotes rand-poked rand-free)
                   (let-values ([(rator-exp rator-quotes rator-poked rator-free)
                                 (analyze rator env)])
                     (values `(,rator-exp . ,rand-exps)
                       (append rator-quotes rand-quotes)
                       (union rator-poked rand-poked)
                       (union rator-free rand-free))))))]))))

(define analyze-list
  (lambda (ls env)
    (if (null? ls)
        (values '() '() '() '())
        (let-values ([(head-exp head-quotes head-poked head-free)
                      (analyze (car ls) env)]
                     [(tail-exps tail-quotes tail-poked tail-free)
                      (analyze-list (cdr ls) env)])
          (values (cons head-exp tail-exps)
            (append head-quotes tail-quotes)
            (union head-poked tail-poked)
            (union head-free tail-free))))))

;; ---------- assignmentless-form:  Removing the set! form.

(define assignmentless-form
  (lambda (exp)
    (let ([qdecls (cadr exp)]
          [subexp (caddr exp)])
      (let ([new-subexp (assignment-convert subexp '())])
        `(let ,qdecls ,new-subexp)))))

(define assignment-convert
  (lambda (exp env)
    (if (not (pair? exp))
        (if (memq exp env)
            `(%vector-ref ,exp (quote 0))
            exp)
        (match exp
          [('quote obj) `(quote ,obj)]
          [('begin a b)
           (let ([a-exp (assignment-convert a env)]
                 [b-exp (assignment-convert b env)])
             `(begin ,a-exp ,b-exp))]
          [('if t c a)
           (let ([t-exp (assignment-convert t env)]
                 [c-exp (assignment-convert c env)]
                 [a-exp (assignment-convert a env)])
             `(if ,t-exp ,c-exp ,a-exp))]
          [('set! v e)
           (let ([e-exp (assignment-convert e env)])
             `(%vector-set! ,v (quote 0) ,e-exp))]
          [('lambda formals poked free arity body)
           (let ([poked (cdadr poked)] ; remove the quote
                 [free (cdadr free)])
             (let ([new-env (union poked (difference env formals))])
               (let ([body-exp (assignment-convert body new-env)])
                 (if (null? poked)
                     `(lambda ,formals (quote (free . ,free)) ,arity ,body-exp)
                     (let ([poked-exps
                             (map (lambda (pv) `(vector ,pv)) poked)]
                           [new-frees
                             (union free (difference formals poked))])
                       `(lambda ,formals
                          (quote (free . ,free))
                          ,arity
                          ((lambda ,poked
                             '(free . ,new-frees)
                             '(fixed)
                             ,body-exp) .
                             ,poked-exps)))))))]
          [else
            (let ([rator (car exp)]
                  [rands (cdr exp)])
              (let ([rator-exp (assignment-convert rator env)]
                    [rand-exps (assignment-convert-list rands env)])
                `(,rator-exp . ,rand-exps)))]))))

(define assignment-convert-list
  (lambda (ls env)
    (map (lambda (e) (assignment-convert e env)) ls)))

;; ---------- Immediate-literal Form:  Lifting heap immediates

(define s-table '())

(define immediate-literal-form
  (lambda (exp)
    (let ([quoted (cadr exp)]
          [exp (caddr exp)])
      (if (null? quoted) exp
          (let ([q-exps (map heap-literal-destruct (map cadadr quoted))]
                [q-vars (map car quoted)])
            (let ([exp `((lambda ,q-vars '(free) '(fixed) ,exp) .
                         ,q-exps)])
              (if (null? s-table) exp
                  (let ([s-exps
                          (map symbol-destruct (map car s-table))]
                        [s-vars (map cadr s-table)])
                    `((lambda ,s-vars '(free) '(fixed) ,exp) .
                      ,s-exps)))))))))

(define heap-literal-destruct
  (lambda (obj)
    (cond
      [(symbol? obj)
       (let ([entry (assq obj s-table)])
         (if (pair? entry)
             (cadr entry)
             (let ([v (gen-ssym)])
               (set! s-table (cons (list obj v) s-table))
               v)))]
      [(or (boolean? obj) (number? obj) (char? obj) (null? obj))
       `(quote ,obj)]
      [(string? obj)
       (let ([char-exps (map (lambda (c) `(quote ,c)) (string->list obj))])
         `(string . ,char-exps))]
      [(pair? obj)
       (let ([car-exp (heap-literal-destruct (car obj))]
             [cdr-exp (heap-literal-destruct (cdr obj))])
         `(%cons ,car-exp ,cdr-exp))]
      [(vector? obj)
       (let ([contents-exps (map heap-literal-destruct (vector->list obj))])
         `(vector . ,contents-exps))])))

(define symbol-destruct
  (lambda (sym)
    (let ([char-exps (map (lambda (x) `(quote ,x))
                       (string->list (symbol->string sym)))])
      `(%string->uninterned-symbol (string . ,char-exps)))))

;; ---------- Code-generation Form: converting variables and lambdas

(define code-generation-form
  (lambda (exp)
    (cg-form-convert exp '() '())))

(define cg-form-convert
  (lambda (exp bounds frees)
    (if (not (pair? exp))
        (let ([i (my-list-index exp bounds)])
          (if i
              `(bound ,i ,exp)
              (let ([i (my-list-index exp frees)])
                (if i
                    `(free ,i ,exp)
                    exp))))               ; inline
        (match exp
          [('quote obj)
           `(quote ,obj)]
          [('begin a b)
           (let ([a-exp (cg-form-convert a bounds frees)]
                 [b-exp (cg-form-convert b bounds frees)])
             `(begin ,a-exp ,b-exp))]
          [('if t c a)
           (let ([t-exp (cg-form-convert t bounds frees)]
                 [c-exp (cg-form-convert c bounds frees)]
                 [a-exp (cg-form-convert a bounds frees)])
             `(if ,t-exp ,c-exp ,a-exp))]
          [('lambda formals quoted-frees arity body)
           (let ([free (cdadr quoted-frees)]) ; getting rid of the quote
             (let ([free-exps (cg-form-convert-list free bounds frees)]
                   [body-exp (cg-form-convert body formals free)])
               `(build-closure (lambda ,formals ,arity ,body-exp) .
                  ,free-exps)))]
          [else
           (let ([rator (car exp)] [rands (cdr exp)])
             (let ([rator-exp (cg-form-convert rator bounds frees)]
                   [rand-exps (cg-form-convert-list rands bounds frees)])
               `(,rator-exp . ,rand-exps)))]))))

(define cg-form-convert-list
  (lambda (ls bounds frees)
    (map (lambda (e) (cg-form-convert e bounds frees)) ls)))

;; ---------- Utility procedures

(define fix-list
  (lambda (lst)
    (cond ((not (pair? lst)) (cons lst '()))
          (else (cons (car lst) (fix-list (cdr lst)))))))

(define improper->proper ; seems apply this transform: (a b c . d) -> (a b c d)
  (lambda (x)
    (if (list? x) x (fix-list x))))

(define my-list-index
  (lambda (v ls)
    (list-index (lambda (x) (eq? x v)) ls)))

(define union
  (lambda (a b)
    (lset-union eq? a b)))

(define difference
  (lambda (a b)
    (lset-difference eq? a b)))

(define intersection
  (lambda (a b)
    (lset-intersection eq? a b)))

(define unit-set
  (lambda (item)
    (list item)))

(define set?
  (lambda (ls)
    (or (null? ls)
        (and (not (memq (car ls) (cdr ls)))
             (set? (cdr ls))))))

(define gen-qsym gensym)      ; variables holding quoted data
(define gen-ssym gensym)      ; variables holding symbols
