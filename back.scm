(define mask        #b111)

(define ws 4)

(define encode
  (let ([numtop (expt 2 29)])
    (lambda (obj)
      (cond
        [(number? obj)
         (cond
           [(and (<= 0 obj) (< obj numtop)) (* obj (+ mask 1))]
           [(and (<= (- numtop) obj) (< obj 0)) (* (+ numtop obj) (+ mask 1))]
           [else
            (error "~s is out of range" obj)])]
        [else
         (error "~s not encodable" obj)]))))

(define (instructions . args)
  (cons 'instructions
    (let loop ([ls args])
      (if (null? ls)
          `()
          (if (eq? (caar ls) 'instructions)
              (append (cdar ls)
                (loop (cdr ls)))
              (cons (car ls)
                (loop (cdr ls))))))))

(define todo '()) ; ((label code) ...)

(define (cg-top exp)
  (set! todo
    (cons (list '_scheme_entry `(lambda () ,exp)) todo))
  (cg-code))

(define (cg-code)
  (if (null? todo)
      (instructions)
      (let ([first (car todo)]
            [rest (cdr todo)])
        (set! todo rest)
        (let ([label (car first)])
          (match (cadr first)
            [('lambda formals body)
             (instructions
               `(label ,label)
               (cg body (* (+ (length formals) 1) ws)
                 'eax 'return 'ignored)
               (cg-code))])))))

(define (cg exp fs dd cd nextlab)
  (match exp
    [('quote obj)
     (cg-set-branch obj dd cd nextlab)]))

(define (cg-jump lab nextlab)
  (instructions
    `(movl (ebp 0) ebx)
    `(jmp (near-ptr ebx))))

(define (cg-set-branch obj dd cd nextlab)
  (instructions
    `(movl ,(encode obj) ,dd ,(format "~s" obj))
    (cg-jump cd nextlab)))
