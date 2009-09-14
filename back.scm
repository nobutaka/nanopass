(define closure-tag #b110)

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
                 'ac 'return 'ignored)
               (cg-code))])))))

(define (varref->address exp)
  (match exp
    [('bound n name)
     `(fp ,(* (+ n 1) ws))]
    [('free n name)
     `(cp ,(* (+ n 2) ws))]))

(define (cg exp fs dd cd nextlab)
  (match exp
    [('bound n name)
     (cg-load-branch `(fp ,(* (+ n 1) ws)) dd cd nextlab)]
    [('free n name)
     (cg-load-branch `(cp ,(* (+ n 2) ws)) dd cd nextlab)]
    [('quote obj)
     (cg-set-branch obj dd cd nextlab)]
    [('build-closure code . fvars)
     (if (eq? dd 'effect)
         (error "Not implemented")
         (let ([codelab (gen-label "code")])
           (set! todo (cons (list codelab code) todo))
           (instructions
             `(comment "build-closure")
             (cg-allocate (+ (length fvars) 2) 'ac)
             `(movl ,(length fvars) t1)
             `(movl t1 (ac 0))
             `(movl (imm ,codelab) t1)
             `(movl t1 (ac ,(* 1 ws)))
             (let f ([ls fvars] [pos 2])
               (if (null? ls)
                   (instructions)
                   (instructions
                     `(movl ,(varref->address (car ls)) t3)
                     `(movl t3 (ac ,(* pos ws)))
                     (f (cdr ls) (+ pos 1)))))
             (cg-type-tag closure-tag 'ac)
             (cg-store 'ac dd)
             `(comment "end build-closure")
             (cg-jump cd nextlab))))]
    [_
     (let ([rator (car exp)]
           [rands (cdr exp)]
           [ratorlab (gen-label "endrator")])
       (cond
         [(symbol? rator)
          (error "Not implemented")]
         [(eq? cd 'return)
          (instructions
            (cg-rands rands fs)
            (cg rator (+ fs (* (length rands) ws)) 'ac ratorlab ratorlab)
            `(label ,ratorlab)
            (cg-shuffle fs (length rands))
            `(movl ,mask cp)
            `(notl cp)
            `(andl ac cp)
            `(movl (cp ,(* 1 ws)) ac)
            `(jmp (near ac)))]
         [else
          (error "Not implemented")]))]))



(define (cg-shuffle fs num)
  (let loop ([top fs] [bot ws] [num num])
    (if (zero? num)
        (instructions)
        (instructions
          `(movl (fp ,top) t1)
          `(movl t1 (fp ,bot))
          (loop (+ top ws) (+ bot ws) (- num 1))))))

(define (cg-jump lab nextlab)
  (if (eq? lab 'return)
      (instructions
        `(movl (fp 0) t1)
        `(jmp (near t1)))
      (if (eq? lab nextlab)
          (instructions)
          (instructions
            `(jmp ,lab)))))

(define (cg-store src dest)
  (cond
    [(eq? dest 'effect)
     (instructions)]
    [(pair? dest)
     `(movl ,src ,dest)]
    [else
     (if (eq? src dest)
         (instructions)
         `(movl ,src ,dest))]))


(define (cg-load-branch loc dd cd nextlab)
  (cond
    [(eq? dd 'effect)
     (error "Not implemented")]
    [(pair? dd)
     (let ([register (car dd)]
           [offset (cadr dd)])
       (instructions
         `(movl ,loc t1)
         `(movl t1 (,register ,offset))
         (cg-jump cd nextlab)))]
    [else
     (instructions
       `(movl ,loc ,dd)
       (cg-jump cd nextlab))]))

(define (cg-set-branch obj dd cd nextlab)
  (instructions
    `(movl ,(encode obj) ,dd ,(format "~s" obj))
    (cg-jump cd nextlab)))

(define (cg-rands rands fs)
  (if (null? rands)
      (instructions)
      (let ([randlab (gen-label "rand")])
        (instructions
          (cg (car rands) fs `(fp ,fs) randlab randlab)
          `(label ,randlab)
          (cg-rands (cdr rands) (+ fs ws))))))

(define (cg-type-tag tag reg)
  `(orl ,tag ,reg))

(define (cg-allocate n target)
  (let ([n (if (even? n) n (+ n 1))])
    (instructions
      `(movl ap ,target)
      `(addl ,(* n ws) ap))))

(define gen-label
  (let ([n 0])
    (lambda (str)
      (inc! n)
      (gensym
        (string-append str (number->string n))))))
