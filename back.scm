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
                 'eax 'return 'ignored)
               (cg-code))])))))

(define (varref->address exp)
  (match exp
    [('bound n name)
     `(ebp ,(* (+ n 1) ws))]
    [('free n name)
     `(esi ,(* (+ n 2) ws))]))

(define (cg exp fs dd cd nextlab)
  (match exp
    [('quote obj)
     (cg-set-branch obj dd cd nextlab)]
    [('build-closure code . fvars)
     (if (eq? dd 'effect)
         (error "Not implemented")
         (let ([codelab (gen-label "code")])
           (set! todo (cons (list codelab code) todo))
           (instructions
             `(comment "build-closure")
             (cg-allocate (+ (length fvars) 2) 'eax)
             `(movl ,(length fvars) ebx)
             `(movl ebx (eax 0))
             `(movl (imm ,codelab) ebx)
             `(movl ebx (eax ,(* 1 ws)))
             (let f ([ls fvars] [pos 2])
               (if (null? ls)
                   (instructions)
                   (instructions
                     `(movl ,(varref->address (car ls)) edx)
                     `(movl edx (eax ,(* pos ws)))
                     (f (cdr ls) (+ pos 1)))))
             (cg-type-tag closure-tag 'eax)
             (cg-store 'eax dd)
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
            (cg rator (+ fs (* (length rands) ws)) 'eax ratorlab ratorlab)
            `(label ,ratorlab)
            (cg-shuffle fs (length rands))
            `(movl ,mask esi)
            `(notl esi)
            `(andl eax esi)
            `(movl (esi ,(* 1 ws)) eax)
            `(jmp (near-ptr eax)))]
         [else
          (error "Not implemented")]))]))



(define (cg-shuffle fs num)
  (let loop ([top fs] [bot ws] [num num])
    (if (zero? num)
        (instructions)
        (instructions
          `(movl (ebp ,top) ebx)
          `(movl ebx (ebp ,bot))
          (loop (+ top ws) (+ bot ws) (- num 1))))))

(define (cg-jump lab nextlab)
  (if (eq? lab 'return)
      (instructions
        `(movl (ebp 0) ebx)
        `(jmp (near-ptr ebx)))
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

(define (cg-set-branch obj dd cd nextlab)
  (instructions
    `(movl ,(encode obj) ,dd ,(format "~s" obj))
    (cg-jump cd nextlab)))

(define (cg-rands rands fs)
  (if (null? rands)
      (instructions)
      (let ([randlab (gen-label "rand")])
        (instructions
          (cg (car rands) fs `(ebp ,fs) randlab randlab)
          `(label ,randlab)
          (cg-rands (cdr rands) (+ fs ws))))))

(define (cg-type-tag tag reg)
  `(orl ,tag ,reg))

(define (cg-allocate n target)
  (let ([n (if (even? n) n (+ n 1))])
    (instructions
      `(movl edi ,target)
      `(addl ,(* n ws) edi))))

(define gen-label
  (let ([n 0])
    (lambda (str)
      (inc! n)
      (gensym
        (string-append str (number->string n))))))
