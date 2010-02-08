(use binary.io)
(use gauche.uvector)

(define pair-tag    #b010)
(define string-tag  #b011)
(define symbol-tag  #b100)
(define vector-tag  #b101)
(define closure-tag #b110)
(define float-tag   #b111)

(define mask        #b111)

(define tag-len 3)

(define bool-tag #b00000001)
(define null-tag #b00001001)
(define char-tag #b00010001)

(define imm-mask #b11111111)

(define ws 4)

(define encode
  (let ([numtop (expt 2 29)])
    (lambda (obj)
      (cond
        [(exact? obj)
         (cond
           [(and (<= 0 obj) (< obj numtop)) (* obj (+ mask 1))]
           [(and (<= (- numtop) obj) (< obj 0)) (* (+ numtop obj) (+ mask 1))]
           [else
            (errorf "~s is out of range" obj)])]
        [(inexact? obj)
         (logior (get-u32 (f32vector obj) 0) float-tag)]
        [(boolean? obj)
         (+ (* (if obj 1 0) (+ imm-mask 1)) bool-tag)]
        [(null? obj) null-tag]
        [(char? obj)
         (let ([val (char->integer obj)])
           (+ (* val (+ imm-mask 1)) char-tag))]
        [else
         (errorf "~s not encodable" obj)]))))

(define encode-regs
  (let ([symbols '(ac t1 t2 t3)])
    (let ([bits (map (lambda (symbol index)
                       (cons symbol (ash 1 index)))
                     symbols
                     (iota (length symbols)))])
      (lambda (regs)
        (fold (lambda (reg val)
                (logior val (cdr (assq reg bits))))
              0
              regs)))))

(define header
  (lambda (len tag)
    (ash (logior (ash len tag-len) tag) 1)))

(define instructions
  (lambda args
    (cons 'instructions
      (let loop ([ls args])
        (if (null? ls)
            `()
            (if (eq? (caar ls) 'instructions)
                (append (cdar ls)
                  (loop (cdr ls)))
                (cons (car ls)
                  (loop (cdr ls)))))))))

(define todo '()) ; ((label code) ...)

(define cg-top
  (lambda (exp)
    (set! todo
      (cons (list '_scheme_entry `(lambda () '(fixed) ,exp)) todo))
    (cg-code)))

(define cg-code
  (lambda ()
    (if (null? todo)
        (instructions)
        (let ([first (car todo)]
              [rest (cdr todo)])
          (set! todo rest)
          (let ([label (car first)])
            (match (cadr first)
              [('lambda formals arity body)
               (instructions
                 `(label ,label)
                 (cg-prologue formals arity)
                 (cg body (* (+ (length formals) 1) ws)
                   'ac 'return 'ignored)
                 (cg-code))]))))))

(define cg-prologue
  (lambda (formals arity)
    (let ([correctlab (gen-label "correctarg")]
          [is-var (eq? (caadr arity) 'variable)])
      (let ([leastlen (- (length formals) (if is-var 1 0))])
        (instructions
          ;; check number of arguments
          `(movl t1 t3)       ; t3=number of arguments
          `(cmpl ,leastlen t3)
          `(jae ,correctlab)  ; leastlen<=t3
          faultcode           ; Missing argument
          `(label ,correctlab)
          ;; pack arguments if arity is variable
          (if is-var
              (let ([looplab (gen-label "packloop")]
                    [dontlab (gen-label "dontpack")])
                (instructions
                  `(comment "pack arguments")
                  `(movl ,(encode '()) ac)  ; ac=list
                  `(cmpl ,leastlen t3)
                  `(je ,dontlab)
                  `(movl t3 t1)       ; t1=address of obj1 on stack. it's under the stack top.
                  `(sall 2 t1)        ; t1=t1*ws
                  `(addl fp t1)
                  `(movl fp t3)       ; t3=end
                  `(addl ,(* leastlen ws) t3) ; TODO: don't have to emit code if leastlen is zero
                  `(label ,looplab)
                  `(movl ac t2)       ; t2=obj2
                  (cg-alloc-pair      ; ac=(cons obj1 obj2)
                    (instructions
                      `(movl t1 ac)
                      `(addl ,ws ac))
                    '(t2)
                    (instructions
                      `(movl t2 (ac ,(* 2 ws)))
                      `(movl (t1 0) t2)
                      `(movl t2 (ac ,(* 1 ws)))))
                  `(subl ,ws t1)      ; t1=t1-ws
                  `(cmpl t3 t1)       ; if t3<t1 goto looplab
                  `(ja ,looplab)
                  `(label ,dontlab)
                  `(movl ac (fp ,(* (length formals) ws)))
                  `(comment "end pack arguments")))
              (instructions)))))))

(define varref->address
  (lambda (exp)
    (match exp
      [('bound n name)
       `(fp ,(* (+ n 1) ws))]
      [('free n name)
       `(cp ,(* (+ n 2) ws))])))

(define cg
  (lambda (exp fs dd cd nextlab)
    (match exp
      [('bound n name)
       (cg-load-branch `(fp ,(* (+ n 1) ws)) dd cd nextlab)]
      [('free n name)
       (cg-load-branch `(cp ,(* (+ n 2) ws)) dd cd nextlab)]
      [('quote obj)
       (cg-set-branch obj dd cd nextlab)]
      [('if t c a)
       (let ([truelab (gen-label "iftrue")]
             [falselab (gen-label "iffalse")])
         (instructions
           (cg t fs 'effect (join-labels truelab falselab) truelab)
           `(label ,truelab)
           (cg c fs dd cd falselab)
           `(label ,falselab)
           (cg a fs dd cd nextlab)))]
      [('build-closure code . fvars)
       (if (eq? dd 'effect)
           (error "Error in build-closure: Not implemented")
           (let ([codelab (gen-label "code")])
             (set! todo (cons (list codelab code) todo))
             (instructions
               `(comment "build-closure")
               (cg-allocate (+ (length fvars) 2) 'ac fs '())
               `(movl ,(header (length fvars) closure-tag) (ac 0))
               `(movl (imm ,codelab) (ac ,(* 1 ws)))
               (let f ([ls fvars] [pos 2])
                 (if (null? ls)
                     (instructions)
                     (instructions
                       `(movl ,(varref->address (car ls)) t1)
                       `(movl t1 (ac ,(* pos ws)))
                       (f (cdr ls) (+ pos 1)))))
               (cg-type-tag closure-tag 'ac)
               (cg-store 'ac dd)
               `(comment "end build-closure")
               (cg-jump cd nextlab))))]
      [else
       (let ([rator (car exp)]
             [rands (cdr exp)]
             [ratorlab (gen-label "endrator")])
         (cond
           [(and (eq? rator '%apply) (eq? cd 'return))
            (instructions
              `(comment "apply")
              ;; expand args and shuffle the stack at once
              (cg-ternary-rands rands fs)   ; t1=cont-exp, t2=proc, t3=args
              `(movl t1 (fp ,ws))           ; push cont-exp to stack
              `(movl fp t1)                 ; t1=stack top
              `(addl ,(* 2 ws) t1)
              (let ([looplab (gen-label "apploop")]
                    [breaklab (gen-label "appbreak")])
                (instructions
                  `(label ,looplab)
                  `(cmpl ,(encode '()) t3)
                  `(je ,breaklab)
                  `(movl (t3 ,(- ws pair-tag)) ac)        ; push car to stack
                  `(movl ac (t1 0))
                  `(addl ,ws t1)
                  `(movl (t3 ,(- (* 2 ws) pair-tag)) t3)  ; t3=cdr
                  `(jmp ,looplab)
                  `(label ,breaklab)))
              ;; pass the number of arguments
              `(subl fp t1)   ; t1=number of arguments
              `(subl ,ws t1)  ; get rid of return address
              `(sarl 2 t1)    ; t1=t1/ws
              ;; call
              `(movl t2 ac)   ; ac=proc
              (cg-jump-closure)
              `(comment "end apply"))]
           [(symbol? rator)
            (cg-inline exp rator rands fs dd cd nextlab)]
           [(eq? cd 'return)
            (instructions
              (cg-rands rands fs)
              (cg rator (+ fs (* (length rands) ws)) 'ac ratorlab ratorlab)
              `(label ,ratorlab)
              (cg-shuffle fs (length rands))
              `(movl ,(length rands) t1)
              (cg-jump-closure))]
           [else
            (error "Error in else: Not implemented")]))])))



(define cg-shuffle
  (lambda (fs num)
    (let loop ([top fs] [bot ws] [num num])
      (if (zero? num)
          (instructions)
          (instructions
            `(movl (fp ,top) t1)
            `(movl t1 (fp ,bot))
            (loop (+ top ws) (+ bot ws) (- num 1)))))))

(define cg-jump
  (lambda (lab nextlab)
    (if (eq? lab 'return)
        (instructions
          `(movl (fp 0) t1)
          `(jmp (near t1)))
        (if (eq? lab nextlab)
            (instructions)
            (instructions
              `(jmp ,lab))))))

(define cg-jump-closure
  (lambda ()
    (instructions
      `(movl ,mask cp)
      `(notl cp)
      `(andl ac cp)
      `(movl (cp ,(* 1 ws)) ac)
      `(jmp (near ac)))))

(define cg-branch
  (lambda (truelab falselab nextlab jump-if-true jump-if-false)
    (instructions
      (cond
        [(eq? truelab nextlab)
         `(,jump-if-false ,falselab)]
        [(eq? falselab nextlab)
         `(,jump-if-true ,truelab)]
        [else
          (instructions
            `(,jump-if-true ,truelab)
            `(jmp ,falselab))]))))

(define cg-store
  (lambda (src dest)
    (cond
      [(eq? dest 'effect)
       (instructions)]
      [(pair? dest)
       `(movl ,src ,dest)]
      [else
       (if (eq? src dest)
           (instructions)
           `(movl ,src ,dest))])))


(define cg-load-branch
  (lambda (loc dd cd nextlab)
    (cond
      [(eq? dd 'effect)
       (cond
         [(pair? cd)
          (let ([truelab (car cd)]
                [falselab (cadr cd)])
            (instructions
              `(movl ,loc t1)
              `(cmpl ,(encode #f) t1)
              (cg-branch truelab falselab nextlab 'jne 'je)))]
         [else
          (cg-jump cd nextlab)])]
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
         (cg-jump cd nextlab))])))

(define cg-set-branch
  (lambda (obj dd cd nextlab)
    (instructions
      `(movl ,(encode obj) ,dd ,(format "~s" obj))
      (cg-jump cd nextlab))))

(define cg-rands
  (lambda (rands fs)
    (if (null? rands)
        (instructions)
        (let ([randlab (gen-label "rand")])
          (instructions
            (cg (car rands) fs `(fp ,fs) randlab randlab)
            `(label ,randlab)
            (cg-rands (cdr rands) (+ fs ws)))))))

(define cg-effect-rands
  (lambda (ls fs)
    (if (null? ls)
        (instructions)
        (let ([randlab (gen-label "rand")])
          (instructions
            (cg (car ls) fs 'effect randlab randlab)
            `(label ,randlab)
            (cg-effect-rands (cdr ls) fs))))))

(define cg-unary-rand
  (lambda (rands fs)
    (let ([rand (car rands)])
      (let ([endlab (gen-label "unaryrand")])
        (instructions
          (cg rand fs 't1 endlab endlab)
          `(label ,endlab))))))

(define cg-binary-rands
  (lambda (rands fs)
    (let ([r0 (car rands)]
          [r1 (cadr rands)])
      (let ([r0lab (gen-label "binary0")]
            [r1lab (gen-label "binary1")])
        (instructions
          (cg r0 fs `(fp ,fs) r0lab r0lab)
          `(label ,r0lab)
          (cg r1 (+ fs (* 1 ws)) 'ac r1lab r1lab)
          `(label ,r1lab)
          `(movl ac t2)
          `(movl (fp ,fs) t1))))))

(define cg-ternary-rands
  (lambda (rands fs)
    (let ([r0 (car rands)]
          [r1 (cadr rands)]
          [r2 (caddr rands)])
      (let ([r0lab (gen-label "ternary0")]
            [r1lab (gen-label "ternary1")]
            [r2lab (gen-label "ternary2")])
        (instructions
          (cg r0 fs `(fp ,fs) r0lab r0lab )
          `(label ,r0lab)
          (cg r1 (+ fs (* 1 ws)) `(fp ,(+ fs (* 1 ws))) r1lab r1lab)
          `(label ,r1lab)
          (cg r2 (+ fs (* 2 ws)) 'ac r2lab r2lab)
          `(label ,r2lab)
          `(movl ac t3)
          `(movl (fp ,(+ fs (* 1 ws))) t2)
          `(movl (fp ,fs) t1))))))

(define cg-inline
  (lambda (exp name rands fs dd cd nextlab)
    (case name
      [(%eq?)
       (cg-binary-pred-inline exp rands fs dd cd nextlab 'je 'jne
         `(cmpl t1 t2))]
      [(%fx+)
       (cg-true-inline cg-binary-rands rands fs dd cd nextlab
         (instructions
           `(movl t1 ac)
           `(addl t2 ac)))]
      [(%fx-)
       (cg-true-inline cg-binary-rands rands fs dd cd nextlab
         (instructions
           `(movl t1 ac)
           `(subl t2 ac)))]
      [(%fl+)
       (cg-true-inline cg-binary-rands rands fs dd cd nextlab
         (instructions
           `(andl ,(not32 mask) t1)
           `(andl ,(not32 mask) t2)
           `(movd t1 xmm0)
           `(movd t2 xmm1)
           `(addss xmm1 xmm0)
           `(movd xmm0 ac)
           (cg-type-tag float-tag 'ac)))]
      [(%car)
       (cg-ref-inline cg-unary-rand rands fs dd cd nextlab
         `(movl (t1 ,(- ws pair-tag)) ac))]
      [(%cdr)
       (cg-ref-inline cg-unary-rand rands fs dd cd nextlab
         `(movl (t1 ,(- (* 2 ws) pair-tag)) ac))]
      [(%cons)
       (cg-true-inline cg-binary-rands rands fs dd cd nextlab
         (cg-alloc-pair (cg-stacktop fs) '(t1 t2)
           (instructions
             `(movl t1 (ac ,(* 1 ws)))
             `(movl t2 (ac ,(* 2 ws))))))]
      [(%string->uninterned-symbol)
       (cg-true-inline cg-unary-rand rands fs dd cd nextlab
         (instructions
           (cg-allocate 2 'ac fs '(t1))
           `(movl ,(header 1 symbol-tag) (ac 0))
           `(movl t1 (ac ,ws))
           (cg-type-tag symbol-tag 'ac)))]
      [(string)
       (cg-true-inline cg-rands rands fs dd cd nextlab
         (instructions
           `(comment "string")
           (cg-allocate (+ (quotient (+ (length rands) (- ws 1)) ws) 1) 'ac (+ fs (* (length rands) ws)) '())
           `(movl ,(header (length rands) string-tag) (ac 0))
           (let loop ([fpos fs] [spos ws] [num (length rands)])
             (if (zero? num)
                 (instructions)
                 (instructions
                   `(movl (fp ,fpos) t1)
                   `(shr 8 t1)          ; 8 bits for the char tag
                   `(movb t1l (ac ,spos))
                   (loop (+ fpos ws) (+ spos 1) (- num 1)))))
           (cg-type-tag string-tag 'ac)
           `(comment "end string")))]
      [(vector)
       (cg-true-inline cg-rands rands fs dd cd nextlab
         (instructions
           `(comment "vector")
           (cg-allocate (+ (length rands) 1) 'ac (+ fs (* (length rands) ws)) '())
           `(movl ,(header (length rands) vector-tag) (ac 0))
           (let loop ([fpos fs] [vpos 1] [num (length rands)])
             (if (zero? num)
                 (instructions)
                 (instructions
                   `(movl (fp ,fpos) t1)
                   `(movl t1 (ac ,(* vpos ws)))
                   (loop (+ fpos ws) (+ vpos 1) (- num 1)))))
           (cg-type-tag vector-tag 'ac)
           `(comment "end vector")))]
      [(%vector-ref)
       (cg-ref-inline cg-binary-rands rands fs dd cd nextlab
         (instructions
           `(sarl 1 t2)
           `(addl t2 t1)
           `(movl (t1 ,(- ws vector-tag)) ac)))]
      [(%vector-set!)
       (instructions
         (cg-ternary-rands rands fs)
         `(comment "vector-set")
         `(sarl 1 t2)
         `(addl t2 t1)
         `(movl t3 (t1 ,(- ws vector-tag)))
         `(comment "end vector-set")
         (if (eq? dd 'effect)
             (error "Error in vector-set!: Not implemented")
             (instructions
               (cg-store 't3 dd)        ; why not?
               (cg-jump cd nextlab))))]
;      [(foreign-call)
;       (cg-ref-inline cg-rands rands fs dd cd nextlab ; TODO: cg-rands only pushes value to scheme stack.
;         (instructions
;           `(comment "foreign-call")
;           `(pushl 3)
;           `(pushl 2)
;           `(pushl 1)
;           `(pushl sp)
;           `(call foreign_call)
;           `(addl ,(* 4 ws) sp)
;           `(comment "end foreign-call")))]
      [else
       (errorf "sanity-check: bad primitive ~s" name)])))

(define cg-alloc-pair
  (lambda (frameinfocode usedregs buildcode)
    (instructions
      (cg-allocate-frameinfo 3 'ac frameinfocode usedregs)
      `(movl ,(header 2 pair-tag) (ac 0))
      buildcode
      (cg-type-tag pair-tag 'ac))))

(define cg-true-inline
  (lambda (rander rands fs dd cd nextlab code)
    (if (eq? dd 'effect)
        (error "Error in cg-true-inline: Not implemented")
        (instructions
          (rander rands fs)
          code
          (cg-store 'ac dd)
          (cg-jump cd nextlab)))))

(define cg-ref-inline
  (lambda (rander rands fs dd cd nextlab code)
    (if (eq? dd 'effect)
        (error "Error in cg-ref-inline: Not implemented")
        (instructions
          (rander rands fs)
          code
          (cg-store 'ac dd)
          (cg-jump cd nextlab)))))

(define cg-binary-pred-inline
  (lambda (exp rands fs dd cd nextlab trueinst falseinst code)
    (if (eq? dd 'effect)
        (if (pair? cd)
            (let ([truelab (car cd)]
                  [falselab (cadr cd)])
              (instructions
                (cg-binary-rands rands fs)
                code
                (cg-branch truelab falselab nextlab trueinst falseinst)))
            (instructions
              (cg-effect-rands rands fs)
              (cg-jump cd nextlab)))
        (cg `(if ,exp '#t '#f) fs dd cd nextlab))))

(define cg-type-tag
  (lambda (tag reg)
    `(orl ,tag ,reg)))

(define cg-stacktop
  (lambda (fs)
    (instructions
      `(movl fp ac)
      `(addl ,fs ac))))

(define cg-allocate
  (lambda (n target fs usedregs)
    (cg-allocate-frameinfo n target (cg-stacktop fs) usedregs)))

(define cg-allocate-frameinfo
  (lambda (n target frameinfocode usedregs)
    (letrec ([aligned (if (even? n) n (+ n 1))]
             [allocate
               (lambda (overflowcode)
                 (let ([dontlab (gen-label "dontgc")])
                   (instructions
                     `(movl ap ,target)
                     `(addl ,(* aligned ws) ap)
                     `(cmpl _heap_end ap)
                     `(jbe ,dontlab)
                     overflowcode
                     `(label ,dontlab))))])
      (allocate
        (instructions
          `(comment "gc")
          `(subl ,(* aligned ws) ap)  ; revert ap
          `(pushl t3)
          `(pushl t2)
          `(pushl t1)
          `(pushl ac)
          `(pushl ap)
          `(pushl cp)
          frameinfocode
          `(pushl ac)           ; push stack top
          `(pushl ,(encode-regs usedregs))
          `(movl sp ac)
          `(subl ,(* 3 ws) sp)  ; padding bytes for OS X
                                ; 8 words struct + 3 paddings + 1 pointer = 12 words
          `(pushl ac)
          `(call _gc_collect)
          `(addl ,(* 6 ws) sp)  ; skip pointer, paddings, usedregs, stack_top
          `(popl cp)
          `(popl ap)
          `(popl ac)
          `(popl t1)
          `(popl t2)
          `(popl t3)
          (allocate faultcode)  ; No more memory.
          `(comment "end gc"))))))

(define faultcode
  (instructions
    `(movl 0 ac)
    `(movl (ac 0) ac))) ; This may causes segmentation fault.

(define join-labels
  (lambda (a b)
    (cond
      [(pair? a)
       (join-labels (car a) b)]
      [(pair? b)
       (list a (cadr b))]
      [else
       (list a b)])))

(define gen-label
  (let ([n 0])
    (lambda (str)
      (inc! n)
      (gensym
        (string-append str (number->string n))))))
