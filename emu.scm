(use srfi-1)
(use util.match)

(define *directive* "")

(define x86
  (lambda (program)
    (x86-assemble program "t.s")
    (unless (zero? (sys-system #"gcc -m32 -Wl,-no_pie -g ~*directive* startup.c call_scheme.s t.s -o a.out"))
      (error "could not build target"))))

(define x86-assemble
  (lambda (code file)
    (with-output-to-file file
      (lambda ()
        (x86-spit (registerize code))))))

(define registerize
  (let ([regs '((sp . esp) (fp . ebp) (cp . esi) (ap . edi) (ac . eax) (t1 . ebx) (t2 . ecx) (t3 . edx) (t1l . bl) (t2l . cl) (xmm0 . xmm0) (xmm1 . xmm1))])
    (lambda (thing)
      (cond
        [(pair? thing)
         (let ([x (assq (car thing) regs)])
           (if x
               `(reg-off (reg ,(cdr x)) ,(cadr thing))
               (map registerize thing)))]
        [(and (symbol? thing) (assq thing regs)) =>
         (lambda (x)
           `(reg ,(cdr x)))]
        [else thing]))))

(define x86-spit
  (lambda (ls)
    (printf "\t.code32\n")
    (printf "\t.align 4\n")
    (printf "\t.globl _scheme_entry\n")
    (let loop ([ls (cdr ls)])
      (unless (null? ls)
        (let ([inst (car ls)])
          (case (car inst)
            [(comment)
             (printf "\t# ~a" (cadr inst))]
            [(label)
             (printf "~a:" (cadr inst))]
            [else
             (let ([rands (insert-delimiter (cdr inst))])
               (printf "\t~s\t" (car inst))
               (for-each print-elem rands))]))
        (newline)
        (loop (cdr ls))))))

(define print-elem
  (lambda (obj)
    (cond
      [(pair? obj)
       (match obj
         [('reg name)
          (printf "%~s" name)]
         [('reg-off reg off)
          (let ([name (cadr reg)])
            (printf "~s(%~s)" off name))]
         [('delim)
          (printf ", ")]
         [('near x)
          (printf "*")
          (print-elem x)]
         [('imm x)
          (printf "$")
          (print-elem x)])]
      [(string? obj)
       (printf "\t/* ~a */" obj)]
      [(number? obj)
       (printf "$~a" obj)]
      [else (printf "~a" obj)])))

(define insert-delimiter
  (lambda (rands)
    (reverse
      (fold (lambda (exp ls)
              (if (string? exp)
                  (cons exp ls)
                  (cons exp (cons '(delim) ls))))
            (cons (car rands) '())
            (cdr rands)))))

(define printf
  (lambda x
    (apply format #t x)))
