(use srfi-1)
(use util.match)

(define (x86 program)
  (x86-assemble program "t.s")
  (printf "running gcc\n")
  (sys-system "gcc -m32 startup.c call_scheme.s t.s -o a.out"))

(define (x86-assemble code file)
  (with-output-to-file file
    (lambda ()
      (x86-spit (registerize code)))))

(define registerize
  (let ([regs '(ebp esi edi eax ebx ecx edx)])
    (lambda (thing)
      (cond
        [(pair? thing)
         (if (memq (car thing) regs)
             `(reg-off (reg ,(car thing)) ,(cadr thing))
             (map registerize thing))]
        [(and (symbol? thing) (memq thing regs))
         `(reg ,thing)]
        [else thing]))))

(define (x86-spit ls)
  (define (print-elem obj)
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
         [('asterisk)
          (printf "*")]
         [('dollar)
          (printf "$")])]
      [(string? obj)
       (printf "\t# ~a " obj)]
      [(number? obj)
       (printf "$~a" obj)]
      [else (printf "~a" obj)]))
  (define (insert-delimiter rands)
    (reverse
      (fold (lambda (exp ls)
              (if (string? exp)
                  (cons exp ls)
                  (cons exp (cons '(delim) ls))))
            (cons (car rands) '())
            (cdr rands))))
  (define (unparse-near-ptr rands)
    (append-map (lambda (exp)
                  (match exp
                    [('near-ptr x) `((asterisk) ,x)]
                    [_ (list exp)]))
                rands))
  (define (unparse-imm rands)
    (append-map (lambda (exp)
                  (match exp
                    [('imm x) `((dollar) ,x)]
                    [_ (list exp)]))
                rands))
  (printf "\t.code32\n")
  (printf "\t.align 4\n")
  (printf "\t.global _scheme_entry\n")
  (let loop ([ls (cdr ls)])
    (unless (null? ls)
      (let ([inst (car ls)])
        (case (car inst)
          [(comment)
           (printf "\t\t# ~a " (cadr inst))]
          [(label)
           (printf "~a:" (cadr inst))]
          [else
           (let ([rands (unparse-imm
                          (unparse-near-ptr
                            (insert-delimiter
                              (cdr inst))))])
             (printf "\t~s\t" (car inst))
             (for-each print-elem rands))]))
      (newline)
      (loop (cdr ls)))))

(define (printf . x)
  (apply format #t x))
