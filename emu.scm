(define (x86 program)
  (x86-assemble program "t.s")
  (printf "running gcc\n")
  (sys-system "gcc t.s -o a.out"))

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
  (define (print-elem obj delim)
    (cond
      [(pair? obj)
       (match obj
         [('reg name)
          (printf "~a%~s" delim name)]
         [('reg-off reg off)
          (let ([name (cadr reg)])
            (printf "~a~s(%~s)" delim off name))])]
       [(string? obj)
        (printf "\t# ~a " obj)]
       [else (printf "~a~a" delim obj)]))
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
           (let ([first (cadr inst)]
                 [rest (cddr inst)])
             (printf "\t~s\t" (car inst))
             (print-elem first "")
             (for-each (lambda (x) (print-elem x ", "))
               rest))]))
      (newline)
      (loop (cdr ls)))))

(define (printf . x)
  (apply format #t x))
