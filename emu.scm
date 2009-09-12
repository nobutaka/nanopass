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
