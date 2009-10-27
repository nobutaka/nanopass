(define all-tests '())

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (set! all-tests
       (cons
         '(test-name [expr output-string] ...)
         all-tests))]))

(define build-program
  (lambda (expr)
    (x86
      (cg-top
        (code-generation-form
          (immediate-literal-form
            (assignmentless-form
              (analyzed-form
                (cps-form
                  (core-form
                    (local-form expr)))))))))))

(define execute
  (lambda ()
    (unless (zero? (sys-system "./a.out > output"))
      (error "produced program exited abnormally"))))

(define get-string
  (lambda ()
    (call-with-input-file "output" port->string)))

(define test-with-string-output
  (lambda (test-id expr expected-output)
    (build-program expr)
    (execute)
    (unless (string=? expected-output (get-string))
      (errorf "output mismatch for test ~s, expected ~s, got ~s"
        test-id expected-output (get-string)))))

(define test-one
  (lambda (test-id test)
    (let ([expr (car test)]
          [out (cadr test)])
      (printf "test ~s:~s ..." test-id expr)
      (flush (current-output-port))
      (test-with-string-output test-id expr out)
      (printf " ok\n"))))

(define test-all
  (lambda ()
    (let f ([i 0] [ls (reverse all-tests)])
      (if (null? ls)
          (printf "passed all ~s tests\n" i)
          (let ([x (car ls)] [ls (cdr ls)])
            (let ([test-name (car x)]
                  [tests (cdr x)])
              (printf "Performing ~a tests ...\n" test-name)
              (let g ([i i] [tests tests])
                (cond
                  [(null? tests) (f i ls)]
                  [else
                   (test-one i (car tests))
                   (g (+ i 1) (cdr tests))]))))))))
