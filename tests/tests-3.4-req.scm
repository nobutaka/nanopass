(add-tests-with-string-output "nontail apply"
  [(let ([f (lambda () 12)])
     (fx+ (apply f '()) 1)) => "13\n"]
  [(let ([f (lambda (x) (fx+ x 12))])
     (fx+ (apply f (cons 13 '())) 1)) => "26\n"]
)

(add-tests-with-string-output "tail apply"
  [(let ([f (lambda () 12)])
     (apply f '())) => "12\n"]
  [(let ([f (lambda (x) (fx+ x 12))])
     (apply f (cons 13 '()))) => "25\n"]
)
