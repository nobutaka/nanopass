(add-tests-with-string-output "call/cc"
  [(call/cc (lambda (k) 12)) => "12\n"]
  [(call/cc (lambda (k) (k 12))) => "12\n"]
  [(call/cc (lambda (k) (+ 1 (k 12)))) => "12\n"]
  [(+ (call/cc (lambda (k) (k 12)))
      (call/cc (lambda (k) 13))) => "25\n"]
)
