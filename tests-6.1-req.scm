(add-tests-with-string-output "gc"
  [(letrec ((fib (lambda (n) (if (= n 0) n (if (= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 35)) => "9227465\n"]
  [(letrec ((accum (lambda (n str) (if (= n 1000000) str (accum (+ n 1) (string #\a)))))) (accum 0 (string #\a))) => "\"a\"\n"]
  [(letrec ((accum (lambda (n sym) (if (= n 1000000) sym (accum (+ n 1) (string->uninterned-symbol (string #\a))))))) (accum 0 (string->uninterned-symbol (string #\a)))) => "a\n"]
)
