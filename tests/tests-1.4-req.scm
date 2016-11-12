(add-tests-with-string-output "if"
  [(if #t 12 13) => "12\n"]
  [(if #f 12 13) => "13\n"]
  [(if 0 12 13)  => "12\n"]
  [(if '() 43 '()) => "43\n"]
  [(if #t (if 12 13 4) 17) => "13\n"]
  [(if #f 12 (if #f 13 4)) => "4\n"]
  [(if #\X (if 1 2 3) (if 4 5 6)) => "2\n"]
  [(+ (if (- 1 1) (- 13 1) 14) 1) => "13\n"]
)

(add-tests-with-string-output "boolean"
  [(boolean? #t) => "#t\n"]
  [(boolean? #f) => "#t\n"]
  [(boolean? 1) => "#f\n"]
  [(boolean? 0) => "#f\n"]
  [(boolean? 1.0) => "#f\n"]
  [(boolean? '()) => "#f\n"]
)
