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

(add-tests-with-string-output "and"
  [(and (= 2 2) (< 1 2)) => "#t\n"]
  [(and (= 2 2) (< 2 1)) => "#f\n"]
  [(and 1 2) => "2\n"]
  [(and #f #t) => "#f\n"]
  [(and #t #t) => "#t\n"]
)

(add-tests-with-string-output "or"
  [(or (= 2 2) (< 1 2)) => "#t\n"]
  [(or (= 2 2) (< 2 1)) => "#t\n"]
  [(or #f #f) => "#f\n"]
  [(or #f #t) => "#t\n"]
  [(or 1 2) => "1\n"]
)

(add-tests-with-string-output "boolean"
  [(boolean? #t) => "#t\n"]
  [(boolean? #f) => "#t\n"]
  [(boolean? 1) => "#f\n"]
  [(boolean? 0) => "#f\n"]
  [(boolean? 1.0) => "#f\n"]
  [(boolean? '()) => "#f\n"]
)

(add-tests-with-string-output "not"
  [(not #t) => "#f\n"]
  [(not #f) => "#t\n"]
  [(not 1) => "#f\n"]
  [(not '()) => "#f\n"]
)

(add-tests-with-string-output "set!"
  ; regression test
  [(lambda (steep)
     (if steep 1 2)
     (set! steep 3)) => "<procedure>\n"]
)
