(add-tests-with-string-output "symbols"
  [(eq? 'foo 'bar) => "#f\n"]
  [(eq? 'foo 'foo) => "#t\n"]
  ['foo => "foo\n"]
)
