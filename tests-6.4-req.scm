(add-tests-with-string-output "uniform vectors"
  [(u8vector-length (make-u8vector 0)) => "0\n"]
  [(u8vector-length (make-u8vector 1)) => "1\n"]
  [(u8vector-length (make-u8vector 1000)) => "1000\n"]
)
