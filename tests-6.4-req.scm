(add-tests-with-string-output "uniform vectors"
  [(u8vector-length (make-u8vector 0)) => "0\n"]
  [(u8vector-length (make-u8vector 1)) => "1\n"]
  [(u8vector-length (make-u8vector 1000)) => "1000\n"]
  [(let ([v (make-u8vector 1)])
     (u8vector-set! v 0 12)
     (u8vector-ref v 0)) => "12\n"]
  [(let ([v (make-u8vector 1)])
     (u8vector-set! v 0 12)
     (u8vector-set! v 0 13)
     (u8vector-set! v 0 (u8vector-ref v 0))
     (u8vector-ref v 0)) => "13\n"]
  [(let ([v (make-u8vector 2)])
     (u8vector-set! v 1 13)
     (u8vector-set! v 0 12)
     (u8vector-ref v 1)) => "13\n"]
  [(let ([v (make-u8vector 2)])
     (u8vector-set! v 0 12)
     (u8vector-set! v 1 13)
     (u8vector-ref v 0)) => "12\n"]
  [(let ([v (make-u8vector 1)])
     (u8vector-set! v 0 255)
     (u8vector-ref v 0)) => "255\n"]
  [(let ([v (make-u8vector 1000)])
     (u8vector-set! v 500 12)
     (u8vector-ref v 500)) => "12\n"]
)
