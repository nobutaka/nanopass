(define too-big32 (expt 2 32))

(define negative-one32 (- too-big32 1))

(define not32 (lambda (a) (- negative-one32 a)))
