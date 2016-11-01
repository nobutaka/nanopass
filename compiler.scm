(load "./front.scm")
(load "./cps.scm")
(load "./beta-reduce.scm")
(load "./bitfield32.scm")
(load "./back.scm")
(load "./emu.scm")
(load "./local-form.scm")
(load "./macroless-form.scm")

(define build-program
  (lambda (expr)
    (x86
      (cg-top
        (code-generation-form
          (immediate-literal-form
            (assignmentless-form
              (analyzed-form
                (beta-reduce
                  (cps-form
                    (core-form
                      (local-form
                        (macroless-form
                          (append-library expr))))))))))))))
