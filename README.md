# Nanopass scheme compiler in scheme for x86

This is a scheme compiler for my study.
It runs on macOS only.

## Compile fib step by step

```
$ gosh
gosh> (load "./compiler.scm")
gosh> (x86
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
                            (append-library
                              '(letrec ((fib (lambda (n)
                                               (if (= n 0)
                                                   n
                                                   (if (= n 1)
                                                       n
                                                       (+ (fib (- n 1)) (fib (- n 2))))))))
                                 (fib 35))))))))))))))
$ ./a.out
9227465
```

## Run sample

```
$ ./run samples/tinyrenderer.scm
```

## Run test

```
$ ./runtests
```

## References

1. Summer Scheme Workshop; Compiling Scheme, http://www.cs.indiana.edu/eip/compile/
1. Ur-Scheme, http://www.canonical.org/~kragen/sw/urscheme/
1. An Incremental Approach to Compiler Construction, http://schemeworkshop.org/2006/11-ghuloum.pdf
1. The 90 Minute Scheme to C compiler, http://churchturing.org/y/90-min-scc.pdf
1. scheme -> LLVM, https://web.archive.org/web/20100121052131/http://www.ida.liu.se/~tobnu/scheme2llvm
1. LYSP — 50 Years of Symbolic Processing, http://piumarta.com/software/lysp/
1. 非決定的計算オペレータ amb の並列化, http://www.principia-m.com/ts/0127/index-jp.html
1. Tiny Renderer or how OpenGL works: software rendering in 500 lines of code, https://github.com/ssloy/tinyrenderer
