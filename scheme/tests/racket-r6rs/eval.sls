#!r6rs

(library (yuni scheme tests racket-r6rs eval)
  (export run-eval-tests)
  (import (yuni scheme r6rs)
          (yuni scheme r6rs eval)
          (yuni scheme tests racket-r6rs test))

  (define (run-eval-tests)

    (test (eval '(let ((x 3)) x)
                (environment '(rnrs)))
          3)

    (test (eval
           '(eval:car (eval:cons 2 4))
           (environment
            '(prefix (only (rnrs) car cdr cons null?)
                     eval:)))
          2)

    ;; Check that `eval' at compile-time produces values (such as conditions)
    ;; that make sense at compile time (i.e., no phase crossing):
    (test (eval
           '(let-syntax ([x (lambda (stx)
                              (datum->syntax
                               #'here
                               (condition-message
                                (call/cc
                                 (lambda (esc)
                                   (with-exception-handler
                                    (lambda (exn) (esc exn))
                                    (lambda ()
                                      (eval '(assertion-violation 'exptime "ok")
                                            (environment
                                             '(rnrs)
                                             '(rnrs eval))))))))))])
              x)
           (environment '(rnrs) '(for (rnrs eval) expand)))
          "ok")

    ;;
    ))

