#!r6rs

(library (yuni scheme tests racket-r6rs programs)
  (export run-programs-tests)
  (import (yuni scheme r6rs)
          (yuni scheme tests racket-r6rs test))

  (define (run-programs-tests)

    (test (list? (command-line)) #t)
    (test (string? (car (command-line))) #t)
      
    ;;
    ))

