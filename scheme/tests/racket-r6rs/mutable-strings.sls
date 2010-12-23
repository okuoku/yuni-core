#!r6rs

(library (yuni scheme tests racket-r6rs mutable-strings)
  (export run-mutable-strings-tests)
  (import (yuni scheme r6rs)
          (yuni scheme r6rs mutable-strings)
          (yuni scheme tests racket-r6rs test))

  (define (f) (make-string 3 #\*))
  (define (g) "***")
  
  (define (run-mutable-strings-tests)

    (test/unspec (string-set! (f) 0 #\?))
    (test/unspec-or-exn (string-set! (g) 0 #\?)
                        &assertion)
    (test/unspec-or-exn (string-set! (symbol->string 'immutable)
                                     0
                                     #\?)
                        &assertion)
      
    ;;
    ))
