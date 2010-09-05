(library (yuni core)
         (export ~ :=)
         (import (rnrs) 
                 (yuni miniobj)
                 (yuni miniobj minitype)
                 (yuni util invalid-form))

; internal
(define-syntax ref
  (syntax-rules ()
    ((_ target slot)
     (miniobj-ref target slot))))

; internal
(define-syntax refset!
  (syntax-rules ()
    ((_ target slot value)
     (miniobj-set! target slot value))))

; ~: generic, recursive ref/set syntax.
(define-syntax ~
  (syntax-rules (:=)
    ((_ target slot := obj)
     (refset! target slot obj))
    ((_ target slot)
     (ref target slot))
    ((_ target slot next-slot ...)
     (~ (ref target slot) next-slot ...))))

(define-invalid-form :=)

)