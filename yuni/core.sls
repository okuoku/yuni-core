(library (yuni core)
         (export ~ := 
                 define-composite
                 let-with let-with*
                 make touch!)
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

; define-composite
(define-syntax define-composite
  (syntax-rules ()
    ((_ typename slots)
     (define-minitype typename slots))))

; ~new
(define-syntax ~new
  (syntax-rules ()
    ((_ typename)
     (make-minitype-obj typename))))

; let-with
(define-syntax let-with
  (syntax-rules ()
    ((_ OBJ (specs0 specs1 ...) body ...)
     (let-with-binder OBJ specs0
                      (let-with OBJ (specs1 ...) body ...)))
    ((_ OBJ (specs0) body ...)
     (let-with-binder OBJ specs0 body ...))))

(define-syntax let-with*
  (syntax-rules ()
    ((_ (specs0 specs1 ...) body ...)
     (let-with specs0 (let-with* (specs1 ...) body ...)))
    ((_ (specs0) body ...)
     (let-with specs0 body ...))))

(define-syntax let-with-binder
  (syntax-rules ()
    ((_ OBJ (bindname name) body ...)
     (let ((bindname (~ OBJ 'name)))
       body ...))
    ((_ OBJ name body ...)
     (let ((name (~ OBJ 'name)))
       body ...))))

; make
(define-syntax make-apply-rule
  (syntax-rules ()
    ((_ NAME (slot body))
     (let ((result body))
       (~ NAME 'slot := result)))))

(define-syntax make
  (syntax-rules ()
    ((_ TYPE rule0 ...)
     (let ((new-object ((~new TYPE))))
       (make-apply-rule new-object rule0)
       ...
       new-object))))

(define-syntax touch!-apply-spec!
  (syntax-rules ()
    ((_ OBJ (slot body))
     (~ OBJ 'slot := body))))

(define-syntax touch!
  (syntax-rules ()
    ((_ OBJ spec0 ...)
     (touch!-apply-spec! OBJ spec0)
     ...
     OBJ)))

(define-invalid-form :=)

)
