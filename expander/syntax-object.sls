(library (yuni expander syntax-object)
         (export 
           ;; util
           make-new-template
           make-library-binding
           identifier?
           env-lookup
           env-append/macros!
           env-append/variables!
           binding-macro?
           binding-variable?
           binding-transformer
           binding-name

           ;; composite
           identifier

           ;; rnrs
           bound-identifier=?
           free-identifier=?
           syntax->datum
           datum->syntax)
         (import (yuni impl base)
                 (only (rnrs) write newline)
                 (yuni core))

;;; color object
;;; FIXME!: this sould be thread-local
(define *color-count* 1)
(define color=? =)
(define (new-color)
  (set! *color-count* (+ *color-count* 1))
  *color-count*)

;;; identifier object
;;;  name: identifier source symbol used at syntax->datum
;;;  global-name: resolved global name for this id
;;;  phases: identifier availability
;;;  color: macro invocation ID
;;;  env: environment object
;;;  debug: identifier source position (if available)
(define-composite identifier 
                  (name global-name phases color env debug))

(define* (bound-identifier=? 
           (id0 identifier)
           (id1 identifier))
 (and 
    (eq? (~ id0 'name)
         (~ id1 'name))
    (color=? (~ id0 'color)
             (~ id1 'color))))

(define* (free-identifier=? 
           (id0 identifier)
           (id1 identifier))
  (let ((b0 (env-lookup id0))
        (b1 (env-lookup id1)))
    (if b0
      (and b1
           ;; FIXME: check their import level here.
           (bound-identifier=? id0 id1))
      (and (not b1)
           (eq? (~ id0 'name)
                (~ id1 'name))))))

;;; binding object
;;;  type: macro | variable 
;;;  source-name: imported/exported name
;;;  source-library:
;;;  export-name: unique id for this binding 
;;;     (it will be global-name for identifier object)
;;;  mutable?: mutable check for R6RS spec.
;;;     (we cannot library-export mutable object)
;;;  code: macro procedure (macro only)
(define-composite binding
                  (type source-name source-library export-name mutable? code))

(define* (binding-transformer (b binding))
  (let-with b (code)
    code))

(define* (binding-macro? (b binding))
  (let-with b (type)
    (eq? type 'macro)))

(define* (binding-variable? (b binding))
  (let-with b (type)
    (eq? type 'variable)))

(define* (binding-name (b binding))
  (let-with b (export-name)
    export-name))

(define (syntax->binding syn type)
  (let-with syn (name color)
            (make binding
                  (type type)
                  (source-name name)
                  (export-name #f)
                  (mutable? #f)
                  (code #f))))

(define (make-library-binding type source-name source-library export-name code)
  (make binding
        (type type)
        (source-name source-name)
        (source-library source-library)
        (export-name export-name)
        (code code)))

;;; environment object
(define-composite environment (bindings parent))

(define (make-unit-env)
  (make environment
        (bindings '())
        (parent #f)))

(define* (make-extended-env (parent-env environment))
  (touch! (make-unit-env)
          (parent parent-env)))

(define* (env-append! (env environment) (new-binding binding))
  ;; Check dupes!
  (let-with env (bindings)
    (touch! env
      (bindings (append bindings new-binding)))))

(define (env-append/variables! env vars)
  (env-append! env (map (lambda (x) (syntax->binding x 'variable)) vars)))

(define (env-append/macros! env macros)
  (define (macrospec->binding m)
    (let ((a (car m))
          (s (cdr m)))
      (let ((b (syntax->binding a 'macro)))
        (touch! b
          (code s)))))
  (env-append! env (map macrospec->binding macros)))

(define (env-lookup/name+color name color env) ; => binding-object / #f
  (define (lookup-one bindings)
    (if (pair? bindings)
      (let ((binding (car bindings))
            (rest (cdr bindings)))
        (if (and (eq? name (~ binding 'source-name))
                 (color=? color (~ binding 'source-color)))
          binding
          (lookup-one rest)))
      #f))
  (let ((b (~ env 'bindings)))
    (or (lookup-one b)
        (let ((next (~ env 'parent)))
          (if next
            (env-lookup/name+color name color next)
            #f)))))

(define* (env-lookup (id identifier)) ; => binding-object / #f
  (let-with id (name color env)
    (env-lookup/name+color name color env)))

(define (make-new-template) ;; => identifier
  (make identifier
        (name #f)
        (phases '())
        (color (new-color))
        (env (make-unit-env))
        (debug #f)))

;;; utils

;; sexp-map is borrowed from Andre van Tonder's implementation

(define (sexp-map f s)
  (cond ((null? s) '())
        ((pair? s)
         (cons (sexp-map f (car s))
               (sexp-map f (cdr s))))
        ((vector? s)
         (apply vector (sexp-map f (vector->list s))))
        (else (f s))))

;;; rnrs

(define (identifier? id) (is-a? id identifier))

(define* (datum->syntax (tmpl identifier) datum)
  (define (newtmpl tmpl)
    (let-with tmpl (env)
      (touch! (newid tmpl 'ignored)
        (env (make-extended-env env)))))

  (define (syn-map sig tmpl s)
    (cond ((null? s) '())
          ((pair? s)
           (if sig
             (let ((e (newtmpl tmpl)))
               (cons (syn-map #t e (car s))
                     (syn-map #f e (cdr s))))
             (cons (syn-map #t tmpl (car s))
                   (syn-map #f tmpl (cdr s)))))
          ((vector? s)
           (apply vector (syn-map sig tmpl (vector->list s))))
          ((symbol? s) (newid tmpl s))
          (else s)))

  (define (newid tmpl sym)
    (let-with tmpl (phases color env)
      (make identifier
            (name sym)
            (phases phases)
            (color color)
            (env env)
            (debug #f))))

  (syn-map #f tmpl datum)) 

(define (syntax->datum syn)
  (define (conv e)
    (cond
      ((symbol? e)
       (assertion-violation 'syntax->datum "symbol is not allowed" e))
      ((identifier? e)
       (~ e 'name))
      (else e)))
  (sexp-map conv syn))

)

