(library (yuni expander core)
         (export
           ;; core expanders library
           core-syntax-library
           core-procedures-library

           ;; expander
           expand-sequence)
         (import
           (yuni impl base)
           (yuni expander syntax-object)
           (yuni core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CORE EXPANDERS : (yuni scheme primitives)
;;;
;;;  QUOTE 
;;;  CORE-LAMBDA, CORE-LAMBDA*
;;;  CORE-DEBUG
;;;  CORE-LET, CORE-LETREC
;;;  LET-SYNTAX, LETREC-SYNTAX
;;;  ER-MACRO-TRANSFORMER
;;;  CORE-IF, SET!
;;;
;;; CORE SEQUENCE EXPANDERS :
;;;
;;;  CORE-BEGIN, CORE-DEFINE, DEFINE-SYNTAX
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (invalid-form name exp)
  ;;; STUB STUB STUB!!
  (display "invalid-form")(newline)
  (display name)(newline)
  (write (syntax->datum exp))(newline)
  (car #f))

(define (unbound-variable name exp)
  ;;; STUB STUB STUB!!
  (display "unbound-variable")(newline)
  (display name)(newline)
  (write (syntax->datum exp))(newline)
  (car #f))

(define (extend/vars exp vars)
  (define* (env-ret (id identifier))
    (let-with id (env)
      (env-append/variables! env vars)))
  (env-ret (car exp)))

(define (extend/macros exp macros)
  (define* (env-ret (id identifier))
    (let-with id (env)
      (env-append/macros! env macros)))
  (env-ret (car exp)))

(define (expand-core-debug k exp)
  exp)
  
;; (QUOTE x) => x
(define (expand-quote k exp)
  (unless (and (pair? exp) (pair? (cdr exp)))
    (invalid-form 'quote exp))
  (list 'quote (syntax->datum (cadr exp))))

;; (CORE-LAMBDA* NAME X BODY)
(define (expand-core-lambda* k exp)
  (unless (and (list? exp) (= 4 (length exp)))
    (invalid-form 'core-lambda* exp))
  (let ((name (cadr exp))
        (x (caddr exp))
        (body (cadddr exp)))
    (extend/vars exp (list x))
    (list 'core-lambda* name (k x) (k body))))

;; (CORE-LAMBDA NAME (X Y ...) BODY)
(define (expand-core-lambda k exp)
  (unless (and (list? exp) (= 4 (length exp)))
    (invalid-form 'core-lambda exp))
  (let ((name (cadr exp))
        (x (caddr exp))
        (body (cadddr exp)))
    (extend/vars exp x)
    (list 'core-lambda name (map k x) (k body))))

(define (expand-core-let/rec-base proc rec? macro? sym k exp)
  (define (conv c)
    (unless (and (list? c) (= 2 (length c)))
      (invalid-form sym exp))
    (cons (car c) 
          (let ((code (cadr c)))
            (if (not rec?) (k code) code))))
  (unless (and (list? exp) (= 3 (length exp)))
    (invalid-form sym exp))
  (let ((clauses (cadr exp))
        (body (caddr exp)))
    (unless (list? clauses)
      (invalid-form sym clauses))
    (let ((vars+code (map conv clauses)))
      ;; extend
      (proc exp 
            (cond
              ((and macro? rec?)
               (map (lambda (c) (cons (car c) 'INVALID)) vars+code))
              (macro?  vars+code)
              (else (map car vars+code))))
      (if macro?  
        (if rec?
            (proc exp (map (lambda (c) (cons (car c) (k (cdr c)))) vars+code))
            (k body))
        (let ((vars (map car vars+code))
              (code (if rec? 
                      (map (lambda (e) (k (cdr e))) vars+code)
                      (map cdr vars+code))))
          (list sym (map (lambda (vars code) (list vars code))
                         vars code)))))))

;; (CORE-LETREC ((X Y) ...) BODY)
(define (expand-core-letrec k exp)
  (expand-core-let/rec-base extend/vars #t #f 'core-letrec k exp))

;; (CORE-LET ((X Y) ...) BODY)
(define (expand-core-let k exp)
  (expand-core-let/rec-base extend/vars #f #f 'core-let k exp))

;; (CORE-LET-SYNTAX ((X Y) ...) BODY)
(define (expand-let-syntax k exp)
  (expand-core-let/rec-base extend/macros #f #t 'core-let-syntax k exp))

;; (CORE-LETREC-SYNTAX ((X Y) ...) BODY)
(define (expand-letrec-syntax k exp)
  (expand-core-let/rec-base extend/macros #t #t 'core-letrec-syntax k exp))

;; (CORE-IF Q THEN-PART ELSE-PART)
(define (expand-core-if k exp)
  (unless (and (list? exp) (= 4 (length exp)))
    (invalid-form 'core-if exp))
  (let ((q (cadr exp))
        (then-part (caddr exp))
        (else-part (cadddr exp)))
    (list 'core-if (k q) (k then-part) (k else-part))))

;; (SET! VAL VALUE)
(define (expand-set! k exp)
  (unless (and (list? exp) (= 3 (length exp)))
    (invalid-form 'set! exp))
  (let ((val (cadr exp))
        (value (caddr exp)))
    ;; FIXME: check variable-transformer here..
    ;; FIXME: mark as mutable
    (list 'set! (k val) (k value))))

;; (er-macro-transformer (^[expr rename compare] ...))
(define (expand-er-macro-transformer k exp)
  (unless (and (list? exp) (= 2 (length exp)))
    (invalid-form 'er-macro-transformer exp))
  (let ((tmpl (car exp))
        (obj (k (cadr exp))))
    (cons 'er-macro-transformer (cons tmpl obj))))

;;; 
;;; SEQUENCE EXPANDER
;;;
;;; Sequence expander is special expander for "program sequence"
;;; e.g. (begin ...) sequence.
;;;
;;; Sequence expander is the only way to brake parensis boundary of
;;; name binding.
;;;
;; (CORE-DEFINE NAME BODY)
(define (expand-core-define k seq exp)
  (unless (and (list? exp) (= 3 (length exp)))
    (invalid-form 'core-define exp))
  (let ((name (cadr exp))
        (body (caddr exp)))
    (extend/vars seq (list name))
    (list 'core-define (k name) (k body))))

;; (DEFINE-SYNTAX NAME BODY)
(define (expand-define-syntax k seq exp)
  (unless (and (list? exp) (= 3 (length exp)))
    (invalid-form 'core-define exp))
  (let ((name (cadr exp))
        (body (caddr exp)))
    (extend/macros seq (list name))
    (list 'define-syntax (k name) (k body))))

(define (expand-core-begin k seq exp)
  (expand-sequence k seq (cdr exp)))

(define (expand-sequence seq)
  (define (expand-form frm)
    (cond
      ((pair? frm)
       (let ((op (car frm))
             (rest (cdr frm)))
         (cond
           ((identifier? op)
            (let ((b (env-lookup op)))
              (if b
                (cond
                  ((binding-macro? b)
                   (let ((t (binding-transformer b)))
                     (cond
                       ((not t) ;; deferred macro
                        (car #f) ;; FIXMEFIXME
                        )
                       ((pair? t)
                        (let ((type (car t))
                              (arg (cdr t)))
                          (case (car t)
                            ((er-macro-transformer)
                             ;; invoke er-macro-transformer
                             (let ((tmpl (car arg))
                                   (code (cdr arg)))
                               (code
                                 frm
                                 (lambda (symbol) (datum->syntax tmpl symbol)) ;; rename
                                 free-identifier=?)))
                            (else
                              (invalid-form 'expand-sequence "FATAL ERROR")))))
                       ((or (eq? t expand-core-begin)
                            (eq? t expand-core-define)
                            (eq? t expand-define-syntax)) ;; sequence expander?
                        (t expand-form seq frm))
                       (else ;; normal transformer
                         (t expand-form frm)))))
                   (else ;; variable
                     (cons
                       (binding-name b)
                       (map expand-form rest))))
                (unbound-variable 'expand-sequence op))))
           (else
             (invalid-form 'expand-sequence frm)))))
      (else
        (cond
          ((identifier? frm)
           (let ((b (env-lookup frm)))
             (if b
               (binding-name b)
               (unbound-variable 'expand-sequence frm))))
          (else
            ;; FIXME: check if symbol?
            frm)))))
  (define (itr cur rest)
    (if (pair? rest)
      (itr (cons (expand-form (car rest)) cur) (cdr rest))
      (reverse cur)))
  ;; do not use map here.. expand-form will cause some side-effects
  ;; and must be applied in-order.
  (itr '() seq))
  

(define core-syntax-library ;; == *binding
  (let ((corelib 
          (list
            (cons expand-core-begin 'core-begin)
            (cons expand-define-syntax 'define-syntax)
            (cons expand-core-define 'core-define)
            (cons expand-core-debug 'core-debug)
            (cons expand-set! 'set!)
            (cons expand-core-if 'core-if)
            (cons expand-letrec-syntax 'letrec-syntax)
            (cons expand-let-syntax 'let-syntax)
            (cons expand-core-let 'core-let)
            (cons expand-core-lambda 'core-lambda)
            (cons expand-core-lambda* 'core-lambda*)
            (cons expand-er-macro-transformer 'er-macro-transformer)
            (cons expand-quote 'quote))))
    (define (conv p)
      (let ((code (car p))
            (name (cdr p)))
        (make-library-binding 'macro name '(*internal* yuni core-expander) #f code)))
    (map conv corelib)))

;; FIXME: stub
(define core-procedures-library ;; == *binding
  (let ((corelib '(car cdr)))
    (define (conv name)
      (make-library-binding 'macro name '(*internal* yuni core-procedures) name #f)))
  (map conv corelib))

)
