(library (yuni expander syntax-case transformer)
         (export 
           syntax-rules/case-transformer
           expand-template)
         (import (rnrs base)
                 (rnrs lists)
                 (rnrs control)
                 (yuni expander syntax-object))

;; This transformer is based on Alex Shinn's chibi-scheme init.scm

;; BSD-style license: http://synthcode.com/license.txt
;;
;; Copyright (c) 2009 Alex Shinn
;; All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; util
(define (any pred ls)                                                                       
    (if (pair? ls) (if (pred (car ls)) (car ls) (any pred (cdr ls))) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax-rules (modified for syntax-case implementation)

;; exported
(define (expand-template rename tmpl vars)
  (define (ellipse-escape? x) (and (pair? x) (free-identifier=? (rename '...) (car x))))
  (define (ellipse? x) (and (pair? x) (pair? (cdr x)) (free-identifier=? (rename '...) (cadr x))))
  (define (ellipse-tail x)
    (if (ellipse? x)
      (ellipse-tail (cdr x))
      (cdr x)))
  (define (ellipse-depth x)
    (if (ellipse? x)
      (+ 1 (ellipse-depth (cdr x)))
      0))
  (define (free-vars x vars dim)
    (let lp ((x x) (free '()))
      (cond
        ((identifier? x)
         (if (and (not (memq x free))
                  (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                        (else #f)))
           (cons x free)
           free))
        ((pair? x) (lp (car x) (lp (cdr x) free)))
        ((vector? x) (lp (vector->list x) free))
        (else free))))
  (let ((_rename (rename 'rename))
        (_quote (rename 'quote))
        (_append (rename 'append))
        (_apply (rename 'apply))
        (_map (rename 'map))
        (_lambda (rename 'lambda))
        (_cons (rename 'cons))
        (_list->vector (rename 'list->vector)))
  (let lp ((t tmpl) (dim 0))
    (cond
      ((identifier? t)
       (cond
         ((any (lambda (v) (free-identifier=? t (car v))) vars)
          => (lambda (cell)
               (if (<= (cdr cell) dim)
                 t
                 (error "too few ...'s"))))
         (else
           (list _rename (list _quote t)))))
      ((pair? t)
       (cond
         ((ellipse-escape? t)
          (if (pair? (cdr t))
            (if (pair? (cddr t)) (cddr t) (cadr t))
            (cdr t)))
         ((ellipse? t)
          (let* ((depth (ellipse-depth t))
                 (ell-dim (+ dim depth))
                 (ell-vars (free-vars (car t) vars ell-dim)))
            (if (null? ell-vars)
              (error "too many ...'s")
              (let* ((once (lp (car t) ell-dim))
                     (nest (if (and (null? (cdr ell-vars))
                                    (identifier? once)
                                    (eq? once (car vars)))
                             once ;; shortcut
                             (cons _map
                                   (cons (list _lambda ell-vars once)
                                         ell-vars))))
                     (many (do ((d depth (- d 1))
                                (many nest
                                      (list _apply _append many)))
                             ((= d 1) many))))
                (if (null? (ellipse-tail t))
                  many ;; shortcut
                  (list _append many (lp (ellipse-tail t) dim)))))))
         (else (list _cons (lp (car t) dim) (lp (cdr t) dim)))))
      ((vector? t) (list _list->vector (lp (vector->list t) dim)))
      ((null? t) (list _quote '()))
      (else t)))))

(define syntax-rules/case-transformer
   (lambda (expand-in-place? expr rename)
     (let ((count 0)
           ;; add
           (compare free-identifier=?) ;; FIXME: replace it..
           (_let-syntax (rename 'let-syntax))
           (_expand-template (rename 'expand-template))
           (_make-syntax (rename 'make-syntax))
           (_syntax (rename 'syntax))

           ;; orig
           (_er-macro-transformer (rename 'er-macro-transformer))
           (_lambda (rename 'lambda))      (_let (rename 'let))
           (_begin (rename 'begin))        (_if (rename 'if))
           (_and (rename 'and))            (_or (rename 'or))
           (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
           (_car (rename 'car))            (_cdr (rename 'cdr))
           (_cons (rename 'cons))          (_pair? (rename 'pair?))
           (_null? (rename 'null?))        (_expr (rename 'expr))
           (_rename (rename 'rename))      (_compare (rename 'compare))
           (_quote (rename 'syntax-quote)) (_apply (rename 'apply))
           (_append (rename 'append))      (_map (rename 'map))
           (_vector? (rename 'vector?))    (_list? (rename 'list?))
           (_lp (rename 'lp))              (_reverse (rename 'reverse))
           (_len (rename 'len))             (_length (rename 'length))
           (_- (rename '-))   (_>= (rename '>=))   (_error (rename 'error))
           (_ls (rename 'ls)) (_res (rename 'res)) (_i (rename 'i))
           (_vector->list (rename 'vector->list))
           (_list->vector (rename 'list->vector)))
       (define ellipse (rename '...))
       (define lits (cadr expr))
       (define forms (cddr expr))
       (define (next-v)
         (set! count (+ count 1))
         (rename (string->symbol (string-append "v." (number->string count)))))
       ;; add: emit-template. 
       (define (emit-template tmpl vars)
         (list
           _let-syntax (list (list _syntax (list _make-syntax vars)))
           tmpl))
       (define (expand-pattern pat tmpl)
         (let lp ((p (cdr pat))
                  (x (list _cdr _expr))
                  (dim 0)
                  (vars '())
                  (k (if expand-in-place?
                       (lambda (vars) (or (expand-template tmpl vars)
                                          (list _begin #f)))
                       (lambda (vars) (emit-template tmpl vars)))))
           (let ((v (next-v)))
             (list
              _let (list (list v x))
              (cond
               ((identifier? p)
                (if (any (lambda (l) (compare p l)) lits)
                    (list _and (list _compare v (list _quote p)) (k vars))
                    (list _let (list (list p v)) (k (cons (cons p dim) vars)))))
               ((ellipse? p)
                (cond
                 ((not (null? (cddr p)))
                  (cond
                   ((not (list? (cddr p)))
                    (error "dotted ellipse" p))
                   ((any (lambda (x) (and (identifier? x) (compare x ellipse)))
                         (cddr p))
                    (error "multiple ellipses" p))
                   (else
                    (let ((len (length (cdr (cdr p)))))
                      `(,_let ((,_len (,_length ,v)))
                         (,_and (,_>= ,_len ,len)
                                (,_let ,_lp ((,_ls ,v)
                                             (,_i (,_- ,_len ,len))
                                             (,_res (,_quote ())))
                                  (,_if (,_>= 0 ,_i)
                                      ,(lp `(,@(cdr (cdr p)) ,(car p) ,(car (cdr p)))
                                           `(,_append ,_ls (,_reverse ,_res))
                                           dim
                                           vars
                                           k)
                                      (,_lp (,_cdr ,_ls)
                                            (,_- ,_i 1)
                                            (,_cons (,_car ,_ls) ,_res))))))))))
                 ((identifier? (car p))
                  (list _and (list _list? v)
                        (list _let (list (list (car p) v))
                              (k (cons (cons (car p) (+ 1 dim)) vars)))))
                 (else
                  (let* ((w (next-v))
                         (new-vars (all-vars (car p) (+ dim 1)))
                         (ls-vars (map (lambda (x)
                                         (rename
                                          (string->symbol
                                           (string-append
                                            (symbol->string
                                             (syntax->datum (car x)))
                                            "-ls"))))
                                       new-vars))
                         (once
                          (lp (car p) (list _car w) (+ dim 1) '()
                              (lambda (_)
                                (cons
                                 _lp
                                 (cons
                                  (list _cdr w)
                                  (map (lambda (x l)
                                         (list _cons (car x) l))
                                       new-vars
                                       ls-vars)))))))
                    (list
                     _let
                     _lp (cons (list w v)
                               (map (lambda (x) (list x '())) ls-vars))
                     (list _if (list _null? w)
                           (list _let (map (lambda (x l)
                                             (list (car x) (list _reverse l)))
                                           new-vars
                                           ls-vars)
                                 (k (append new-vars vars)))
                           (list _and (list _pair? w) once)))))))
               ((pair? p)
                (list _and (list _pair? v)
                      (lp (car p)
                          (list _car v)
                          dim
                          vars
                          (lambda (vars)
                            (lp (cdr p) (list _cdr v) dim vars k)))))
               ((vector? p)
                (list _and
                      (list _vector? v)
                      (lp (vector->list p) (list _vector->list v) dim vars k)))
               ((null? p) (list _and (list _null? v) (k vars)))
               (else (list _and (list _equal? v p) (k vars))))))))
       (define (ellipse? x)
         (and (pair? x) (pair? (cdr x)) (compare ellipse (cadr x))))
       (define (all-vars x dim)
         (let lp ((x x) (dim dim) (vars '()))
           (cond ((identifier? x)
                  (if (any (lambda (lit) (compare x lit)) lits)
                      vars
                      (cons (cons x dim) vars)))
                 ((ellipse? x) (lp (car x) (+ dim 1) vars))
                 ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
                 ((vector? x) (lp (vector->list x) dim vars))
                 (else vars))))
       ;; expand-template was moved as global
       (list
        _er-macro-transformer
        (list _lambda (list _expr _rename _compare)
              (cons
               _or
               (append
                (map
                 (lambda (clause) (expand-pattern (car clause) (cadr clause)))
                 forms)
                (list (list _error "no expansion for"
                            (list (rename 'strip-syntactic-closures) _expr))))))))))

)
