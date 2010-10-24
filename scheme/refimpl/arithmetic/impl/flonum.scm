; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Flonums in terms of R5RS; assumes underlying IEEE-like representation


; SRFI 9
(define-record-type :flonum
  (really-make-flonum inexact)
  flonum?
  (inexact flonum-inexact))

; Scheme 48 extension; comment out if not available
(define-record-discloser :flonum
  (lambda (r)
    (list 'flonum (flonum-inexact r))))

(define (make-flonum n)
  (really-make-flonum (core:exact->inexact n)))

(define core->flonum make-flonum)

(define flonum->core flonum-inexact)

; for playing around
(define fl make-flonum)

(define (make-fl*fl->fl r5rs-op)
  (lambda (a b)
    (if (or (flnan? a)
	    (flnan? b))
	flnan
	(make-flonum (r5rs-op (flonum-inexact a) (flonum-inexact b))))))

(define fl+/2 (make-fl*fl->fl core:+))
(define (fl+ . args)
  (reduce (make-flonum 0.0) fl+/2 args))

(define fl-/2 (make-fl*fl->fl core:-))
(define (fl- arg0 . args)
  (reduce (make-flonum 0.0) fl-/2 (cons arg0 args)))

(define (make-fl->fl r5rs-op)
  (lambda (a)
    (if (flnan? a)
	flnan
	(make-flonum (r5rs-op (flonum-inexact a))))))

(define fl*/2 (make-fl*fl->fl core:*))
(define (fl* . args)
  (reduce (make-flonum 1.0) fl*/2 args))

(define (/* a b)
  (cond
   ((core:= b r5rs-inf+)
    (cond
     ((or (core:= a r5rs-inf+) (core:= a r5rs-inf-))
      r5rs-nan)
     ((core:< a 0.0)
      -0.0)
     (else
      0.0)))
   ((core:= b r5rs-inf-)
    (cond
     ((or (core:= a r5rs-inf+) (core:= a r5rs-inf-))
      r5rs-nan)
     ((core:< a 0.0)
      0.0)
     (else
      -0.0)))
   ((not (core:= b 0.0)) (core:/ a b))
   ((core:= a 0.0) r5rs-nan)
   ((core:> a 0.0) r5rs-inf+)
   (else r5rs-inf-)))

(define fl//2 (make-fl*fl->fl /*))
(define (fl/ arg0 . args)
  (reduce (make-flonum 1.0) fl//2 (cons arg0 args)))

(define (make-fl*fl->val r5rs-op)
  (lambda (a b)
    (r5rs-op (flonum-inexact a) (flonum-inexact b))))

(define fl=? (make-transitive-pred (make-fl*fl->val core:=)))
(define fl>=? (make-transitive-pred (make-fl*fl->val core:>=)))
(define fl<=? (make-transitive-pred (make-fl*fl->val core:<=)))
(define fl>? (make-transitive-pred (make-fl*fl->val core:>)))
(define fl<? (make-transitive-pred (make-fl*fl->val core:<)))

(define (make-fl->val r5rs-op)
  (lambda (a)
    (r5rs-op (flonum-inexact a))))

(define (flzero? x)
  (fl=? x (core->flonum 0.0)))
(define (flpositive? x)
  (fl>? x (core->flonum 0.0)))
(define (flnegative? x)
  (fl<? x (core->flonum 0.0)))

(define flmin (make-min/max fl<?))
(define flmax (make-min/max fl>?))

(define (flabs x)
  (if (flnegative? x)
      (fl- x)
      x))

(define flexp (make-fl->fl core:exp))

(define (log1* z)
  (cond
   ((core:= r5rs-inf+ z)
    r5rs-inf+)
   ((core:= r5rs-inf- z)
    r5rs-nan)
   ((not (core:= z z))
    r5rs-nan)
   ((core:= 0.0 z)
    r5rs-inf-)
   (else
    (core:log z))))

(define fllog1 (make-fl->fl log1*))
(define flsin (make-fl->fl core:sin))
(define flcos (make-fl->fl core:cos))
(define fltan (make-fl->fl core:tan))
(define flasin (make-fl->fl core:asin))
(define flacos (make-fl->fl core:acos))
(define flatan1 (make-fl->fl core:atan))
(define flatan2 (make-fl*fl->fl core:atan))

(define (fllog z . extra)
  (if (null? extra)
      (fllog1 z)
      (fl/ (fllog1 z)
	   (fllog1 (car extra)))))

(define (flatan x . extra)
  (if (null? extra)
      (flatan1 x)
      (flatan2 x (car extra))))

(define (sqrt* z)
  (cond
   ((core:= r5rs-inf+ z)
    r5rs-inf+)
   ((core:= r5rs-inf- z)
    r5rs-nan)
   ((core:< z 0.0)
    r5rs-nan)
   ((not (core:= z z))
    r5rs-nan)
   (else
    (core:sqrt z))))

(define flsqrt (make-fl->fl sqrt*))

(define (expt* a b)
  (cond
   ((core:> a 0.0)
    (cond ((core:> b 0.0)
           (core:expt a b))
          ((core:= b 0.0)
           a)
          (else
           (core:/ 1.0 (expt* a (core:- b))))))
   ((core:= a 0.0)
    (cond ((core:> b 0.0)
           0.0)
          ((core:= b 0.0)
           1.0)
          (else
           r5rs-nan)))
   (else
    (cond ((core:= b 0.0)
           1.0)
          (else
           r5rs-nan)))))

(define flexpt (make-fl*fl->fl core:expt))

(define flfloor (make-fl->fl core:floor))
(define flceiling (make-fl->fl core:ceiling))
(define fltruncate (make-fl->fl core:truncate))
(define flround (make-fl->fl core:round))

(define (fixnum->flonum fx)
  (make-flonum (fixnum->core fx)))

(define (flonum->fixnum f)
  (cond
   ((fl<? f (fixnum->flonum (least-fixnum)))
    (least-fixnum))
   ((fl>? f (fixnum->flonum (greatest-fixnum)))
    (greatest-fixnum))
   (else
    (core->fixnum (core:inexact->exact (core:round (flonum-inexact f)))))))

; FIXME: Are these still used?

(define flquotient (make-fl*fl->fl core:quotient))
(define flremainder (make-fl*fl->fl core:remainder))
(define (flquotient+remainder a b)
  (values (flquotient a b)
	  (flremainder a b)))
(define flmodulo (make-fl*fl->fl core:modulo))

(define (fldiv+mod x y)
  (if (flzero? y)
      (values flnan flnan)
      (let* ((div (flfloor (fl/ x y)))
             (mod (fl- x (fl* div y))))
        (values div mod))))

(define (fldiv x y)
  (call-with-values
   (lambda () (fldiv+mod x y))
   (lambda (d m)
     d)))

(define (flmod x y)
  (call-with-values
   (lambda () (fldiv+mod x y))
   (lambda (d m)
     m)))

(define flodd?
  (make-fl->val
   (lambda (x)
     (if (or (core:= x r5rs-inf+)
             (core:= x r5rs-inf-)
             (not (core:= x x)))
         #f
         (core:odd? x)))))

(define fleven?
  (make-fl->val
   (lambda (x)
     (if (or (core:= x r5rs-inf+)
             (core:= x r5rs-inf-)
             (not (core:= x x)))
         #f
         (core:even? x)))))

(define flinteger?
  (make-fl->val
   (lambda (x)
     (if (or (core:= x r5rs-inf+)
             (core:= x r5rs-inf-)
             (not (core:= x x)))
         #f
         (core:integer? x)))))

(define r5rs-inf+ 1e1025)
(define r5rs-inf- -1e1025)
(define r5rs-nan (core:- r5rs-inf+ r5rs-inf+))

(define flinf+ (make-flonum r5rs-inf+))
(define flinf- (make-flonum r5rs-inf-))

(define flnan (make-flonum r5rs-nan))

(define flnan? (make-fl->val (lambda (x) (not (core:= x x)))))

(define (infinite?* x)
  (or (core:= x r5rs-inf+)
      (core:= x r5rs-inf-)))

(define flinfinite? (make-fl->val infinite?*))

(define (finite?* x)
  (and (core:= x x)
       (not (infinite?* x))))

(define flfinite? (make-fl->val finite?*))
