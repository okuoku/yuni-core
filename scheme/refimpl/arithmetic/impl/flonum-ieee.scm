; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; IEEE dependencies

(define fl-ieee-min-exponent/denormalized (r5rs->integer -1074)) ; this includes denormalized numbers
(define fl-ieee-min-exponent (r5rs->integer -1023))
(define fl-ieee-max-exponent (r5rs->integer 1023))
(define r5rs-ieee-mantissa-width 53)
(define fl-ieee-mantissa-width (r5rs->integer r5rs-ieee-mantissa-width))

(define (r5rs-sign x)			      
  (cond			
   ((core:negative? x) -1)
   ((core:positive? x) 0)
   ;; kludge
   ((char=? #\- (string-ref (core:number->string x) 0))
    -1)
   (else 0)))

(define (flsign x)
  (r5rs->integer (r5rs-sign (flonum->r5rs x))))

; This is a kludge to work around a bug in the Scheme 48
; implementation of ->exact on denormalized numbers.

(define (r5rs-abs x)
  (if (core:< x 0.0)
      (core:- x)
      x))

; from Larceny:

(define (r5rs-significand x)
  (if (core:= 0.0 x)
      0
      (let loop ((x (r5rs-abs x)))
	(cond ((and (core:<= .5 x) (core:< x 1.0))
	       (core:inexact->exact
                (core:* x (core:expt 2.0 r5rs-ieee-mantissa-width))))
	      ((core:< x .5) (loop  (core:* 2.0 x)))
	      ((core:<= 1.0 x) (loop (core:* .5 x)))))))

(define (flsignificand x)
  (r5rs->integer (r5rs-significand (flonum->r5rs x))))

(define (r5rs-exponent x)
  (if (core:= 0.0 x)
      0
      (let loop ((x (r5rs-abs x)) (k 0))
	(cond ((and (core:<= .5 x) (core:< x 1.0))
               (core:- k r5rs-ieee-mantissa-width))
	      ((core:< x .5)
               (loop (core:* 2.0 x) (core:- k 1)))
	      ((core:<= 1.0 x)
               (loop (core:* .5 x) (core:+ k 1)))))))

(define (flexponent x)
  (r5rs->integer (r5rs-exponent (flonum->r5rs x))))

