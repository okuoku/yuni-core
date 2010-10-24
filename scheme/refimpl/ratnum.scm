; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; from Scheme 48

; Rational arithmetic

(define-record-type :ratnum
  (make-unreduced-ratnum num den)
  ratnum?
  ;; these are integers
  (num ratnum-numerator)
  (den ratnum-denominator))

(define-record-discloser :ratnum
  (lambda (r)
    (list 'ratnum
	  (ratnum-numerator r)
	  (ratnum-denominator r))))

(define (r5rs->ratnum r)
  (integer/ (r5rs->integer (r5rs:numerator r))
	    (r5rs->integer (r5rs:denominator r))))

(define (integer/ m n)
  (cond ((integer<? n (r5rs->integer 0))
	 (integer/ (integer-negate m) (integer-negate n)))
	((integer=? n (r5rs->integer 0))
	 (error "rational division by zero" m))
	(else
	 (let ((g (integer-gcd m n)))
	   (let ((m (integer-quotient m g))
		 (n (integer-quotient n g)))
	     (if (integer=? n (r5rs->integer 1))
		 m
		 (make-unreduced-ratnum m n)))))))

(define (ratnum->rational r)
  (integer/ (ratnum-numerator r) (ratnum-denominator r)))

; a/b * c/d = a*c / b*d

(define (ratnum* p q)
  (integer/
   (integer* (ratnum-numerator p) (ratnum-numerator q))
   (integer* (ratnum-denominator p) (ratnum-denominator q))))

; a/b / c/d = a*d / b*c

(define (ratnum/ p q)
  (integer/
   (integer* (ratnum-numerator p) (ratnum-denominator q))
   (integer* (ratnum-denominator p) (ratnum-numerator q))))

; a/b + c/d = (a*d + b*c)/(b*d)

(define (ratnum+ p q)
  (let ((b (ratnum-denominator p))
	(d (ratnum-denominator q)))
    (integer/
     (integer+ (integer* (ratnum-numerator p) d)
	       (integer* b (ratnum-numerator q)))
     (integer* b d))))

; a/b - c/d = (a*d - b*c)/(b*d)

(define (ratnum- p q)
  (let ((b (ratnum-denominator p))
	(d (ratnum-denominator q)))
    (integer/
     (integer- (integer* (ratnum-numerator p) d)
	       (integer* b (ratnum-numerator q)))
     (integer* b d))))

; a/b < c/d  when  a*d < b*c

(define (ratnum<? p q)
  (integer<? (integer* (ratnum-numerator p) (ratnum-denominator q))
	     (integer* (ratnum-denominator p) (ratnum-numerator q))))

(define (ratnum<=? p q)
  (not (ratnum<? q p)))

(define (ratnum>=? p q)
  (not (ratnum<? p q)))

(define (ratnum>? p q)
  (ratnum<? q p))

(define (ratnum-positive? r)
  (integer-positive? (ratnum-numerator r)))
(define (ratnum-negative? r)
  (integer-negative? (ratnum-numerator r)))

(define (ratnum-abs r)
  (integer/ (integer-abs (ratnum-numerator r))
	    (ratnum-denominator r)))

(define (ratnum-min m n)
  (if (ratnum<=? m n)
      (ratnum->rational m)
      (ratnum->rational n)))

(define (ratnum-max m n)
  (if (ratnum>=? m n)
      (ratnum->rational m)
      (ratnum->rational n)))

; a/b = c/d  when a = b and c = d  (always lowest terms)

(define (ratnum=? p q)
  (and (integer=? (ratnum-numerator p) (ratnum-numerator q))
       (integer=? (ratnum-denominator p) (ratnum-denominator q))))

; (rational-truncate p) = integer of largest magnitude <= (abs p)

(define (ratnum-truncate p)
  (integer-quotient (ratnum-numerator p) (ratnum-denominator p)))

; (floor p) = greatest integer <= p

(define (ratnum-floor p)
  (let* ((n (ratnum-numerator p))
	 (q (integer-quotient n (ratnum-denominator p))))
    (if (integer<? (r5rs->integer 0) n)
	q
	(integer- q (r5rs->integer 1)))))

(define (ratnum->string r radix)
  (string-append
   (integer->string (ratnum-numerator r) radix)
   "/"
   (integer->string (ratnum-denominator r) radix)))
