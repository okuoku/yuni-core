; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Package definitions for running the implementation in Scheme 48

(define-interface customization-interface
  (export *width*
          *fixnums-are-records*
          *flonums-are-records*))

(define-structure customization customization-interface
  (open scheme)
  (files custom))

(define scheme-sans-arithmetic
  (modify scheme
	  (hide eqv?
                number? complex? real? rational? integer?
		exact? inexact?
		zero? positive? negative? odd? even?
		max min
		+ * - /
		= < <= >= >
		abs
		quotient remainder modulo
		gcd lcm
		numerator denominator
		floor ceiling truncate round
		rationalize
		exp log sin cos tan asin acos atan
		sqrt expt
		make-rectangular make-polar
		real-part imag-part
		magnitude angle
		exact->inexact inexact->exact
		number->string string->number)))

(define-interface r5rs-arithmetic-interface
  (export r5rs:eqv?
          r5rs:number? r5rs:complex? r5rs:real? r5rs:rational? r5rs:integer?
	  r5rs:exact? r5rs:inexact?
	  r5rs:zero? r5rs:positive? r5rs:negative? r5rs:odd? r5rs:even?
	  r5rs:max r5rs:min
	  r5rs:+ r5rs:* r5rs:- r5rs:/
	  r5rs:= r5rs:< r5rs:<= r5rs:>= r5rs:>
	  r5rs:abs
	  r5rs:quotient r5rs:remainder r5rs:modulo
	  r5rs:gcd r5rs:lcm
	  r5rs:numerator r5rs:denominator
	  r5rs:floor r5rs:ceiling r5rs:truncate r5rs:round
	  r5rs:rationalize
	  r5rs:exp r5rs:log r5rs:sin r5rs:cos r5rs:tan
          r5rs:asin r5rs:acos r5rs:atan
	  r5rs:sqrt r5rs:expt
	  r5rs:make-rectangular r5rs:make-polar
	  r5rs:real-part r5rs:imag-part
	  r5rs:magnitude r5rs:angle
	  r5rs:exact->inexact r5rs:inexact->exact
	  r5rs:number->string r5rs:string->number))

(define-structure r5rs-arithmetic r5rs-arithmetic-interface
  (open scheme)
  (files r5rs-arithmetic))

(define-interface nary-interface
  (export make-transitive-pred
	  reduce
	  make-min/max))

(define-structure nary nary-interface
  (open scheme-sans-arithmetic)
  (files nary))

(define-interface fixnums-interface
  (export make-fixnum ; FIXME: should be used only by fixnum tests
          fixnum-rep  ; FIXME: should be used only by fixnum tests
          fx          ; temporary
	  fixnum?
	  fixnum-width least-fixnum greatest-fixnum
	  fixnum=? fixnum>=? fixnum<=? fixnum>? fixnum<?
	  fixnum-zero? fixnum-positive? fixnum-negative?
          fixnum-even? fixnum-odd?
	  fixnum-max fixnum-min
	  fixnum+ fixnum- fixnum*
          ; FIXME: these next two lines should go away.
	  fixnum-quotient fixnum-remainder fixnum-modulo
	  fixnum-quotient+remainder
	  fixnum-div+mod fixnum-div fixnum-mod
	  fixnum-div0+mod0 fixnum-div0 fixnum-mod0
	  fixnum+/carry fixnum-/carry fixnum*/carry

	  fixnum-not
	  fixnum-and fixnum-ior fixnum-xor
          fixnum-if fixnum-bit-count fixnum-length
          fixnum-first-bit-set fixnum-bit-set?
          fixnum-copy-bit fixnum-bit-field fixnum-copy-bit-field
          fixnum-arithmetic-shift
	  fixnum-arithmetic-shift-left
          fixnum-arithmetic-shift-right
	  fixnum-logical-shift-left fixnum-logical-shift-right
          fixnum-rotate-bit-field fixnum-reverse-bit-field

          fx=? fx>? fx<? fx>=? fx<=?
          fxzero? fxpositive? fxnegative? fxodd? fxeven?
          fxmax fxmin
          fx+ fx- fx*
          fxdiv+mod fxdiv fxmod fxdiv0+mod0 fxdiv0 fxmod0

          fxnot fxand fxior fxxor
          fxif fxbit-count fxlength
          fxfirst-bit-set fxbit-set?
          fxcopy-bit fxbit-field fxcopy-bit-field
          fxarithmetic-shift
	  fxarithmetic-shift-left fxarithmetic-shift-right
          fxrotate-bit-field fxreverse-bit-field
))

(define-structures ((fixnums fixnums-interface)
		    (fixnums-r5rs (export r5rs->fixnum
					  fixnum->r5rs)))
  (open customization
        scheme-sans-arithmetic
        r5rs-arithmetic
	bitwise
	srfi-9 ; define-record-type
	srfi-23 ; error
	(subset define-record-types (define-record-discloser))
	nary)
  (files fixnum))

(define-interface flonums-interface
  (export flonum?
	  fl+ fl- fl* fl/ fl=? fl>=? fl<=? fl>? fl<?
	  flzero? flpositive? flnegative?
	  flmin flmax flabs
	  flexp fllog
	  flsin flcos fltan flasin flacos flatan
	  flsqrt flexpt
	  flfloor flceiling fltruncate flround
	  flinteger? 
	  flquotient flremainder flquotient+remainder
	  fldiv+mod fldiv flmod
	  flodd? fleven?
	  flonum->fixnum fixnum->flonum
	  flinf+ flinf- flnan
	  flinfinite? flfinite? flnan?))

(define-structures ((flonums flonums-interface)
		    (flonums-r5rs (export r5rs->flonum flonum->r5rs)))
  (open scheme-sans-arithmetic
        r5rs-arithmetic
	srfi-9 ; define-record-type
	(subset define-record-types (define-record-discloser))
	fixnums fixnums-r5rs
	nary)
  (files flonum))

(define-interface bignums-interface
  (export bignum->integer
	  fixnum->bignum
	  bignum?
	  bignum+ bignum- bignum*
	  bignum-divide
	  bignum-quotient bignum-remainder bignum-quotient+remainder
	  bignum-negate
	  bignum=?
	  bignum<? bignum<=? bignum>=? bignum>?
	  bignum-positive? bignum-negative?
	  bignum-abs
	  bignum-min bignum-max
	  bignum-even? bignum-odd?
	  bignum-zero?
	  bignum->string

	  bignum-not
	  bignum-ior bignum-xor bignum-and
	  bignum-arithmetic-shift-left bignum-arithmetic-shift-right
          bignum-bit-count bignum-length bignum-first-bit-set))

(define-structures ((bignums bignums-interface)
		    (bignums-r5rs (export bignum->r5rs r5rs->bignum)))
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose >= = - remainder quotient * +))
  	srfi-9 ; define-record-type
	(subset define-record-types (define-record-discloser))
	srfi-23 ; ERROR
	fixnums
	fixnums-r5rs)
  (files bignum
	 bigbit))

(define-interface integers-interface
  (export exact-integer?
	  integer+
	  integer-
	  integer*
	  integer-quotient integer-remainder integer-quotient+remainder
	  integer-negate
	  integer=?
	  integer<?

	  integer-gcd integer-lcm
	  integer-zero? integer-expt integer-even? integer-odd?
	  integer>? integer>=? integer<=?
	  integer-positive? integer-negative?
	  integer-min integer-abs integer-max
	  integer->string

	  integer->bignum

	  integer-bitwise-not
	  integer-bitwise-ior integer-bitwise-xor integer-bitwise-and
	  integer-arithmetic-shift-left))

(define-structures ((integers integers-interface)
		    (integers-r5rs (export integer->r5rs r5rs->integer )))
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose >= <=))
	fixnums fixnums-r5rs
	bignums bignums-r5rs)
  (files integer))

(define-interface flonums-ieee-interface
  (export flsign flsignificand flexponent
	  fl-ieee-min-exponent fl-ieee-min-exponent/denormalized
	  fl-ieee-max-exponent
	  fl-ieee-mantissa-width))

(define-structure flonums-ieee flonums-ieee-interface
  (open scheme-sans-arithmetic
        r5rs-arithmetic
	flonums-r5rs
	integers-r5rs)
  (files flonum-ieee))

(define-interface ratnums-interface
  (export make-unreduced-ratnum
	  integer/
	  ratnum?
	  ratnum-numerator ratnum-denominator
	  ratnum* ratnum/ ratnum+ ratnum-
	  ratnum<? ratnum<=? ratnum>=? ratnum>?
	  ratnum=?
	  ratnum-positive? ratnum-negative?
	  ratnum-abs
	  ratnum-min ratnum-max
	  ratnum-truncate ratnum-floor
	  ratnum->string))

(define-structures ((ratnums ratnums-interface)
		    (ratnums-r5rs (export r5rs->ratnum)))
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose numerator denominator))
	srfi-9				; define-record-types
	(subset define-record-types (define-record-discloser))
	srfi-23				; ERROR
	integers integers-r5rs
	)
  (files ratnum))

(define-interface rationals-interface
  (export exact-rational?
	  rational-numerator rational-denominator
	  rational* rational/ rational+ rational-
	  rational<? rational=?
	  rational-positive? rational-negative?
	  rational-truncate rational-floor
	  rational->string))

(define-structure rationals rationals-interface
  (open scheme-sans-arithmetic
	integers integers-r5rs
	ratnums
	srfi-23 ; ERROR
	)
  (files rational))

(define-interface compnums-interface
  (export compnum ; temporary
	  compnum?
	  make-compnum
	  make-compnum-polar
	  compnum-real compnum-imag
	  compnum+ compnum- compnum* compnum/
	  compnum=? compnum-zero?
	  compnum-angle compnum-magnitude
	  compnum-exp compnum-log compnum-sqrt
	  compnum-sin compnum-cos compnum-tan
	  compnum-asin compnum-acos compnum-atan1
	  compnum->string))

(define-structures ((compnums compnums-interface)
		    (compnums-r5rs (export r5rs->compnum)))
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose real-part imag-part))
	srfi-9 ; define-record-types
	(subset define-record-types (define-record-discloser))
	flonums flonums-r5rs
	flonums-to-strings)
  (files compnum))

(define-interface recnums-interface
  (export make-recnum
	  recnum?
	  rectangulate ; temporary
	  recnum-real recnum-imag
	  recnum+ recnum- recnum* recnum/
	  recnum=?
	  recnum->string))

(define-structures ((recnums recnums-interface)
		    (recnums-r5rs (export r5rs->recnum)))
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose real-part imag-part))
	srfi-9 ; define-record-types
	(subset define-record-types (define-record-discloser))
	rationals 
	integers-r5rs
	)
  (files recnum))

(define-interface rationals-to-flonums-interface
  (export rational->flonum
	  integer->flonum))

(define-structure rationals-to-flonums rationals-to-flonums-interface
  (open scheme-sans-arithmetic
	fixnums
	integers integers-r5rs
	ratnums rationals
	flonums flonums-r5rs
	srfi-23)
  (files rational2flonum))

(define-interface flonums-to-rationals-interface
  (export flonum->rational
	  flonum->integer))

(define-structure flonums-to-rationals flonums-to-rationals-interface
  (open scheme-sans-arithmetic
	fixnums
	integers integers-r5rs
	ratnums
	flonums flonums-ieee
	rationals
	rationals-to-flonums
	srfi-23)
  (files flonum2rational))

(define-structure bellerophon (export bellerophon)
  (open scheme-sans-arithmetic
	integers integers-r5rs
	flonums flonums-r5rs
	flonums-ieee
	rationals-to-flonums flonums-to-rationals
	srfi-23)
  (files bellerophon))

(define-structure flonums-to-strings (export flonum->string)
  (open scheme-sans-arithmetic
	integers integers-r5rs
	flonums flonums-r5rs
	flonums-ieee
	flonums-to-rationals)
  (files flonum2string))

(define-structure numbers-to-strings (export number->string)
  (open scheme-sans-arithmetic
	fixnums bignums flonums compnums ratnums recnums
	integers integers-r5rs
	flonums-to-strings
	srfi-23 ; error
	)
  (files number2string))

(define-structure r5rs-to-numbers (export r5rs->number)
  (open scheme-sans-arithmetic
        r5rs-arithmetic
	integers-r5rs
	flonums-r5rs
	recnums-r5rs
	compnums-r5rs
	ratnums-r5rs)
  (files r5rs2number))

(define-interface contagion-utils-interface
  (export fixnum->ratnum fixnum->recnum fixnum->compnum
	  bignum->ratnum bignum->recnum bignum->compnum 
	  ratnum->recnum ratnum->flonum ratnum->compnum 
	  recnum->compnum 
	  flonum->compnum
	  compnum->bignum
	  compnum->integer
	  flonum->recnum
	  flonum->bignum
	  bignum->flonum
	  compnum->recnum
	  exact-integer?
	  compnum-float?
	  recnum-integral?
	  compnum-integral?
	  id
	  do-contagion
	  make-contagion-matrix
	  (define-contagion :syntax)
	  (numtype-enum :syntax)
	  ))

(define-structure contagion-utils contagion-utils-interface
  (open scheme-sans-arithmetic
	fixnums
	bignums
	ratnums
	recnums
	flonums
	compnums
	rationals-to-flonums flonums-to-rationals
	integers integers-r5rs
	flonums-r5rs
	srfi-23 ; error
	)
  (files coercion
	 contagion))

(define-structure strings-to-numbers (export string->number)
  (open scheme-sans-arithmetic
	fixnums bignums ratnums flonums flonums-r5rs
	recnums recnums-r5rs
	compnums
	integers integers-r5rs
	rationals
	bellerophon
	contagion-utils
	rationals-to-flonums flonums-to-rationals
	srfi-23 ; error
	)
  (files string2number))

(define-interface arithmetic-utils-interface
  (export make-typo-op/2 make-typo-op/1
	  never always id one one/flo))

(define-structure arithmetic-utils arithmetic-utils-interface
  (open scheme-sans-arithmetic
	integers-r5rs
	flonums-r5rs
	srfi-23 ; error
	)
  (files arithmetic-util))

(define-interface generic-arithmetic/exact-interface
  (export exact-number? exact-complex? exact-rational? exact-integer?
	  exact=? exact<? exact<=? exact>=? exact>?
	  exact-zero? exact-positive? exact-negative?
	  exact-odd? exact-even?
	  exact-min exact-max
	  exact+ exact- exact* exact/
	  exact-abs
          ;FIXME: these should go away
	  exact-quotient exact-remainder exact-quotient+remainder
	  exact-div exact-mod exact-div+mod
	  exact-div0 exact-mod0 exact-div0+mod0
          ;FIXME: this should go away
	  exact-modulo
	  exact-gcd exact-lcm
	  exact-numerator exact-denominator
	  exact-floor exact-ceiling exact-truncate exact-round
	  exact-make-rectangular
	  exact-remainder exact-real-part exact-imag-part
	  exact-expt
	  exact-sqrt
	  exact-not                                  
	  exact-ior exact-and exact-xor
          exact-if exact-bit-count exact-length
          exact-first-bit-set exact-bit-set? exact-copy-bit
          exact-bit-field exact-copy-bit-field
          exact-arithmetic-shift
	  exact-arithmetic-shift-left
          exact-arithmetic-shift-right
          exact-rotate-bit-field exact-reverse-bit-field))

(define-structure generic-arithmetic/exact generic-arithmetic/exact-interface
  (open scheme-sans-arithmetic
	integers-r5rs
	fixnums
	bignums
	ratnums ratnums-r5rs
	recnums
        ; for EXACT-INTEGER-SQRT
	(subset flonums (flsqrt flonum->fixnum fixnum->flonum))
	contagion-utils
	arithmetic-utils
	nary
	srfi-23 ; error
	)
  (files contagion-ex
	 generic-ex))

(define-interface generic-arithmetic/inexact-interface
  (export inexact-number? inexact-complex? inexact-real?
          inexact-rational? inexact-integer?
	  inexact=? inexact<? inexact<=? inexact>=? inexact>?
	  inexact-zero? inexact-positive? inexact-negative?
	  inexact-nan? inexact-finite? inexact-infinite?
	  inexact-odd? inexact-even?
	  inexact-min inexact-max
	  inexact+ inexact- inexact* inexact/
	  inexact-abs
	  inexact-quotient inexact-remainder inexact-quotient+remainder
	  inexact-div+mod inexact-div inexact-mod
	  inexact-modulo
	  inexact-gcd inexact-lcm
	  inexact-numerator inexact-denominator
	  inexact-floor inexact-ceiling inexact-truncate inexact-round
	  inexact-exp inexact-sqrt inexact-log
	  inexact-sin inexact-cos inexact-tan
          inexact-asin inexact-acos inexact-atan
	  inexact-make-rectangular inexact-make-polar
	  inexact-real-part inexact-imag-part
	  inexact-magnitude inexact-angle
	  inexact-expt))

(define-structure generic-arithmetic/inexact
  generic-arithmetic/inexact-interface
  (open scheme-sans-arithmetic

	;; all this just to implement innumerator and indenominator:
	rationals-to-flonums flonums-to-rationals
	(subset rationals (rational-numerator rational-denominator))

	flonums flonums-r5rs
	compnums compnums-r5rs
	contagion-utils
	arithmetic-utils
	nary
	srfi-23 ; error
	)
  (files contagion-in
	 generic-in))

(define-interface generic-arithmetic-interface
  (export number? complex? real? rational? integer?
	  real-valued? rational-valued? integer-valued?
	  exact? inexact?
	  = < > <= >=
	  zero? positive? negative? odd? even?
	  finite? infinite? nan?
	  max min
	  + * - /
	  abs
	  quotient remainder quotient+remainder modulo
	  div mod div+mod
	  gcd lcm
	  numerator denominator
	  floor ceiling truncate round
	  exp log sin cos tan asin acos atan
	  sqrt expt
	  make-rectangular make-polar real-part imag-part magnitude angle
	  x->inexact x->exact number->flonum
	  rationalize))

(define-structure generic-arithmetic generic-arithmetic-interface
  (open scheme-sans-arithmetic
	integers-r5rs
	fixnums
	bignums
	ratnums ratnums-r5rs
	recnums
	flonums flonums-r5rs
	compnums compnums-r5rs
	rationals-to-flonums flonums-to-rationals
	contagion-utils
	arithmetic-utils
	nary
	srfi-23 ; error
	)
  (files contagion-generic
	 generic))

(define-interface generic-arithmetic/mike-interface
  (export number? complex? real? rational? integer?
	  exact? inexact?
	  = < > <= >=
	  zero? positive? negative? odd? even?
	  max min
	  + * - /
	  abs
	  quotient remainder quotient+remainder modulo
	  gcd lcm
	  numerator denominator
	  floor ceiling truncate round
	  expt
	  make-rectangular real-part imag-part
	  x->inexact x->exact number->flonum
	 ;bitwise-not
	 ;bitwise-ior bitwise-and bitwise-xor
	  arithmetic-shift-left
	  rationalize))

(define-structure generic-arithmetic/mike generic-arithmetic/mike-interface
  (open scheme-sans-arithmetic
	integers-r5rs
	fixnums
	bignums
	ratnums ratnums-r5rs
	recnums
	flonums flonums-r5rs
	compnums compnums-r5rs
	rationals-to-flonums flonums-to-rationals
	generic-arithmetic/inexact
	contagion-utils
	arithmetic-utils
	nary
	srfi-23 ; error
	)
  (files contagion-mike
	 generic-mike))

; Putting it all together

(define-structure r6rs
  (compound-interface (interface-of scheme-sans-arithmetic)
                      fixnums-interface flonums-interface
                      generic-arithmetic/exact-interface
                      generic-arithmetic/inexact-interface
                      generic-arithmetic-interface
                      (interface-of strings-to-numbers)
                      (interface-of numbers-to-strings)
                      (interface-of r5rs-to-numbers))
  (open scheme-sans-arithmetic
	fixnums flonums
	generic-arithmetic/exact
	generic-arithmetic/inexact
	generic-arithmetic
	strings-to-numbers numbers-to-strings
	r5rs-to-numbers))

; Test suites

(define-structure test-strings-to-numbers (export)
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose +))
	(subset generic-arithmetic (=))
	r5rs-to-numbers
	strings-to-numbers numbers-to-strings)
  (files test-prelude
	 test-string2number
	 test-postlude))

(define-structure test-fixnum-arithmetic (export)
  (open scheme-sans-arithmetic
        r5rs-arithmetic
	(subset generic-arithmetic (=))
	r5rs-to-numbers
	;strings-to-numbers
        ;generic-arithmetic
        fixnums)
  (files test-prelude
	 test-fixnum-arithmetic
	 test-postlude))

(define-structure test-generic-arithmetic (export)
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose +))
	(subset flonums (flinf+ flinf- flnan flnan?))
	r5rs-to-numbers
	strings-to-numbers
	generic-arithmetic)
  (files test-prelude
	 test-generic-arithmetic
	 test-postlude))

(define-structure test-generic-arithmetic/mike (export)
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose +))
	(subset flonums (flinf+ flinf- flnan flnan?))
	r5rs-to-numbers
	strings-to-numbers
	generic-arithmetic/mike)
  (files test-prelude
	 test-generic-arithmetic-mike
	 test-postlude))

(define-structure test-generic-arithmetic/exact (export)
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose +))
	r5rs-to-numbers
	strings-to-numbers
        (subset fixnums (least-fixnum greatest-fixnum fixnum-width))
	generic-arithmetic/exact)
  (files test-prelude
	 test-generic-arithmetic-ex
	 test-postlude))

(define-structure test-generic-arithmetic/inexact (export)
  (open scheme-sans-arithmetic
	(modify scheme (prefix r5rs:) (expose +))
	(subset flonums (flinf+ flinf- flnan flnan?))
	r5rs-to-numbers
	strings-to-numbers
	generic-arithmetic/inexact)
  (files test-prelude
	 test-generic-arithmetic-in
	 test-postlude))
