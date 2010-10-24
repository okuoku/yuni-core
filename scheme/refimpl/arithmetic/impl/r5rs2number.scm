; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting R5RS numbers to those of the reference implementation

(define (r5rs->number n)
  (cond
   ((core:exact? n)
    (cond
     ((core:integer? n)
      (r5rs->integer n))
     ((core:rational? n)
      (r5rs->ratnum n))
     ((core:complex? n)
      (r5rs->recnum n))
     (else #f)))
   
   ((core:real? n)
    (r5rs->flonum n))
   (else
    (r5rs->compnum n))))

      
   
		