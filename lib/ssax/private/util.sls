#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax private util)
  (export
    ;; Only the ones (wak ssax ---) and (wak sxml-tools ---) need
    string->integer
    string-split
    make-char-quotator
    substring?
    string-whitespace?)
  (import
    (except (rnrs) error)
    (rnrs mutable-pairs)
    (wak private include)
    (wak ssax private error)
    (wak ssax private misc)
    (except (srfi :13 strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash))
  
  (define error (make-errorer "(wak ssax private util)"))

  ; Test if a string is made of only whitespace
  ; An empty string is considered made of whitespace as well
  (define (string-whitespace? str)
    (let ((len (string-length str)))
      (cond
        ((zero? len) #T)
        ((= 1 len) (char-whitespace? (string-ref str 0)))
        ((= 2 len) (and (char-whitespace? (string-ref str 0))
                        (char-whitespace? (string-ref str 1))))
        (else
         (let loop ((i 0))
           (or (>= i len)
               (and (char-whitespace? (string-ref str i))
                    (loop (inc i)))))))))
  
  
  (include-file ((wak ssax private) util))
)
