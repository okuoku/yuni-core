#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax sxpath)
  (export
    nodeset?
    node-typeof?
    node-eq?
    node-equal?
    node-pos
    (rename (filter node-filter))
    take-until
    take-after
    map-union
    node-reverse
    node-trace
    select-kids
    node-self
    node-join
    node-reduce
    node-or
    node-closure
    node-parent
    sxpath)
  (import
    (except (rnrs) error filter)
    (srfi :48 intermediate-format-strings)
    (wak private include)
    (wak ssax private error)
    (wak ssax private output)
    (wak ssax private misc))
  
  (define error (make-errorer "(wak ssax sxpath)"))

  (define (pretty-print x)
    (format #t "~y" x))
  
  (include-file ("wak" "ssax" "private") "SXPath-old.scm")
)
