#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax private define-opt)
  (export
    define-opt)
  (import
    (except (rnrs) error)
    (wak private include)
    (wak ssax private error))
  
  (define error (make-errorer "(wak ssax private define-opt)"))
  
  (include-file ((wak ssax private) define-opt))
)
