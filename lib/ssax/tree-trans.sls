#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax tree-trans)
  (export
    SRV:send-reply
    pre-post-order
    post-order
    foldts
    replace-range)
  (import
    (except (rnrs) error)
    (wak private include)
    (wak ssax private error))
  
  (define error (make-errorer "(wak ssax tree-trans)"))
  
  (include-file ((wak ssax private) SXML-tree-trans))
)
