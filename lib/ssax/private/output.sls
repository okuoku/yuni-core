#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax private output)
  (export
    cout cerr nl)
  (import
    (rnrs)
    (wak private include))  

  (include-file ((wak ssax private) output))
)
