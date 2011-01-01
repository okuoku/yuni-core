#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax private look-for-str)
  (export
    MISCIO:find-string-from-port?
    find-string-from-port?)
  (import
    (rnrs)
    (wak private include)
    (wak ssax private misc))
  
  (include-file ((wak ssax private) look-for-str))
)
