#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak ssax private to-html-ext)
  (export
    make-header
    make-navbar
    make-footer
    universal-conversion-rules
    universal-protected-rules
    alist-conv-rules
    find-Header
    generic-web-rules)
  (import
    (except (rnrs) error)
    (rnrs r5rs)
    (wak private include)
    (except (srfi :13 strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash)
    (wak ssax private to-html)
    (wak ssax tree-trans)
    (wak ssax private error)
    (wak ssax private output)
    (wak ssax private misc)
    (wak ssax private util))
  
  (define (OS:file-length filename)
    (if (file-exists? filename) 1 0))
  
  (define error (make-errorer "(wak ssax private to-html-ext)"))
  
  (include-file ((wak ssax private) SXML-to-HTML-ext))
)
