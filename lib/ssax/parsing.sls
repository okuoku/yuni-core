#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; NOTE: The SXML created and understood by this library uses
;;       ^ instead of @ because @ is not a valid R6RS symbol.

(library (wak ssax parsing)
  (export
    make-xml-token
    xml-token?
    xml-token-kind
    xml-token-head
    ssax:S-chars
    ssax:skip-S
    ssax:ncname-starting-char?
    ssax:read-NCName
    ssax:read-QName
    ssax:Prefix-XML
    name-compare
    ssax:largest-unres-name
    ssax:read-markup-token
    ssax:skip-pi
    ssax:read-pi-body-as-string
    ssax:skip-internal-dtd
    ssax:read-cdata-body
    ssax:read-char-ref
    ssax:predefined-parsed-entities
    ssax:handle-parsed-entity
    make-empty-attlist
    attlist-add
    attlist-null?
    attlist-remove-top
    attlist->alist
    attlist-fold
    ssax:read-attributes
    ssax:resolve-name
    ssax:uri-string->symbol
    ssax:complete-start-tag
    ssax:read-external-id
    ssax:scan-Misc
    ssax:read-char-data
    ssax:assert-token
    ssax:make-pi-parser
    ssax:make-elem-parser
    ssax:make-parser
    ssax:make-parser/positional-args
    ssax:define-labeled-arg-macro
    ssax:reverse-collect-str
    ssax:reverse-collect-str-drop-ws
    ssax:xml->sxml
    SSAX:XML->SXML)
  (import
    (except (rnrs) fold-right error)
    (wak private include)
    (except (srfi :13 strings) string-copy string->list string-titlecase
            string-upcase string-downcase string-hash string-for-each)
    (wak ssax raise)
    (wak ssax private define-opt)
    (wak ssax private input-parse)
    (wak ssax private look-for-str)
    (wak ssax private output)
    (wak ssax private misc)
    (wak ssax private error)
    (wak ssax private util))
  
  (define error (make-errorer "(wak ssax parsing)"))

  (define-syntax begin0
    (syntax-rules ()
      ((begin0 expression0 expressions ...)
       (call-with-values (lambda () expression0)
         (lambda results
           expressions ...
           (apply values results))))))
  
  (include-file ((wak ssax private) SSAX))
)
