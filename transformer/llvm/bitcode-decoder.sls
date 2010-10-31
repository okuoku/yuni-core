(library (yuni transformer llvm bitcode-decoder)
         (export bitcode-decode)
         (import (rnrs)
                 (for (yuni transformer llvm bitcode-dict) run expand))

(define-syntax decoder
  (syntax-rules ()
    ((_ (#f) input)
     input)
    ((_ (root) input)
     (if (pair? input)
       (let ((code (car input))
             (rest (cdr input)))
         ;(display (list 'DECODE code '=> (root code)))(newline)
         (cons (root code)
               rest))
       input))))

(define-syntax define-bitcode-blocks
  (syntax-rules ()
    ((_ name root (spec-sym spec-code ...) ...)
     (define (name block-id record)
       (let ((sym (root block-id)))
         (case sym
           ((spec-sym) (decoder (spec-code ...) record))
           ...
           (else record)))))))

(define-bitcode-blocks bitcode-decode
   llvm-ir-blockid
   (MODULE record-MODULE_BLOCK)
   (PARAMATTR record-PARAMATTR_BLOCK)
   (TYPE record-TYPE_BLOCK)
   (CONSTANTS record-CONSTANTS_BLOCK)
   (TYPE_SYMTAB record-TYPE_SYMTAB_BLOCK)
   (VALUE_SYMTAB record-VALUE_SYMTAB_BLOCK)
   (METADATA record-METADATA_BLOCK)
   (METADATA_ATTACHMENT #f))

)
