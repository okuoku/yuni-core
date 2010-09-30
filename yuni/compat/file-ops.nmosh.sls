(library (yuni compat file-ops)
         (export 
           ;; chez like file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory

           ;; ancient
           system-msdos-style-path?
           )
         (import (rnrs)
                 (mosh)
                 (mosh file))
(define (system-msdos-style-path?)
  (string=? "win32" (host-os)))

)


