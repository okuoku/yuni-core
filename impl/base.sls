(library (yuni impl base)
         (export 
           define cond case
           assertion-violation
           quote else
           symbol?  null?  pair?  vector?
           if let or and lambda set! unless
           cons car cdr apply vector vector->list eq? list list?
           length
           cadr caddr cadddr
           map append not + = 
           reverse
           
           display newline write
           )
         (import (rnrs)))
