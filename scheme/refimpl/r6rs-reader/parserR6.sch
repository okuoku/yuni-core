(define (parse-outermost-datum)
  (case (next-token)
    ((boolean
       number
       character
       string
       id
       miscflag
       bvecstart
       vecstart
       lparen
       lbracket
       quote
       backquote
       comma
       splicing
       syntax
       quasisyntax
       unsyntax
       unsyntaxsplicing)
     (let ((ast1 (parse-datum))) (identity ast1)))
    ((eofobj) (begin (consume-token!) (makeEOF)))
    (else
     (parse-error
       '<outermost-datum>
       '(backquote
          boolean
          bvecstart
          character
          comma
          eofobj
          id
          lbracket
          lparen
          miscflag
          number
          quasisyntax
          quote
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-datum)
  (case (next-token)
    ((unsyntaxsplicing
       unsyntax
       quasisyntax
       syntax
       splicing
       comma
       backquote
       quote
       lbracket
       lparen
       vecstart
       bvecstart)
     (let ((ast1 (parse-location)))
       (let ((ast2 (parse-structured)))
         (makeStructured ast1 ast2))))
    ((miscflag) (begin (consume-token!) (makeFlag)))
    ((id) (begin (consume-token!) (makeSym)))
    ((string) (begin (consume-token!) (makeString)))
    ((character) (begin (consume-token!) (makeChar)))
    ((number) (begin (consume-token!) (makeNum)))
    ((boolean) (begin (consume-token!) (makeBool)))
    (else
     (parse-error
       '<datum>
       '(backquote
          boolean
          bvecstart
          character
          comma
          id
          lbracket
          lparen
          miscflag
          number
          quasisyntax
          quote
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-structured)
  (case (next-token)
    ((bvecstart)
     (let ((ast1 (parse-bytevector))) (identity ast1)))
    ((vecstart)
     (let ((ast1 (parse-vector))) (identity ast1)))
    ((lparen
       lbracket
       quote
       backquote
       comma
       splicing
       syntax
       quasisyntax
       unsyntax
       unsyntaxsplicing)
     (let ((ast1 (parse-list))) (identity ast1)))
    (else
     (parse-error
       '<structured>
       '(backquote
          bvecstart
          comma
          lbracket
          lparen
          quasisyntax
          quote
          splicing
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-string)
  (case (next-token)
    ((string) (begin (consume-token!) (makeString)))
    (else (parse-error '<string> '(string)))))

(define (parse-symbol)
  (case (next-token)
    ((id) (begin (consume-token!) (makeSym)))
    (else (parse-error '<symbol> '(id)))))

(define (parse-list)
  (case (next-token)
    ((unsyntaxsplicing
       unsyntax
       quasisyntax
       syntax
       splicing
       comma
       backquote
       quote)
     (let ((ast1 (parse-abbreviation)))
       (identity ast1)))
    ((lbracket)
     (begin
       (consume-token!)
       (let ((ast1 (parse-blst2))) (identity ast1))))
    ((lparen)
     (begin
       (consume-token!)
       (let ((ast1 (parse-list2))) (identity ast1))))
    (else
     (parse-error
       '<list>
       '(backquote
          comma
          lbracket
          lparen
          quasisyntax
          quote
          splicing
          syntax
          unsyntax
          unsyntaxsplicing)))))

(define (parse-list2)
  (case (next-token)
    ((boolean
       number
       character
       string
       id
       miscflag
       bvecstart
       vecstart
       lparen
       lbracket
       quote
       backquote
       comma
       splicing
       syntax
       quasisyntax
       unsyntax
       unsyntaxsplicing)
     (let ((ast1 (parse-datum)))
       (let ((ast2 (parse-list3))) (cons ast1 ast2))))
    ((rparen) (begin (consume-token!) (emptyList)))
    (else
     (parse-error
       '<list2>
       '(backquote
          boolean
          bvecstart
          character
          comma
          id
          lbracket
          lparen
          miscflag
          number
          quasisyntax
          quote
          rparen
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-list3)
  (case (next-token)
    ((rparen
       period
       unsyntaxsplicing
       unsyntax
       quasisyntax
       syntax
       splicing
       comma
       backquote
       quote
       lbracket
       lparen
       vecstart
       bvecstart
       miscflag
       id
       string
       character
       number
       boolean)
     (let ((ast1 (parse-data)))
       (let ((ast2 (parse-list4)))
         (pseudoAppend ast1 ast2))))
    (else
     (parse-error
       '<list3>
       '(backquote
          boolean
          bvecstart
          character
          comma
          id
          lbracket
          lparen
          miscflag
          number
          period
          quasisyntax
          quote
          rparen
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-list4)
  (case (next-token)
    ((period)
     (begin
       (consume-token!)
       (let ((ast1 (parse-datum)))
         (if (eq? (next-token) 'rparen)
             (begin (consume-token!) (identity ast1))
             (parse-error '<list4> '(rparen))))))
    ((rparen) (begin (consume-token!) (emptyList)))
    (else (parse-error '<list4> '(period rparen)))))

(define (parse-blst2)
  (case (next-token)
    ((boolean
       number
       character
       string
       id
       miscflag
       bvecstart
       vecstart
       lparen
       lbracket
       quote
       backquote
       comma
       splicing
       syntax
       quasisyntax
       unsyntax
       unsyntaxsplicing)
     (let ((ast1 (parse-datum)))
       (let ((ast2 (parse-blst3))) (cons ast1 ast2))))
    ((rbracket) (begin (consume-token!) (emptyList)))
    (else
     (parse-error
       '<blst2>
       '(backquote
          boolean
          bvecstart
          character
          comma
          id
          lbracket
          lparen
          miscflag
          number
          quasisyntax
          quote
          rbracket
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-blst3)
  (case (next-token)
    ((rbracket
       period
       unsyntaxsplicing
       unsyntax
       quasisyntax
       syntax
       splicing
       comma
       backquote
       quote
       lbracket
       lparen
       vecstart
       bvecstart
       miscflag
       id
       string
       character
       number
       boolean)
     (let ((ast1 (parse-data)))
       (let ((ast2 (parse-blst4)))
         (pseudoAppend ast1 ast2))))
    (else
     (parse-error
       '<blst3>
       '(backquote
          boolean
          bvecstart
          character
          comma
          id
          lbracket
          lparen
          miscflag
          number
          period
          quasisyntax
          quote
          rbracket
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-blst4)
  (case (next-token)
    ((period)
     (begin
       (consume-token!)
       (let ((ast1 (parse-datum)))
         (if (eq? (next-token) 'rbracket)
             (begin (consume-token!) (identity ast1))
             (parse-error '<blst4> '(rbracket))))))
    ((rbracket) (begin (consume-token!) (emptyList)))
    (else (parse-error '<blst4> '(period rbracket)))))

(define (parse-abbreviation)
  (case (next-token)
    ((quote backquote
            comma
            splicing
            syntax
            quasisyntax
            unsyntax
            unsyntaxsplicing)
     (let ((ast1 (parse-abbrev-prefix)))
       (let ((ast2 (parse-datum))) (list ast1 ast2))))
    (else
     (parse-error
       '<abbreviation>
       '(backquote
          comma
          quasisyntax
          quote
          splicing
          syntax
          unsyntax
          unsyntaxsplicing)))))

(define (parse-abbrev-prefix)
  (case (next-token)
    ((unsyntaxsplicing)
     (begin (consume-token!) (symUnsyntax-splicing)))
    ((unsyntax)
     (begin (consume-token!) (symUnsyntax)))
    ((quasisyntax)
     (begin (consume-token!) (symQuasisyntax)))
    ((syntax) (begin (consume-token!) (symSyntax)))
    ((splicing)
     (begin (consume-token!) (symSplicing)))
    ((comma) (begin (consume-token!) (symUnquote)))
    ((backquote)
     (begin (consume-token!) (symBackquote)))
    ((quote) (begin (consume-token!) (symQuote)))
    (else
     (parse-error
       '<abbrev-prefix>
       '(backquote
          comma
          quasisyntax
          quote
          splicing
          syntax
          unsyntax
          unsyntaxsplicing)))))

(define (parse-vector)
  (case (next-token)
    ((vecstart)
     (begin
       (consume-token!)
       (let ((ast1 (parse-data)))
         (if (eq? (next-token) 'rparen)
             (begin (consume-token!) (list2vector ast1))
             (parse-error '<vector> '(rparen))))))
    (else (parse-error '<vector> '(vecstart)))))

(define (parse-bytevector)
  (case (next-token)
    ((bvecstart)
     (begin
       (consume-token!)
       (let ((ast1 (parse-octets)))
         (if (eq? (next-token) 'rparen)
             (begin (consume-token!) (list2bytevector ast1))
             (parse-error '<bytevector> '(rparen))))))
    (else (parse-error '<bytevector> '(bvecstart)))))

(define (parse-data)
  (case (next-token)
    ((boolean
       number
       character
       string
       id
       miscflag
       bvecstart
       vecstart
       lparen
       lbracket
       quote
       backquote
       comma
       splicing
       syntax
       quasisyntax
       unsyntax
       unsyntaxsplicing)
     (let ((ast1 (parse-datum)))
       (let ((ast2 (parse-data))) (cons ast1 ast2))))
    ((rparen period rbracket) (emptyList))
    (else
     (parse-error
       '<data>
       '(backquote
          boolean
          bvecstart
          character
          comma
          id
          lbracket
          lparen
          miscflag
          number
          period
          quasisyntax
          quote
          rbracket
          rparen
          splicing
          string
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

(define (parse-octets)
  (case (next-token)
    ((number)
     (let ((ast1 (parse-octet)))
       (let ((ast2 (parse-octets))) (cons ast1 ast2))))
    ((rparen) (emptyList))
    (else (parse-error '<octets> '(number rparen)))))

(define (parse-octet)
  (case (next-token)
    ((number) (begin (consume-token!) (makeOctet)))
    (else (parse-error '<octet> '(number)))))

(define (parse-location)
  (case (next-token)
    ((unsyntaxsplicing
       unsyntax
       quasisyntax
       syntax
       splicing
       comma
       backquote
       quote
       lbracket
       lparen
       vecstart
       bvecstart)
     (sourceLocation))
    (else
     (parse-error
       '<location>
       '(backquote
          bvecstart
          comma
          lbracket
          lparen
          quasisyntax
          quote
          splicing
          syntax
          unsyntax
          unsyntaxsplicing
          vecstart)))))

