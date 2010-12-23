(define (scanner0)
  (define (state0 c)
    (case c
      ((#\`) (consumeChar) (accept 'backquote))
      ((#\') (consumeChar) (accept 'quote))
      ((#\]) (consumeChar) (accept 'rbracket))
      ((#\[) (consumeChar) (accept 'lbracket))
      ((#\)) (consumeChar) (accept 'rparen))
      ((#\() (consumeChar) (accept 'lparen))
      ((#\tab #\newline #\vtab #\page #\return #\space)
       (consumeChar)
       (begin
         (set! string_accumulator_length 0)
         (state0 (scanChar))))
      ((#\;) (consumeChar) (state212 (scanChar)))
      ((#\#) (consumeChar) (state211 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state140 (scanChar)))
      ((#\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\g
        #\h
        #\i
        #\j
        #\k
        #\l
        #\m
        #\n
        #\o
        #\p
        #\q
        #\r
        #\s
        #\t
        #\u
        #\v
        #\w
        #\x
        #\y
        #\z
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F
        #\G
        #\H
        #\I
        #\J
        #\K
        #\L
        #\M
        #\N
        #\O
        #\P
        #\Q
        #\R
        #\S
        #\T
        #\U
        #\V
        #\W
        #\X
        #\Y
        #\Z
        #\!
        #\$
        #\%
        #\&
        #\*
        #\/
        #\:
        #\<
        #\=
        #\>
        #\?
        #\^
        #\_
        #\~)
       (consumeChar)
       (state13 (scanChar)))
      ((#\\) (consumeChar) (state12 (scanChar)))
      ((#\-) (consumeChar) (state9 (scanChar)))
      ((#\+) (consumeChar) (state8 (scanChar)))
      ((#\.) (consumeChar) (state7 (scanChar)))
      ((#\") (consumeChar) (state5 (scanChar)))
      ((#\,) (consumeChar) (state1 (scanChar)))
      (else
       (if ((lambda (c)
              (and (char? c)
                   (> (char->integer c) 127)
                   (let ((cat (char-general-category c)))
                     (memq cat
                           '(Lu Ll
                                Lt
                                Lm
                                Lo
                                Mn
                                Nl
                                No
                                Pd
                                Pc
                                Po
                                Sc
                                Sm
                                Sk
                                So
                                Co)))))
            c)
           (begin (consumeChar) (state13 (scanChar)))
           (if (eof-object? c)
               (begin (consumeChar) (accept 'eofobj))
               (if ((lambda (c) (and (char? c) (char-whitespace? c)))
                    c)
                   (begin
                     (consumeChar)
                     (begin
                       (set! string_accumulator_length 0)
                       (state0 (scanChar))))
                   (if ((lambda (c)
                          (and (char? c) (char=? c (integer->char 133))))
                        c)
                       (begin
                         (consumeChar)
                         (begin
                           (set! string_accumulator_length 0)
                           (state0 (scanChar))))
                       (scannerError errIncompleteToken))))))))
  (define (state1 c)
    (case c
      ((#\@) (consumeChar) (accept 'splicing))
      (else (accept 'comma))))
  (define (state2 c)
    (case c
      ((#\") (consumeChar) (accept 'string))
      ((#\newline #\return)
       (consumeChar)
       (state5 (scanChar)))
      ((#\\) (consumeChar) (state4 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state2 (scanChar)))
      (else
       (if (char? c)
           (begin (consumeChar) (state5 (scanChar)))
           (if ((lambda (c)
                  (and (char? c) (char=? c (integer->char 8232))))
                c)
               (begin (consumeChar) (state5 (scanChar)))
               (if ((lambda (c)
                      (and (char? c) (char=? c (integer->char 133))))
                    c)
                   (begin (consumeChar) (state5 (scanChar)))
                   (scannerError errIncompleteToken)))))))
  (define (state3 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state2 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state4 c)
    (case c
      ((#\a
        #\b
        #\t
        #\n
        #\v
        #\f
        #\r
        #\"
        #\\
        #\newline
        #\return
        #\space)
       (consumeChar)
       (state5 (scanChar)))
      ((#\x) (consumeChar) (state3 (scanChar)))
      (else
       (if ((lambda (c)
              (and (char? c) (char=? c (integer->char 8232))))
            c)
           (begin (consumeChar) (state5 (scanChar)))
           (if ((lambda (c)
                  (and (char? c) (char=? c (integer->char 133))))
                c)
               (begin (consumeChar) (state5 (scanChar)))
               (scannerError errIncompleteToken))))))
  (define (state5 c)
    (case c
      ((#\") (consumeChar) (accept 'string))
      ((#\newline #\return)
       (consumeChar)
       (state5 (scanChar)))
      ((#\\) (consumeChar) (state4 (scanChar)))
      (else
       (if (char? c)
           (begin (consumeChar) (state5 (scanChar)))
           (if ((lambda (c)
                  (and (char? c) (char=? c (integer->char 8232))))
                c)
               (begin (consumeChar) (state5 (scanChar)))
               (if ((lambda (c)
                      (and (char? c) (char=? c (integer->char 133))))
                    c)
                   (begin (consumeChar) (state5 (scanChar)))
                   (scannerError errIncompleteToken)))))))
  (define (state6 c)
    (case c
      ((#\.) (consumeChar) (accept 'id))
      (else (scannerError errIncompleteToken))))
  (define (state7 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state129 (scanChar)))
      ((#\.) (consumeChar) (state6 (scanChar)))
      (else (accept 'period))))
  (define (state8 c)
    (case c
      ((#\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state202 (scanChar)))
      ((#\.) (consumeChar) (state148 (scanChar)))
      ((#\n) (consumeChar) (state147 (scanChar)))
      ((#\i) (consumeChar) (state142 (scanChar)))
      (else (accept 'id))))
  (define (state9 c)
    (case c
      ((#\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state202 (scanChar)))
      ((#\.) (consumeChar) (state148 (scanChar)))
      ((#\n) (consumeChar) (state147 (scanChar)))
      ((#\i) (consumeChar) (state142 (scanChar)))
      ((#\>) (consumeChar) (state13 (scanChar)))
      (else (accept 'id))))
  (define (state10 c)
    (case c
      ((#\;) (consumeChar) (state13 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state10 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state11 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state10 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state12 c)
    (case c
      ((#\x) (consumeChar) (state11 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state13 c)
    (case c
      ((#\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\g
        #\h
        #\i
        #\j
        #\k
        #\l
        #\m
        #\n
        #\o
        #\p
        #\q
        #\r
        #\s
        #\t
        #\u
        #\v
        #\w
        #\x
        #\y
        #\z
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F
        #\G
        #\H
        #\I
        #\J
        #\K
        #\L
        #\M
        #\N
        #\O
        #\P
        #\Q
        #\R
        #\S
        #\T
        #\U
        #\V
        #\W
        #\X
        #\Y
        #\Z
        #\!
        #\$
        #\%
        #\&
        #\*
        #\/
        #\:
        #\<
        #\=
        #\>
        #\?
        #\^
        #\_
        #\~
        #\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\+
        #\-
        #\.
        #\@)
       (consumeChar)
       (state13 (scanChar)))
      ((#\\) (consumeChar) (state12 (scanChar)))
      (else
       (if ((lambda (c)
              (and (char? c)
                   (let ((cat (char-general-category c)))
                     (memq cat '(Nd Mc Me)))))
            c)
           (begin (consumeChar) (state13 (scanChar)))
           (if ((lambda (c)
                  (and (char? c)
                       (> (char->integer c) 127)
                       (let ((cat (char-general-category c)))
                         (memq cat
                               '(Lu Ll
                                    Lt
                                    Lm
                                    Lo
                                    Mn
                                    Nl
                                    No
                                    Pd
                                    Pc
                                    Po
                                    Sc
                                    Sm
                                    Sk
                                    So
                                    Co)))))
                c)
               (begin (consumeChar) (state13 (scanChar)))
               (accept 'id))))))
  (define (state14 c)
    (case c
      ((#\@) (consumeChar) (accept 'unsyntaxsplicing))
      (else (accept 'unsyntax))))
  (define (state15 c)
    (case c
      ((#\() (consumeChar) (accept 'bvecstart))
      (else (scannerError errIncompleteToken))))
  (define (state16 c)
    (case c
      ((#\8) (consumeChar) (state15 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state17 c)
    (case c
      ((#\u) (consumeChar) (state16 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state18 c)
    (case c
      ((#\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\g
        #\h
        #\i
        #\j
        #\k
        #\l
        #\m
        #\n
        #\o
        #\p
        #\q
        #\r
        #\s
        #\t
        #\u
        #\v
        #\w
        #\x
        #\y
        #\z
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F
        #\G
        #\H
        #\I
        #\J
        #\K
        #\L
        #\M
        #\N
        #\O
        #\P
        #\Q
        #\R
        #\S
        #\T
        #\U
        #\V
        #\W
        #\X
        #\Y
        #\Z)
       (consumeChar)
       (state18 (scanChar)))
      (else (accept 'character))))
  (define (state19 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state19 (scanChar)))
      (else (accept 'character))))
  (define (state20 c)
    (case c
      ((#\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
       (consumeChar)
       (state20 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state19 (scanChar)))
      ((#\g
        #\h
        #\i
        #\j
        #\k
        #\l
        #\m
        #\n
        #\o
        #\p
        #\q
        #\r
        #\s
        #\t
        #\u
        #\v
        #\w
        #\x
        #\y
        #\z
        #\G
        #\H
        #\I
        #\J
        #\K
        #\L
        #\M
        #\N
        #\O
        #\P
        #\Q
        #\R
        #\S
        #\T
        #\U
        #\V
        #\W
        #\X
        #\Y
        #\Z)
       (consumeChar)
       (state18 (scanChar)))
      (else (accept 'character))))
  (define (state21 c)
    (case c
      ((#\x) (consumeChar) (state20 (scanChar)))
      ((#\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\g
        #\h
        #\i
        #\j
        #\k
        #\l
        #\m
        #\n
        #\o
        #\p
        #\q
        #\r
        #\s
        #\t
        #\u
        #\v
        #\w
        #\y
        #\z
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F
        #\G
        #\H
        #\I
        #\J
        #\K
        #\L
        #\M
        #\N
        #\O
        #\P
        #\Q
        #\R
        #\S
        #\T
        #\U
        #\V
        #\W
        #\X
        #\Y
        #\Z)
       (consumeChar)
       (state18 (scanChar)))
      (else
       (if (char? c)
           (begin (consumeChar) (accept 'character))
           (scannerError errIncompleteToken)))))
  (define (state22 c)
    (case c
      ((#\i #\I #\e #\E)
       (consumeChar)
       (state57 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state23 c)
    (case c
      ((#\+ #\-) (consumeChar) (state56 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state32 (scanChar)))
      ((#\#) (consumeChar) (state22 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state24 c)
    (case c
      ((#\i #\I #\e #\E)
       (consumeChar)
       (state87 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state25 c)
    (case c
      ((#\+ #\-) (consumeChar) (state86 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state62 (scanChar)))
      ((#\#) (consumeChar) (state24 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state26 c)
    (case c
      ((#\i #\I #\e #\E)
       (consumeChar)
       (state125 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state27 c)
    (case c
      ((#\+ #\-) (consumeChar) (state124 (scanChar)))
      ((#\0 #\1) (consumeChar) (state92 (scanChar)))
      ((#\#) (consumeChar) (state26 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state28 c)
    (case c
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      ((#\#) (consumeChar) (state28 (scanChar)))
      (else (accept 'number))))
  (define (state29 c)
    (case c
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state29 (scanChar)))
      ((#\#) (consumeChar) (state28 (scanChar)))
      (else (accept 'number))))
  (define (state30 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state29 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state31 c)
    (case c
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      ((#\#) (consumeChar) (state31 (scanChar)))
      ((#\/) (consumeChar) (state30 (scanChar)))
      (else (accept 'number))))
  (define (state32 c)
    (case c
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state32 (scanChar)))
      ((#\#) (consumeChar) (state31 (scanChar)))
      ((#\/) (consumeChar) (state30 (scanChar)))
      (else (accept 'number))))
  (define (state33 c)
    (case c
      ((#\f) (consumeChar) (state37 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state34 c)
    (case c
      ((#\n) (consumeChar) (state33 (scanChar)))
      (else (accept 'number))))
  (define (state35 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      (else (accept 'number))))
  (define (state36 c)
    (case c
      ((#\0) (consumeChar) (state35 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state37 c)
    (case c
      ((#\.) (consumeChar) (state36 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state38 c)
    (case c
      ((#\n) (consumeChar) (state37 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state39 c)
    (case c
      ((#\a) (consumeChar) (state38 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state40 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state107 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state40 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state41 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state40 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state42 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state42 (scanChar)))
      ((#\/) (consumeChar) (state41 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state43 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state43 (scanChar)))
      ((#\#) (consumeChar) (state42 (scanChar)))
      ((#\/) (consumeChar) (state41 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state44 c)
    (case c
      ((#\n) (consumeChar) (state106 (scanChar)))
      ((#\i) (consumeChar) (state101 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state43 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state45 c)
    (case c
      ((#\#) (consumeChar) (state191 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state45 (scanChar)))
      (else (accept 'number))))
  (define (state46 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state45 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state47 c)
    (case c
      ((#\#) (consumeChar) (state47 (scanChar)))
      ((#\/) (consumeChar) (state46 (scanChar)))
      (else (accept 'number))))
  (define (state48 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state48 (scanChar)))
      ((#\#) (consumeChar) (state47 (scanChar)))
      ((#\/) (consumeChar) (state46 (scanChar)))
      (else (accept 'number))))
  (define (state49 c)
    (case c
      ((#\n) (consumeChar) (state182 (scanChar)))
      ((#\i) (consumeChar) (state178 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state48 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state50 c)
    (case c
      ((#\+ #\-) (consumeChar) (state49 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state48 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state51 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state51 (scanChar)))
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      (else (accept 'number))))
  (define (state52 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state52 (scanChar)))
      ((#\#) (consumeChar) (state51 (scanChar)))
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      (else (accept 'number))))
  (define (state53 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state52 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state54 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state54 (scanChar)))
      ((#\/) (consumeChar) (state53 (scanChar)))
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      (else (accept 'number))))
  (define (state55 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state55 (scanChar)))
      ((#\#) (consumeChar) (state54 (scanChar)))
      ((#\/) (consumeChar) (state53 (scanChar)))
      ((#\@) (consumeChar) (state50 (scanChar)))
      ((#\+ #\-) (consumeChar) (state44 (scanChar)))
      (else (accept 'number))))
  (define (state56 c)
    (case c
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state55 (scanChar)))
      ((#\n) (consumeChar) (state39 (scanChar)))
      ((#\i) (consumeChar) (state34 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state57 c)
    (case c
      ((#\+ #\-) (consumeChar) (state56 (scanChar)))
      ((#\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
        #\a
        #\b
        #\c
        #\d
        #\e
        #\f
        #\A
        #\B
        #\C
        #\D
        #\E
        #\F)
       (consumeChar)
       (state32 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state58 c)
    (case c
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      ((#\#) (consumeChar) (state58 (scanChar)))
      (else (accept 'number))))
  (define (state59 c)
    (case c
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state59 (scanChar)))
      ((#\#) (consumeChar) (state58 (scanChar)))
      (else (accept 'number))))
  (define (state60 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state59 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state61 c)
    (case c
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      ((#\#) (consumeChar) (state61 (scanChar)))
      ((#\/) (consumeChar) (state60 (scanChar)))
      (else (accept 'number))))
  (define (state62 c)
    (case c
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state62 (scanChar)))
      ((#\#) (consumeChar) (state61 (scanChar)))
      ((#\/) (consumeChar) (state60 (scanChar)))
      (else (accept 'number))))
  (define (state63 c)
    (case c
      ((#\f) (consumeChar) (state67 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state64 c)
    (case c
      ((#\n) (consumeChar) (state63 (scanChar)))
      (else (accept 'number))))
  (define (state65 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      (else (accept 'number))))
  (define (state66 c)
    (case c
      ((#\0) (consumeChar) (state65 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state67 c)
    (case c
      ((#\.) (consumeChar) (state66 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state68 c)
    (case c
      ((#\n) (consumeChar) (state67 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state69 c)
    (case c
      ((#\a) (consumeChar) (state68 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state70 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state107 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state70 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state71 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state70 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state72 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state72 (scanChar)))
      ((#\/) (consumeChar) (state71 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state73 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state73 (scanChar)))
      ((#\#) (consumeChar) (state72 (scanChar)))
      ((#\/) (consumeChar) (state71 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state74 c)
    (case c
      ((#\n) (consumeChar) (state106 (scanChar)))
      ((#\i) (consumeChar) (state101 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state73 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state75 c)
    (case c
      ((#\#) (consumeChar) (state191 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state75 (scanChar)))
      (else (accept 'number))))
  (define (state76 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state75 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state77 c)
    (case c
      ((#\#) (consumeChar) (state77 (scanChar)))
      ((#\/) (consumeChar) (state76 (scanChar)))
      (else (accept 'number))))
  (define (state78 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state78 (scanChar)))
      ((#\#) (consumeChar) (state77 (scanChar)))
      ((#\/) (consumeChar) (state76 (scanChar)))
      (else (accept 'number))))
  (define (state79 c)
    (case c
      ((#\n) (consumeChar) (state182 (scanChar)))
      ((#\i) (consumeChar) (state178 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state78 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state80 c)
    (case c
      ((#\+ #\-) (consumeChar) (state79 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state78 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state81 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state81 (scanChar)))
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      (else (accept 'number))))
  (define (state82 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state82 (scanChar)))
      ((#\#) (consumeChar) (state81 (scanChar)))
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      (else (accept 'number))))
  (define (state83 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state82 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state84 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state84 (scanChar)))
      ((#\/) (consumeChar) (state83 (scanChar)))
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      (else (accept 'number))))
  (define (state85 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state85 (scanChar)))
      ((#\#) (consumeChar) (state84 (scanChar)))
      ((#\/) (consumeChar) (state83 (scanChar)))
      ((#\@) (consumeChar) (state80 (scanChar)))
      ((#\+ #\-) (consumeChar) (state74 (scanChar)))
      (else (accept 'number))))
  (define (state86 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state85 (scanChar)))
      ((#\n) (consumeChar) (state69 (scanChar)))
      ((#\i) (consumeChar) (state64 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state87 c)
    (case c
      ((#\+ #\-) (consumeChar) (state86 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (consumeChar)
       (state62 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state88 c)
    (case c
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      ((#\#) (consumeChar) (state88 (scanChar)))
      (else (accept 'number))))
  (define (state89 c)
    (case c
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      ((#\0 #\1) (consumeChar) (state89 (scanChar)))
      ((#\#) (consumeChar) (state88 (scanChar)))
      (else (accept 'number))))
  (define (state90 c)
    (case c
      ((#\0 #\1) (consumeChar) (state89 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state91 c)
    (case c
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      ((#\#) (consumeChar) (state91 (scanChar)))
      ((#\/) (consumeChar) (state90 (scanChar)))
      (else (accept 'number))))
  (define (state92 c)
    (case c
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      ((#\0 #\1) (consumeChar) (state92 (scanChar)))
      ((#\#) (consumeChar) (state91 (scanChar)))
      ((#\/) (consumeChar) (state90 (scanChar)))
      (else (accept 'number))))
  (define (state93 c)
    (case c
      ((#\f) (consumeChar) (state97 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state94 c)
    (case c
      ((#\n) (consumeChar) (state93 (scanChar)))
      (else (accept 'number))))
  (define (state95 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      (else (accept 'number))))
  (define (state96 c)
    (case c
      ((#\0) (consumeChar) (state95 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state97 c)
    (case c
      ((#\.) (consumeChar) (state96 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state98 c)
    (case c
      ((#\n) (consumeChar) (state97 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state99 c)
    (case c
      ((#\a) (consumeChar) (state98 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state100 c)
    (case c
      ((#\f) (consumeChar) (state104 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state101 c)
    (case c
      ((#\n) (consumeChar) (state100 (scanChar)))
      (else (accept 'number))))
  (define (state102 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      (else (scannerError errIncompleteToken))))
  (define (state103 c)
    (case c
      ((#\0) (consumeChar) (state102 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state104 c)
    (case c
      ((#\.) (consumeChar) (state103 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state105 c)
    (case c
      ((#\n) (consumeChar) (state104 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state106 c)
    (case c
      ((#\a) (consumeChar) (state105 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state107 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state107 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state108 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1) (consumeChar) (state108 (scanChar)))
      ((#\#) (consumeChar) (state107 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state109 c)
    (case c
      ((#\0 #\1) (consumeChar) (state108 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state110 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state110 (scanChar)))
      ((#\/) (consumeChar) (state109 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state111 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1) (consumeChar) (state111 (scanChar)))
      ((#\#) (consumeChar) (state110 (scanChar)))
      ((#\/) (consumeChar) (state109 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state112 c)
    (case c
      ((#\0 #\1) (consumeChar) (state111 (scanChar)))
      ((#\n) (consumeChar) (state106 (scanChar)))
      ((#\i) (consumeChar) (state101 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state113 c)
    (case c
      ((#\#) (consumeChar) (state191 (scanChar)))
      ((#\0 #\1) (consumeChar) (state113 (scanChar)))
      (else (accept 'number))))
  (define (state114 c)
    (case c
      ((#\0 #\1) (consumeChar) (state113 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state115 c)
    (case c
      ((#\#) (consumeChar) (state115 (scanChar)))
      ((#\/) (consumeChar) (state114 (scanChar)))
      (else (accept 'number))))
  (define (state116 c)
    (case c
      ((#\0 #\1) (consumeChar) (state116 (scanChar)))
      ((#\#) (consumeChar) (state115 (scanChar)))
      ((#\/) (consumeChar) (state114 (scanChar)))
      (else (accept 'number))))
  (define (state117 c)
    (case c
      ((#\n) (consumeChar) (state182 (scanChar)))
      ((#\i) (consumeChar) (state178 (scanChar)))
      ((#\0 #\1) (consumeChar) (state116 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state118 c)
    (case c
      ((#\+ #\-) (consumeChar) (state117 (scanChar)))
      ((#\0 #\1) (consumeChar) (state116 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state119 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state119 (scanChar)))
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      (else (accept 'number))))
  (define (state120 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1) (consumeChar) (state120 (scanChar)))
      ((#\#) (consumeChar) (state119 (scanChar)))
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      (else (accept 'number))))
  (define (state121 c)
    (case c
      ((#\0 #\1) (consumeChar) (state120 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state122 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state122 (scanChar)))
      ((#\/) (consumeChar) (state121 (scanChar)))
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      (else (accept 'number))))
  (define (state123 c)
    (case c
      ((#\i) (consumeChar) (accept 'number))
      ((#\0 #\1) (consumeChar) (state123 (scanChar)))
      ((#\#) (consumeChar) (state122 (scanChar)))
      ((#\/) (consumeChar) (state121 (scanChar)))
      ((#\@) (consumeChar) (state118 (scanChar)))
      ((#\+ #\-) (consumeChar) (state112 (scanChar)))
      (else (accept 'number))))
  (define (state124 c)
    (case c
      ((#\0 #\1) (consumeChar) (state123 (scanChar)))
      ((#\n) (consumeChar) (state99 (scanChar)))
      ((#\i) (consumeChar) (state94 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state125 c)
    (case c
      ((#\+ #\-) (consumeChar) (state124 (scanChar)))
      ((#\0 #\1) (consumeChar) (state92 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state126 c)
    (case c
      ((#\d #\D) (consumeChar) (state204 (scanChar)))
      ((#\b #\B) (consumeChar) (state125 (scanChar)))
      ((#\o #\O) (consumeChar) (state87 (scanChar)))
      ((#\x #\X) (consumeChar) (state57 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state127 c)
    (case c
      ((#\+ #\-) (consumeChar) (state203 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state140 (scanChar)))
      ((#\.) (consumeChar) (state128 (scanChar)))
      ((#\#) (consumeChar) (state126 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state128 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state129 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state129 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\#) (consumeChar) (state135 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state134 (scanChar)))
      ((#\|) (consumeChar) (state131 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state129 (scanChar)))
      (else (accept 'number))))
  (define (state130 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state130 (scanChar)))
      (else (accept 'number))))
  (define (state131 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state130 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state132 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state132 (scanChar)))
      ((#\|) (consumeChar) (state131 (scanChar)))
      (else (accept 'number))))
  (define (state133 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state132 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state134 c)
    (case c
      ((#\+ #\-) (consumeChar) (state133 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state132 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state135 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\#) (consumeChar) (state135 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state134 (scanChar)))
      ((#\|) (consumeChar) (state131 (scanChar)))
      (else (accept 'number))))
  (define (state136 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\#) (consumeChar) (state136 (scanChar)))
      (else (accept 'number))))
  (define (state137 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state137 (scanChar)))
      ((#\#) (consumeChar) (state136 (scanChar)))
      (else (accept 'number))))
  (define (state138 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state137 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state139 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\#) (consumeChar) (state139 (scanChar)))
      ((#\/) (consumeChar) (state138 (scanChar)))
      ((#\.) (consumeChar) (state135 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state134 (scanChar)))
      ((#\|) (consumeChar) (state131 (scanChar)))
      (else (accept 'number))))
  (define (state140 c)
    (case c
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state140 (scanChar)))
      ((#\#) (consumeChar) (state139 (scanChar)))
      ((#\/) (consumeChar) (state138 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state134 (scanChar)))
      ((#\|) (consumeChar) (state131 (scanChar)))
      ((#\.) (consumeChar) (state129 (scanChar)))
      (else (accept 'number))))
  (define (state141 c)
    (case c
      ((#\f) (consumeChar) (state145 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state142 c)
    (case c
      ((#\n) (consumeChar) (state141 (scanChar)))
      (else (accept 'number))))
  (define (state143 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      (else (accept 'number))))
  (define (state144 c)
    (case c
      ((#\0) (consumeChar) (state143 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state145 c)
    (case c
      ((#\.) (consumeChar) (state144 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state146 c)
    (case c
      ((#\n) (consumeChar) (state145 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state147 c)
    (case c
      ((#\a) (consumeChar) (state146 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state148 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state149 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state149 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\#) (consumeChar) (state155 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state154 (scanChar)))
      ((#\|) (consumeChar) (state151 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state149 (scanChar)))
      (else (accept 'number))))
  (define (state150 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state150 (scanChar)))
      (else (accept 'number))))
  (define (state151 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state150 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state152 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state152 (scanChar)))
      ((#\|) (consumeChar) (state151 (scanChar)))
      (else (accept 'number))))
  (define (state153 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state152 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state154 c)
    (case c
      ((#\+ #\-) (consumeChar) (state153 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state152 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state155 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\#) (consumeChar) (state155 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state154 (scanChar)))
      ((#\|) (consumeChar) (state151 (scanChar)))
      (else (accept 'number))))
  (define (state156 c)
    (case c
      ((#\f) (consumeChar) (state160 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state157 c)
    (case c
      ((#\n) (consumeChar) (state156 (scanChar)))
      (else (accept 'number))))
  (define (state158 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      (else (scannerError errIncompleteToken))))
  (define (state159 c)
    (case c
      ((#\0) (consumeChar) (state158 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state160 c)
    (case c
      ((#\.) (consumeChar) (state159 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state161 c)
    (case c
      ((#\n) (consumeChar) (state160 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state162 c)
    (case c
      ((#\a) (consumeChar) (state161 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state163 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state164 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state164 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state170 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state169 (scanChar)))
      ((#\|) (consumeChar) (state166 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state164 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state165 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state165 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state166 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state165 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state167 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state167 (scanChar)))
      ((#\|) (consumeChar) (state166 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state168 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state167 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state169 c)
    (case c
      ((#\+ #\-) (consumeChar) (state168 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state167 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state170 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state170 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state169 (scanChar)))
      ((#\|) (consumeChar) (state166 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state171 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state171 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state172 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state172 (scanChar)))
      ((#\#) (consumeChar) (state171 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state173 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state172 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state174 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state174 (scanChar)))
      ((#\/) (consumeChar) (state173 (scanChar)))
      ((#\.) (consumeChar) (state170 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state169 (scanChar)))
      ((#\|) (consumeChar) (state166 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state175 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state175 (scanChar)))
      ((#\#) (consumeChar) (state174 (scanChar)))
      ((#\/) (consumeChar) (state173 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state169 (scanChar)))
      ((#\|) (consumeChar) (state166 (scanChar)))
      ((#\.) (consumeChar) (state164 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state176 c)
    (case c
      ((#\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state175 (scanChar)))
      ((#\.) (consumeChar) (state163 (scanChar)))
      ((#\n) (consumeChar) (state162 (scanChar)))
      ((#\i) (consumeChar) (state157 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state177 c)
    (case c
      ((#\f) (consumeChar) (state180 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state178 c)
    (case c
      ((#\n) (consumeChar) (state177 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state179 c)
    (case c
      ((#\0) (consumeChar) (accept 'number))
      (else (scannerError errIncompleteToken))))
  (define (state180 c)
    (case c
      ((#\.) (consumeChar) (state179 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state181 c)
    (case c
      ((#\n) (consumeChar) (state180 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state182 c)
    (case c
      ((#\a) (consumeChar) (state181 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state183 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state184 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state184 c)
    (case c
      ((#\#) (consumeChar) (state190 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state189 (scanChar)))
      ((#\|) (consumeChar) (state186 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state184 (scanChar)))
      (else (accept 'number))))
  (define (state185 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state185 (scanChar)))
      (else (accept 'number))))
  (define (state186 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state185 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state187 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state187 (scanChar)))
      ((#\|) (consumeChar) (state186 (scanChar)))
      (else (accept 'number))))
  (define (state188 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state187 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state189 c)
    (case c
      ((#\+ #\-) (consumeChar) (state188 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state187 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state190 c)
    (case c
      ((#\#) (consumeChar) (state190 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state189 (scanChar)))
      ((#\|) (consumeChar) (state186 (scanChar)))
      (else (accept 'number))))
  (define (state191 c)
    (case c
      ((#\#) (consumeChar) (state191 (scanChar)))
      (else (accept 'number))))
  (define (state192 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state192 (scanChar)))
      ((#\#) (consumeChar) (state191 (scanChar)))
      (else (accept 'number))))
  (define (state193 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state192 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state194 c)
    (case c
      ((#\#) (consumeChar) (state194 (scanChar)))
      ((#\/) (consumeChar) (state193 (scanChar)))
      ((#\.) (consumeChar) (state190 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state189 (scanChar)))
      ((#\|) (consumeChar) (state186 (scanChar)))
      (else (accept 'number))))
  (define (state195 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state195 (scanChar)))
      ((#\#) (consumeChar) (state194 (scanChar)))
      ((#\/) (consumeChar) (state193 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state189 (scanChar)))
      ((#\|) (consumeChar) (state186 (scanChar)))
      ((#\.) (consumeChar) (state184 (scanChar)))
      (else (accept 'number))))
  (define (state196 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state195 (scanChar)))
      ((#\.) (consumeChar) (state183 (scanChar)))
      ((#\n) (consumeChar) (state182 (scanChar)))
      ((#\i) (consumeChar) (state178 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state197 c)
    (case c
      ((#\+ #\-) (consumeChar) (state196 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state195 (scanChar)))
      ((#\.) (consumeChar) (state183 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state198 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state198 (scanChar)))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      (else (accept 'number))))
  (define (state199 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state199 (scanChar)))
      ((#\#) (consumeChar) (state198 (scanChar)))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      (else (accept 'number))))
  (define (state200 c)
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state199 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state201 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\#) (consumeChar) (state201 (scanChar)))
      ((#\/) (consumeChar) (state200 (scanChar)))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\.) (consumeChar) (state155 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state154 (scanChar)))
      ((#\|) (consumeChar) (state151 (scanChar)))
      (else (accept 'number))))
  (define (state202 c)
    (case c
      ((#\i #\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state202 (scanChar)))
      ((#\#) (consumeChar) (state201 (scanChar)))
      ((#\/) (consumeChar) (state200 (scanChar)))
      ((#\@) (consumeChar) (state197 (scanChar)))
      ((#\+ #\-) (consumeChar) (state176 (scanChar)))
      ((#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
       (consumeChar)
       (state154 (scanChar)))
      ((#\|) (consumeChar) (state151 (scanChar)))
      ((#\.) (consumeChar) (state149 (scanChar)))
      (else (accept 'number))))
  (define (state203 c)
    (case c
      ((#\I) (consumeChar) (accept 'number))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state202 (scanChar)))
      ((#\.) (consumeChar) (state148 (scanChar)))
      ((#\n) (consumeChar) (state147 (scanChar)))
      ((#\i) (consumeChar) (state142 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state204 c)
    (case c
      ((#\+ #\-) (consumeChar) (state203 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state140 (scanChar)))
      ((#\.) (consumeChar) (state128 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state205 c)
    (case c
      ((#\i #\I #\e #\E)
       (consumeChar)
       (state204 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state206 c)
    (case c
      ((#\#) (consumeChar) (state205 (scanChar)))
      ((#\+ #\-) (consumeChar) (state203 (scanChar)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (consumeChar)
       (state140 (scanChar)))
      ((#\.) (consumeChar) (state128 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state207 c)
    (case c
      ((#\s) (consumeChar) (accept 'miscflag))
      (else (scannerError errIncompleteToken))))
  (define (state208 c)
    (case c
      ((#\r) (consumeChar) (state207 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state209 c)
    (case c
      ((#\6) (consumeChar) (state208 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state210 c)
    (case c
      ((#\r) (consumeChar) (state209 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state211 c)
    (case c
      ((#\`) (consumeChar) (accept 'quasisyntax))
      ((#\') (consumeChar) (accept 'syntax))
      ((#\() (consumeChar) (accept 'vecstart))
      ((#\t #\T #\f #\F)
       (consumeChar)
       (accept 'boolean))
      ((#\;) (consumeChar) (accept 'commentdatum))
      ((#\|) (consumeChar) (accept 'comment))
      ((#\!) (consumeChar) (state210 (scanChar)))
      ((#\d #\D) (consumeChar) (state206 (scanChar)))
      ((#\i #\I #\e #\E)
       (consumeChar)
       (state127 (scanChar)))
      ((#\b #\B) (consumeChar) (state27 (scanChar)))
      ((#\o #\O) (consumeChar) (state25 (scanChar)))
      ((#\x #\X) (consumeChar) (state23 (scanChar)))
      ((#\\) (consumeChar) (state21 (scanChar)))
      ((#\v) (consumeChar) (state17 (scanChar)))
      ((#\,) (consumeChar) (state14 (scanChar)))
      (else (scannerError errIncompleteToken))))
  (define (state212 c)
    (case c
      (else
       (if ((lambda (c)
              (and (char? c)
                   (not (char=? c (integer->char 10)))))
            c)
           (begin (consumeChar) (state212 (scanChar)))
           (begin
             (set! string_accumulator_length 0)
             (state0 (scanChar)))))))
  (define (state213 c)
    (case c
      (else
       (begin
         (set! string_accumulator_length 0)
         (state0 (scanChar))))))
  (define (state214 c)
    (case c (else (accept 'comment))))
  (define (state215 c)
    (case c (else (accept 'commentdatum))))
  (define (state216 c)
    (case c (else (accept 'miscflag))))
  (define (state217 c)
    (case c (else (accept 'boolean))))
  (define (state218 c)
    (case c (else (accept 'number))))
  (define (state219 c)
    (case c (else (accept 'character))))
  (define (state220 c)
    (case c (else (accept 'vecstart))))
  (define (state221 c)
    (case c (else (accept 'bvecstart))))
  (define (state222 c)
    (case c (else (accept 'syntax))))
  (define (state223 c)
    (case c (else (accept 'quasisyntax))))
  (define (state224 c)
    (case c (else (accept 'unsyntaxsplicing))))
  (define (state225 c)
    (case c (else (accept 'eofobj))))
  (define (state226 c)
    (case c (else (accept 'id))))
  (define (state227 c)
    (case c (else (accept 'string))))
  (define (state228 c)
    (case c (else (accept 'lparen))))
  (define (state229 c)
    (case c (else (accept 'rparen))))
  (define (state230 c)
    (case c (else (accept 'lbracket))))
  (define (state231 c)
    (case c (else (accept 'rbracket))))
  (define (state232 c)
    (case c (else (accept 'quote))))
  (define (state233 c)
    (case c (else (accept 'backquote))))
  (define (state234 c)
    (case c (else (accept 'splicing))))
  (let loop ((c (scanChar)))
    (if (char-whitespace? c)
        (begin
          (consumeChar)
          (set! string_accumulator_length 0)
          (loop (scanChar)))))
  (let ((c (scanChar)))
    (if (char=? c EOF) (accept 'eof) (state0 c))))
