(define EOF-CHAR (integer->char 0))

(define EOF #f)

(define (eof-objectx? ch)
  EOF)

(define line-number 1)

(define (read-charx)
  (if EOF
    EOF-CHAR
    (let
      ((ch (read-char)))
      (if (eof-object? ch)
        (begin
          (set! ch EOF-CHAR)
          (set! EOF #t))
        (if (char=? ch #\newline)
          (set! line-number (+ line-number 1))))
      ch)))

(define readx
  (lambda ()
    (letrec (
      (token-type '())
      (ch #\space)
      (previous-ch #\space)
      (TAB-CHAR (integer->char 9))
      (CARRIAGE-RETURN (integer->char 13))
      (white-space (list #\space #\newline TAB-CHAR CARRIAGE-RETURN))
      (readin
        (lambda ()
        (letrec (
          (get-token
            (lambda ()
              (letrec (
                (token '())
                (token-list '())
                (token-new '())
                (token-tail '())
                (char-alpha-numeric?
                  (lambda ()
                    (or (char-alphabetic? ch) (char-numeric? ch))))
                (skip-past-alpha
                  (lambda ()
                    (let loop ()
                      (if (char-alpha-numeric?)
                        (begin
                          (get-ch)
                          (skip-past-alpha)
                          (loop))))))
                (get-ch
                  (lambda ()
                    (set! ch (read-charx))
                    (if (char=? ch #\newline)
                      (set! ch #\space))))
                (get-ch-no-newline
                  (lambda ()
                    (set! ch (read-charx))))
                (skip-line
                  (lambda ()
                    (if (char=? ch #\newline)
                      (get-ch)
                      (begin
                        (get-ch-no-newline)
                        (skip-line)))))
                (skip-blanks
                  (lambda ()
                    (cond
                      ((eof-objectx? ch))
                      ((char=? ch #\;)
                        (skip-line)
                        (skip-blanks))
                      ((memq ch white-space)
                        (get-ch)
                        (skip-blanks))
                      (else '()))))
                (replace-last-ch
                  (lambda ()
                    (set-car! token-tail ch)
                    (set! previous-ch ch)
                    (get-ch)))
                (append-to-list
                  (lambda ()
                    (set! token-new (list ch))
                    (if (not (null? token-list))
                      (begin
                        (set-cdr! token-tail token-new)
                        (set! token-tail (cdr token-tail)))
                      (begin
                        (set! token-list token-new)
                        (set! token-tail token-new)))
                    (set! previous-ch ch)
                    (get-ch)))
                (get-vector
                  (lambda ()
                    (get-ch)
                    (if (member ch '(#\t #\f)) ; boolean true or false
                      (begin
                        (set! token-type 'BOOLEAN-TOK)
                        (if (char=? ch #\t)
                          (set! token #t)
                          (set! token #f))
                        (get-ch))
                      (if (char=? ch #\\)
                        (begin
                          (set! token-type 'CHAR-TOK)
                          (get-ch)
                          (set! token ch)
                          (let
                            ((first-ch ch))
                            (get-ch)
                            (case first-ch
                              ((#\s)
                                (if (char=? ch #\p)
                                  (begin
                                    (set! token #\space)
                                    (skip-past-alpha))))
                              ((#\n)
                                (if (char=? ch #\e)
                                  (begin
                                    (set! token #\newline)
                                    (skip-past-alpha)))))))
                        (set! token-type 'VECTOR-TOK)))))
                (get-string
                  (lambda ()
                    (get-ch) ; after open double quote
                    (let loop ()
                      (if (not (or (char=? ch #\") (eof-objectx? ch)))
                        (begin
                          (append-to-list)
                          (if (char=? previous-ch #\\) ; escape char
                            (replace-last-ch))
                          (loop))))
                    (get-ch) ; after close double quote
                    (set! token-type 'STRING-TOK)
                    (set! token (list->string token-list))))
                (number-token?
                  (lambda (token)
                    (letrec (
                      (ch #\space)
                      (END-OF-TOKEN (integer->char 0))
                      (return #f)
                      (next-ch
                        (lambda ()
                          (if (null? token)
                            (set! ch END-OF-TOKEN)
                            (begin
                              (set! ch (car token))
                              (set! token (cdr token))))))
                      (state-0
                        (lambda ()
                          (next-ch)
                          (cond
                            ((char=? ch #\-) (state-1))
                            ((char-numeric? ch) (state-2))
                            ((char=? ch #\.) (state-3))
                            (else '()))))
                      (state-1
                        (lambda ()
                          (next-ch)
                          (cond
                            ((char=? ch #\.) (state-3))
                            ((char-numeric? ch) (state-2))
                            (else '()))))
                      (state-2
                        (lambda ()
                          (next-ch)
                          (cond
                            ((char=? ch #\.) (state-3))
                            ((char-numeric? ch) (state-2))
                            ((or (char=? ch #\e) (char=? ch #\E)) (state-4))
                            ((char=? ch END-OF-TOKEN) (state-7))
                            (else '()))))
                      (state-3
                        (lambda ()
                          (next-ch)
                          (cond
                            ((or (char=? ch #\e) (char=? ch #\E)) (state-4))
                            ((char-numeric? ch) (state-3))
                            ((char=? ch END-OF-TOKEN) (state-7))
                            (else '()))))
                      (state-4
                        (lambda ()
                          (next-ch)
                          (cond
                            ((or (char=? ch #\+) (char=? ch #\-)) (state-5))
                            ((char-numeric? ch) (state-6))
                            (else '()))))
                      (state-5
                        (lambda ()
                          (next-ch)
                          (if (char-numeric? ch) (state-6))))
                      (state-6
                        (lambda ()
                          (next-ch)
                          (cond
                            ((char=? ch END-OF-TOKEN) (state-7))
                            ((char-numeric? ch) (state-6))
                            (else '()))))
                      (state-7
                        (lambda ()
                          (set! return #t))))
                      (state-0)
                      return)))
                (get-symbol
                  (lambda ()
                    (let loop ()
                      (if (not (or (memq ch white-space)
                                   (memq ch '(#\( #\)))
                                   (eof-objectx? ch)))
                        (begin
                          (append-to-list)
                          (loop))))
                    (if (equal? token-list '(#\.))
                      (set! token-type 'DOT-TOK)
                      (begin
                        (set! token-type 'SYMBOL-TOK)
                        (if (number-token? token-list) 
                          (set! token
                            (string->number (list->string token-list)))
                          (set! token
                            (string->symbol (list->string token-list)))))))))
                (skip-blanks)
                (if (eof-objectx? ch)
                  (begin
                    (set! token-type 'EOF-TOK)
                    (set! token ch))
                  (let
                    ((match (assv ch '(
                       (#\( OPAR-TOK) (#\) CPAR-TOK)
                       (#\' TIC-TOK) (#\# VECTOR-TOK) (#\" STRING-TOK)))))
                    (if match
                      (case (cadr match)
                        ((OPAR-TOK CPAR-TOK TIC-TOK)
                          (set! token-type (cadr match))
                          (set! token ch)
                          (get-ch))
                        ((VECTOR-TOK)
                          (get-vector))
                        ((STRING-TOK)
                          (get-string)))
                      (get-symbol))))
;(display "get-token: ") (write token) (newline)
                token)))
          (exit-bad-sexpr
            (lambda (object)
              (display "bad s-expression: ") (display object)
              (display " on line ") (display line-number)
              (newline)))
          (read-dot-token
            (lambda ()
              (let
                ((token (get-token)))
                (if (not (or (eq? token-type 'SYMBOL-TOK)
                             (eq? token-type 'STRING-TOK)))
                  (exit-bad-sexpr token))
                (get-token)
                (if (not (eq? token-type 'CPAR-TOK))
                  (exit-bad-sexpr "expecting close paren"))
                token)))
          (read-list
            (lambda ()
              (let
                ((lst '())
                 (tail '())
                 (new '()))
                (let loop ((token (get-token)))
                  (if (not (or (eq? token-type 'CPAR-TOK)
                               (eq? token-type 'EOF-TOK)))
                    (begin
                      (case token-type
                        ((OPAR-TOK)
                          (set! new (list (read-list))))
                        ((DOT-TOK)
                          (set-cdr! tail (read-dot-token))
                          (set! token-type 'WAS-DOT-TOK))
                        ((EOF-TOK)
                          (exit-bad-sexpr "unexpected end of file"))
                        ((TIC-TOK)
                          (set! new (list (cons 'quote (list (readin))))))
                        ((VECTOR-TOK)
                          (set! new (list (read-vector))))
                        (else
                          (set! new (list token))))
                      (if (not (eq? token-type 'WAS-DOT-TOK))
                        (begin
                          (if (not (null? lst))
                            (begin
                              (set-cdr! tail new)
                              (set! tail (cdr tail)))
                            (begin
                              (set! lst new)
                              (set! tail new)))
                          (loop (get-token)))
                        (begin
                          (set! token-type 'CPAR-TOK))))))
                lst)))
          (read-vector
            (lambda ()
              (get-token)
              (if (eq? token-type 'OPAR-TOK)
                (list->vector (read-list))
                (exit-bad-sexpr "expecting open paren")))))
        (let
          ((token (get-token)))
          (case token-type
            ((OPAR-TOK)
              (read-list))
            ((TIC-TOK)
              (cons 'quote (list (readin))))
            ((VECTOR-TOK)
              (read-vector))
            ((CPAR-TOK)
              (exit-bad-sexpr "unexpected close paren"))
            (else
              token)))))))
  (readin))))
