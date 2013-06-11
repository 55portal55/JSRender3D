
;;; a scheme interpreter of render scene definitions

(define *prompt* #f)

(define number-list '())

(define scheme-interpretor-error? #f)

(define (do-nothing)
  #f)

(define (display-number x)
  (if (number? x)
    (set! number-list (cons x number-list))))

(define (interpretor-error message token)
  (display message)
  (display " ")
  (display token)
  (newline)
  (set! scheme-interpretor-error? #t))
      
(define (simple-min-max f)
  (cadr (assq f '((min simple-min) (max simple-max)))))

(define (binding-in-env var env)
  (if (null? env) ; no more frames
    '()
    (let
      ((b (assq var (car env))))
      (if b
        b
        (binding-in-env var (cdr env))))))

(define (set-variable-value! var val env)
  (let
    ((b (binding-in-env var env)))
      (if (not (null? b))
        (set-cdr! b val)
        (interpretor-error "unbound variable" var))))

(define (define-variable! var val env)
  (let
    ((b (assq var (car env))))
    (if b
      (set-cdr! b val)
      (set-car!
        env
        (cons
          (cons var val)
          (car env))))))

(define (lookup-variable-value var env)
  (let
    ((b (binding-in-env var env)))
    (if (not (null? b))
      (cdr b)
      (interpretor-error "unbound variable" var))))

(define (make-frame variables vals)
  (if (null? variables)
    (if (null? vals)
      '()
      (interpretor-error "too many values supplied" vals))
    (if (symbol? variables)
      (cons
        (cons variables vals)
        '())
      (if (null? vals)
        (interpretor-error "too few values supplied" variables)
        (cons
          (cons (car variables) (car vals))
          (make-frame (cdr variables) (cdr vals)))))))

(define (extend-environment variables vals base-env)
  (cons (make-frame variables vals) base-env))

(define (primitive-procedure? proc)
  (if (and (not (null? proc)) (pair? proc))
    (eq? (car proc) 'primitive)
    #f))

(define (reverse-core x y)
  (cond
    ((null? x) y)
    (else
      (reverse-core
        (cdr x)
        (cons (car x) y)))))

(define (rev x)
  (reverse-core x '()))

(define (append2 x y)
  (cond
    ((null? x) y)
    (else
      (cons
        (car x)
        (append2 (cdr x) y)))))

(define gensym-count 0)

(define (gensym)
  (let*
    ((prefix "g:::")
     (string-number (number->string gensym-count))
     (symbol (string->symbol (string-append prefix string-number))))
  (set! gensym-count (+ gensym-count 1))
  symbol))

(define procedure-alist
  (list

    (list 'car car)
    (list 'cdr cdr)
    (list 'caar caar)
    (list 'cadr cadr)
    (list 'cdar cdar)
    (list 'cddr cddr)
    (list 'not not)
    (list 'null? null?)
    (list 'symbol? symbol?)
    (list 'pair? pair?)
    (list 'list? list?)
    (list 'number? number?)
    (list 'zero? zero?)
    (list 'positive? positive?)
    (list 'negative? negative?)
    (list 'string? string?)
    (list 'make-string make-string)
    (list 'string-length string-length)
    (list 'string-copy string-copy)
    (list 'string->symbol string->symbol)
    (list 'symbol->string symbol->string)
    (list 'string->list string->list)
    (list 'list->string list->string)
    (list 'string->number string->number)
    (list 'number->string number->string)
    (list 'char? char?)
    (list 'char->integer char->integer)
    (list 'integer->char integer->char)
    (list 'char-alphabetic? char-alphabetic?)
    (list 'char-numeric? char-numeric?)
    (list 'char-upcase char-upcase)
    (list 'char-downcase char-downcase)
    (list 'integer? integer?)
    (list 'real? real?)
    (list 'floor floor)
    (list 'ceiling ceiling)
    (list 'truncate truncate)
    (list 'round round)
    (list 'inexact->exact inexact->exact)
    (list 'exact->inexact exact->inexact)
    (list 'abs abs)
    (list 'length length)
    (list 'vector? vector?)
    (list 'make-vector make-vector)
    (list 'vector-length vector-length)
    (list 'list->vector list->vector)
    (list 'boolean? boolean?)
    (list 'reverse reverse)
;   (list 'write write)
;   (list 'write-char write-char)
;   (list 'open-input-file open-input-file)
;   (list 'open-output-file open-output-file)
;   (list 'close-input-port close-input-port)
;   (list 'close-output-port close-output-port)
;   (list 'eof-object? eof-object?)

    (list 'list-tail list-tail)
    (list 'list-ref list-ref)
;   (list 'last-pair last-pair) ; TODO remove
    (list 'eqv? eqv?)
    (list 'memq memq)
    (list 'memv memv)
    (list 'member member)
    (list 'assq assq)
    (list 'assv assv)
    (list 'cons cons)
    (list 'eq? eq?)
    (list 'string-ref string-ref)
;   (list 'string-set! string-set!)
    (list 'string=? string=?)
    (list 'substring substring)
;   (list 'string-fill! string-fill!)
    (list 'char=? char=?)
    (list 'char-numeric? char-numeric?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list 'quotient quotient)
    (list 'remainder remainder)
    (list 'cos cos)
    (list 'sin sin)
    (list 'tan tan)
    (list 'asin asin)
    (list 'acos acos)
    (list 'exp exp)
    (list 'log log)
    (list 'sqrt sqrt)
    (list '= =)
    (list '< <)
    (list '> >)
    (list '<= <=)
    (list '>= >=)
    (list 'equal? equal?)
    (list 'set-car! set-car!)
    (list 'set-cdr! set-cdr!)
    (list 'vector-ref vector-ref)
    (list 'vector-set! vector-set!)
    (list 'vector-fill! vector-fill!)
;   (list 'display display)
;   (list 'newline newline)
;   (list 'read read)
;   (list 'read-char read-char)
;   (list 'save-binary save-binary)
;   (list 'simple-min simple-min)
;   (list 'simple-max simple-max)
   ))

(define (apply-primitive-procedure proc args)
  (let
    ((p (cadr proc)))
    (let ((procedure (assq p procedure-alist)))
      (if procedure
        (apply (cadr procedure) args)
        (cond
          ((eq? p 'display)        (display (car args)))
          ((eq? p 'output)         (display-number (car args)))
          ((eq? p 'output-newline) (do-nothing))
          ((eq? p 'newline)        (newline))
          ((eq? p 'procedure?)
            (let ((arg (car args)))
              (is-procedure? arg)))
          ((eq? p 'apply)          (mc-apply (car args) (cadr args)))
          ((eq? p 'prompt)         (set! *prompt* (not *prompt*)))
          (else
            (interpretor-error "unknown primitive procedure" proc)))))))

(define (make-primitive-procedure-objects primitive-procedures)
  (if (not (null? primitive-procedures))
    (cons
      (cons
        'primitive
        (cons (car primitive-procedures) '()))
      (make-primitive-procedure-objects (cdr primitive-procedures)))
    '()))

(define (setup-environment) 
  (let
    ((primitive-procedures
      '(car cdr caar cadr cdar cddr
        list-ref list-tail
        eqv? memq memv member assq assv
        cons not
        null? eq? symbol? pair? list?
        number? zero? positive? negative?
        string? make-string string-length
        string-ref
;       string-set!
        string=?
        substring string-copy
;       string-fill!
        string->symbol symbol->string
        string->list list->string
        string->number number->string
        char? char=? char-alphabetic? char-numeric?
        char-upcase char-downcase
        char->integer integer->char
        integer? real?
        + - * / quotient remainder
        floor ceiling truncate round
        cos sin tan asin acos
        exp log sqrt
        inexact->exact exact->inexact
        abs length
        = < > <= >=
        equal?
        set-car! set-cdr!
        vector? make-vector vector-length
        vector-ref vector-set!
        vector-fill!
        list->vector vector->list
        boolean? procedure?
        apply reverse
        display newline
        output ; extension
        output-newline ; extension
;       write write-char
;       read read-char
;       open-input-file open-output-file
;       close-input-port close-output-port
;       eof-object?
;       load
;       simple-min simple-max
        prompt)))
    (let
      ((initial-env
        (extend-environment
          primitive-procedures
          (make-primitive-procedure-objects primitive-procedures)
          '())))
      initial-env)))

(define (is-procedure? p)
  (or (procedure? p) (primitive-procedure? p) (compound-procedure? p)))
  
(define (self-evaluating? e)
  (cond
    ((eq? e '#f) #t)
    ((eq? e '#t) #t)
    ((null? e) #t)
    ((number? e) #t)
    ((string? e) #t)
    ((char? e) #t)
    ((is-procedure? e) #t)
    (else #f)))

(define (quoted? e)
  (if (not (or (null? e) (symbol? e)))
    (eq? (car e) 'quote)
    #f))

(define (text-of-quotation e)
  (cadr e))

(define (variable? e)
  (symbol? e))

(define (assignment? e)
  (if (not (null? e))
    (eq? (car e) 'set!)
    #f))

(define (assignment-variable e)
  (cadr e))

(define (assignment-value e)
  (car (cddr e)))

(define (definition? e)
  (if e
    (eq? (car e) 'define)))

(define (definition-variable e)
  (if (variable? (cadr e))
    (cadr e)
    (car (cadr e))))

(define (definition-value e)
  (if (variable? (cadr e))
    (car (cddr e))
    (cons 'lambda
          (cons (cdr (cadr e))
                (cddr e)))))

(define (lambda? e)
  (if (not (null? e))
    (eq? (car e) 'lambda)
    #f))

(define (conditional? e)
  (if (not (null? e))
    (eq? (car e) 'cond)
    #f))

(define (if? e)
  (if (not (null? e))
    (eq? (car e) 'if)
    #f))

(define (let? e)
  (if (not (null? e))
    (eq? (car e) 'let)
    #f))

(define (let*? e)
  (if (not (null? e))
    (eq? (car e) 'let*)
    #f))

(define (letrec? e)
  (if (not (null? e))
    (eq? (car e) 'letrec)
    #f))

(define (begin? e)
  (if (not (null? e))
    (eq? (car e) 'begin)
    #f))

(define (arith-terms? e)
  (if (not (null? e))
    (and (memq (car e) '(+ - * /)) (not (= (length (cdr e)) 2)))
    #f))

(define (min-max? e)
  (if (not (null? e))
    (memq (car e) '(min max))
    #f))

(define (list-? e)
  (if (not (null? e))
    (eq? (car e) 'list)
    #f))

(define (or? e)
  (if (not (null? e))
    (eq? (car e) 'or)
    #f))

(define (and? e)
  (if (not (null? e))
    (eq? (car e) 'and)
    #f))

(define (not? e)
  (if (not (null? e))
    (eq? (car e) 'not)
    #f))

(define (while? e)
  (if (not (null? e))
    (eq? (car e) 'while)
    #f))

(define (case? e)
  (if (not (null? e))
    (eq? (car e) 'case)
    #f))

(define (do? e)
  (if (not (null? e))
    (eq? (car e) 'do)
    #f))

(define (append-? e)
  (if (not (null? e))
    (eq? (car e) 'append)
    #f))

(define (vector-? e)
  (if (not (null? e))
    (eq? (car e) 'vector)
    #f))

(define (string-? e)
  (if (not (null? e))
    (eq? (car e) 'string)
    #f))

(define (load? e)
  (if (not (null? e))
    (eq? (car e) 'load)
    #f))

(define (clauses e)
  (cdr e))

(define (no-clauses? clauses)
  (null? clauses))

(define (first-clause clauses)
  (car clauses))

(define (rest-clauses clauses)
  (cdr clauses))

(define (actions clause)
  (cdr clause))

(define (predicate clause)
  (car clause))

(define (else-clause? clause)
  (eq? (predicate clause) 'else))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (application? e)
  (not (symbol? e)))

(define (operator app)
  (car app))

(define (operands app)
  (cdr app))

(define (make-proc lambda-exp env)
  (list 'procedure lambda-exp env))

(define (compound-procedure? proc)
  (if (and (not (null? proc)) (pair? proc))
    (eq? (car proc) 'procedure)
    #f))

(define (parameters proc)
  (cadr (cadr proc)))

(define (procedure-body proc)
  (cddr (cadr proc)))

(define (procedure-environment proc)
  (car (cddr proc)))

(define (eval-assignment e env)
  (let
    ((new-value (mc-eval (assignment-value e) env)))
    (begin
      (set-variable-value!
        (assignment-variable e)
        new-value
        env)
      new-value)))

(define (eval-definition e env)
  (define-variable!
    (definition-variable e)
    (mc-eval (definition-value e) env)
    env)
  "") ; print out nothing in eval
; '())

(define (eval-sequence exps env)
  (if (not (null? exps)) ; TODO is this test a kludge - had to add it later
    (if (last-exp? exps)
      (mc-eval (car exps) env)
      (begin
        (mc-eval (car exps) env)
        (eval-sequence (cdr exps) env)))))

(define (transform-cond e)
  (if (null? e)
    '#f
    (if (else-clause? (first-clause e))
      (cons
        'begin
        (actions (first-clause e)))
      (cons
        'if
        (cons
          (predicate (first-clause e))
          (cons
            (cons
              'begin 
              (actions (first-clause e)))
            (cons
              (cons
                'cond
                (rest-clauses e))
              '())))))))

(define (eval-cond e env)
  (mc-eval (transform-cond e) env))

(define (optional-else else)
  (if (null? else)
    '()
    (car else)))

(define (eval-if clist env)
  (if (null? clist)
    '()
    (if (mc-eval (first-clause clist) env) 
      (mc-eval (cadr clist) env)
      (mc-eval (optional-else (cddr clist)) env))))

(define (build-let-formals pairs)
  (if (null? pairs)
    '()
    (cons
      (caar pairs)
      (build-let-formals (cdr pairs)))))

(define (build-let-actuals pairs)
  (if (null? pairs)
    '()
    (cons
      (car (cdar pairs))
      (build-let-actuals (cdr pairs)))))

(define (transform-let formals body actuals)
  (cons
    (cons
      'lambda
      (cons formals body))
    actuals))

(define (transform-let-tag tag formals body actuals)
  (cons
    (list
      'letrec
      (cons
        (cons
          tag
          (cons
            (cons
              'lambda
              (cons formals body))
            '()))
         '())
      tag)
    actuals))

(define (eval-let e env)
  (if (symbol? (car e))
    (let
      ((tag (car e))
       (formals (build-let-formals (cadr e)))
       (body (cddr e))
       (actuals (build-let-actuals (cadr e))))
      (mc-eval (transform-let-tag tag formals body actuals) env))
    (let
      ((formals (build-let-formals (car e)))
       (body (cdr e))
       (actuals (build-let-actuals (car e))))
      (mc-eval (transform-let formals body actuals) env))))

(define (transform-let* assignments body)
  (if (not (null? assignments))
    (cons
      'let
      (cons
        (list
          (car assignments))
        (list
          (cons
            'let*
            (cons
              (cdr assignments)
              body)))))
    (cons ; degenerate let*
      (cons
        'lambda
        (cons
          '()
          body))
      '())))

(define (eval-let* e env)
  (let
    ((assignments (car e))
     (body (cdr e)))
    (mc-eval (transform-let* assignments body) env)))

(define (build-letrec-unassigned pairs)
  (if (null? pairs)
    '()
    (cons
      (cons
        (caar pairs)
        (cons '() '()))
      (build-letrec-unassigned (cdr pairs)))))

(define (build-letrec-assigned pairs body)
  (if (null? pairs)
    body
    (cons
      (cons
        'set!
        (car pairs))
      (build-letrec-assigned (cdr pairs) body))))

(define (transform-letrec unassigned assigned)
  (cons
    'let
    (cons
      unassigned
      assigned)))

(define (eval-letrec e env)
  (let
    ((unassigned (build-letrec-unassigned (car e)))
     (assigned (build-letrec-assigned (car e) (cdr e))))
    (mc-eval (transform-letrec unassigned assigned) env)))

(define (list-of-values exps env)
  (if (null? exps)
    '()
    (cons
      (mc-eval (car exps) env)
      (list-of-values (cdr exps) env))))

(define (transform-arith-terms f e)
  (if (= (length e) 2)
    (cons f (rev e))
    (list f (transform-arith-terms f (cdr e)) (car e))))

(define (arith-term-constant f)
  (cadr (assq f '((+ 0) (- 0) (* 1) (/ 1)))))

(define (prepare-arith-terms f e)
  (if (= (length e) 1)
    (rev (cons (arith-term-constant f) e))
    (rev e)))

(define (eval-arith-terms f e env)
  (if (= (length e) 0)
    (mc-eval (arith-term-constant f) env)
    (mc-eval
      (transform-arith-terms f (prepare-arith-terms f e)) env)))

(define (transform-min-max f e)
  (if (= (length e) 2)
    (cons (simple-min-max f) e)
    (list (simple-min-max f) (transform-min-max f (cdr e)) (car e))))

(define (eval-min-max f e env)
  (mc-eval (transform-min-max f e) env))

(define (transform-or e)
  (if (null? e)
    '#f
    (if (null? (cdr e))
      (car e)
      (let
        ((x (gensym)))
        (cons
          'let
           (cons
             (cons
               (cons
                 x
                 (cons
                   (car e)
                   '()))
               '())
             (cons
               (cons
                 'if
                 (cons
                   x
                   (cons
                     x
                     (cons
                       (cons
                         'or
                         (cdr e))
                       '()))))
               '())))))))

(define (eval-or e env)
  (mc-eval (transform-or e) env))

(define (transform-and e)
  (if (null? e)
    '#t
    (if (null? (cdr e))
      (car e)
      (cons
        'if
        (cons
          (car e)
          (cons
            (cons
              'and
              (cdr e))
            (cons
              '#f
              '())))))))

(define (eval-and e env)
  (mc-eval (transform-and e) env))

(define (transform-while test body)
  (let
    ((loop (gensym)))
    (cons
      'letrec
      (cons
        (cons
          (cons
            loop
            (cons
              (cons
                'lambda
                (cons
                  '()
                  (cons
                    (cons
                      'if
                      (cons
                        test
                        (cons
                          (cons
                            'begin
                            (append2
                              body
                              (cons
                                (cons loop '())
                                '())))
                          '())))
                    '())))
              '()))
          '())
        (list
          (list loop))))))

(define (eval-while e env)
  (let
    ((test (car e))
     (body (cdr e)))
    (mc-eval (transform-while test body) env)))

(define (transform-one-case key-name one-case)
  (if (eq? (car one-case) 'else)
    one-case
    (let
      ((selectors (car one-case)))
      (if (= (length selectors) 1)
        (cons
          (cons
            'eqv?
            (cons
              key-name
              (cons
                (cons
                  'quote
                  (cons
                    (car selectors)
                    '()))
                '())))
          (cdr one-case))
        (cons
          (cons
            'memv
            (cons
              key-name
              (cons
                (cons
                  'quote
                  (cons
                    selectors
                    '()))
                '())))
          (cdr one-case))))))

(define (transform-rest-of-case key-name rest-of-case)
  (if (null? rest-of-case)
    '()
    (cons
      (transform-one-case key-name (car rest-of-case))
      (transform-rest-of-case key-name (cdr rest-of-case)))))
       
(define (transform-case key-value body)
  (let
    ((key-name (gensym)))
    (cons 'let
      (cons
        (cons
          (cons
            key-name
            (cons key-value '()))
          '())
        (cons
          (cons
            'cond
            (transform-rest-of-case key-name body))
          '())))))

(define (eval-case e env)
  (let
    ((key-value (car e))
     (body (cdr e)))
    (mc-eval (transform-case key-value body) env)))

(define (transform-do vars inits steps test exprs commands)
  (list
    'letrec
    (list
      (list
        'loop
        (list
          'lambda
           vars
           (list
             'if
             test
             (append2
               '(begin)
               exprs)
             (append2
               '(begin)
               (append2
                 commands
                 (list
                   (append2
                     '(loop)
                     steps))))))))
      (append2
        '(loop)
        inits)))

(define (build-do-vars e)
  (if (null? e)
    '()
    (cons (caar e) (build-do-vars (cdr e)))))

(define (build-do-inits e)
  (if (null? e)
    '()
    (cons (cadr (car e)) (build-do-inits (cdr e)))))

(define (build-do-step-value e)
  (if (null? (cddr (car e)))
    (caar e) ; use var if no step value
    (car (cddr (car e)))))

(define (build-do-steps e)
  (if (null? e)
    '()
    (cons (build-do-step-value e) (build-do-steps (cdr e)))))

(define (eval-do e env)
  (let
    ((vars (build-do-vars (car e)))
     (inits (build-do-inits (car e)))
     (steps (build-do-steps (car e)))
     (test (caar (cdr e)))
     (exprs (cdar (cdr e)))
     (commands (cdr (cdr e))))
    (mc-eval
      (transform-do vars inits steps test exprs commands) env)))

(define (eval-append exps env)
  (let 
    ((l (length exps)))
    (cond
      ((<= l 1) #f)
      ((= l 2)
        (append2
          (mc-eval (car exps) env)
          (mc-eval (cadr exps) env)))
      (else
        (append2
          (mc-eval (car exps) env)
          (eval-append (cdr exps) env))))))

(define (eval-vector e env)
  (mc-eval
    (cons 'list->vector
      (cons
        (cons
          'quote
          (cons
            (list-of-values e env)
            '()))
        '()))
    env))

(define (eval-string e env)
  (mc-eval
    (cons 'list->string
      (cons
        (cons
          'quote
          (cons
            (list-of-values e env)
            '()))
        '()))
    env))

(define (mc-eval e env)
  (cond
    ((self-evaluating? e) e)
    ((quoted? e) (text-of-quotation e))
    ((variable? e) (lookup-variable-value e env))
    ((definition? e) (eval-definition e env))
    ((assignment? e) (eval-assignment e env))
    ((lambda? e) (make-proc e env))
    ((conditional? e) (eval-cond (clauses e) env))
    ((if? e) (eval-if (cdr e) env))
    ((let? e) (eval-let (cdr e) env))
    ((let*? e) (eval-let* (cdr e) env))
    ((letrec? e) (eval-letrec (cdr e) env))
    ((begin? e) (eval-sequence (cdr e) env))
    ((list-? e) (list-of-values (cdr e) env))
    ((arith-terms? e) (eval-arith-terms (car e) (cdr e) env))
    ((min-max? e) (eval-min-max (car e) (cdr e) env))
    ((or? e) (eval-or (cdr e) env))
    ((and? e) (eval-and (cdr e) env))
    ((while? e) (eval-while (cdr e) env))
    ((case? e) (eval-case (cdr e) env))
    ((do? e) (eval-do (cdr e) env))
    ((append-? e) (eval-append (cdr e) env))
    ((vector-? e) (eval-vector (cdr e) env))
    ((string-? e) (eval-string (cdr e) env))
;    ((load? e) (eval-load (cdr e) env))
    ((application? e)
      (mc-apply (mc-eval (operator e) env)
                (list-of-values (operands e) env)))
    (else
      (interpretor-error "unknown expression type -- eval" e))))

(define (mc-apply procedure arguments)
  (cond
    ((primitive-procedure? procedure)
      (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (parameters procedure)
          arguments
          (procedure-environment procedure))))
    (else
      (interpretor-error "unknown procedure type -- apply " procedure))))

(define (user-print obj)
; (if (compound-procedure? obj)
;   (display
;     (cons
;       'compound-procedure
;       (cons
;         (parameters obj)
;         (cons
;           (procedure-body obj)
;           (cons
;             '(procedure-env)
;             '())))))
  (if (not (compound-procedure? obj))
    (display-number obj)))

;(define (eval-load e env)
;  (let
;    ((port (open-input-file (car e))))
;    (let loop ((input (read port)))
;      (if (not (eof-object? input))
;        (begin
;          (mc-eval input env)
;          (loop (read port)))))))

(define (driver-loop env)
; (if *prompt*
;   (display "> "))
  (let
    ((input (readx)))
    (if (not (eof-objectx? input))
      (begin
        (user-print
          (mc-eval input env))
        (driver-loop env)))))

(define (scheme) 
  (set! EOF #f)
  (driver-loop (setup-environment)))
