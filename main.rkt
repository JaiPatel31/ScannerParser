#lang racket

;; ============================================================
;; Recursive Descent Parser for the Simple Grammar
;; Author: Jai Patel
;; ============================================================

(require racket/pretty)

;; -----------------------------
;; GLOBAL STATE
;; -----------------------------
(define tokens '())
(define current-line 1)

;; Track whether a statement has ended (used for newline rule)
(define last-token-ended-statement? #t)

(define (tok-type t)
  (if (list? t) (car t) t))

(define (note-token-ending! tok)
  (define t (tok-type tok))
  (cond
    ;; Tokens that clearly indicate continuation of an expression
    [(member t '(PLUS MINUS MUL DIV COMP-OP ASSIGN LPAREN))
     (set! last-token-ended-statement? #f)]
    ;; Tokens that mark end of a logical expression or statement
    [(member t '(ID NUM RPAREN END SEMI EOF THEN ELSE BEGIN PRINT READ WHILE IF))
     (set! last-token-ended-statement? #t)]
    [else
     (set! last-token-ended-statement? #t)]))

;; -----------------------------
;; TOKENIZER
;; -----------------------------
(define (tokenize input)
  ;; reset state for each parse
  (set! current-line 1)
  (set! last-token-ended-statement? #t)

  ;; whitespace skipper (now detects mid-expression newlines)
  (define (skip-space s)
    (let loop ([str s])
      (cond
        [(regexp-match #px"^[ \t]+" str)
         (loop (regexp-replace #px"^[ \t]+" str ""))]

        ;; newline handling: error if inside an expression
        [(regexp-match #px"^\r?\n" str)
         (if (not last-token-ended-statement?)
             (error (format "Syntax error on line ~a: line break inside expression" current-line))
             (begin
               (set! current-line (add1 current-line))
               (loop (regexp-replace #px"^\r?\n" str ""))))]

        [else str])))

  ;; remove /* ... */ comments, disallow nested or stray closers
(define (skip-comments s)
  (cond
    ;; Opening /* ... */ comment
    [(regexp-match #px"^/\\*" s)
     (define end-pos (regexp-match-positions #px"\\*/" s))
     (if end-pos
         (let* ([pair (car end-pos)]
                [end (cdr pair)]
                [rest (substring s end)])
           (skip-comments rest))
         (syntax-error "comment close '*/'" "EOF"))]  ; use helper

    ;; Stray */ without matching open
    [(regexp-match #px"^\\*/" s)
     (syntax-error "no prior comment open" "'*/'")]

    [else s]))


  ;; recursively clean spaces and comments
  (define (clean s)
    (let loop ([str s])
      (define no-space (skip-space str))
      (define no-comment (skip-comments no-space))
      (if (not (equal? str no-comment))
          (loop no-comment)
          no-comment)))

  ;; single token extraction
  (define (next-token s)
    (cond
      [(string=? s "") (values 'EOF "")]
      ;; keywords
      [(regexp-match #px"^if\\b" s) => (λ(_) (values 'IF (regexp-replace #px"^if\\b" s "")))]
      [(regexp-match #px"^then\\b" s) => (λ(_) (values 'THEN (regexp-replace #px"^then\\b" s "")))]
      [(regexp-match #px"^else\\b" s) => (λ(_) (values 'ELSE (regexp-replace #px"^else\\b" s "")))]
      [(regexp-match #px"^while\\b" s) => (λ(_) (values 'WHILE (regexp-replace #px"^while\\b" s "")))]
      [(regexp-match #px"^begin\\b" s) => (λ(_) (values 'BEGIN (regexp-replace #px"^begin\\b" s "")))]
      [(regexp-match #px"^end\\b" s) => (λ(_) (values 'END (regexp-replace #px"^end\\b" s "")))]
      [(regexp-match #px"^read\\b" s) => (λ(_) (values 'READ (regexp-replace #px"^read\\b" s "")))]
      [(regexp-match #px"^print\\b" s) => (λ(_) (values 'PRINT (regexp-replace #px"^print\\b" s "")))]

      ;; comparison operators
      [(regexp-match #px"^>=" s) => (λ(m) (values (list 'COMP-OP (car m)) (substring s (string-length (car m)))))]
      [(regexp-match #px"^<=" s) => (λ(m) (values (list 'COMP-OP (car m)) (substring s (string-length (car m)))))]
      [(regexp-match #px"^<>" s) => (λ(m) (values (list 'COMP-OP (car m)) (substring s (string-length (car m)))))]
      [(regexp-match #px"^=" s)  => (λ(m) (values (list 'COMP-OP (car m)) (substring s (string-length (car m)))))]
      [(regexp-match #px"^>" s)  => (λ(m) (values (list 'COMP-OP (car m)) (substring s (string-length (car m)))))]
      [(regexp-match #px"^<" s)  => (λ(m) (values (list 'COMP-OP (car m)) (substring s (string-length (car m)))))]
      
      ;; assignment
      [(regexp-match #px"^:=" s) => (λ(_) (values 'ASSIGN (regexp-replace #px"^:=" s "")))]

;; numbers (integer, float, or scientific notation)
[(regexp-match #px"^[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?" s)
 => (λ(m)
      (values (list 'NUM (car m))
              (substring s (string-length (car m)))))]

      ;; identifiers
      [(regexp-match #px"^[A-Za-z][A-Za-z0-9_-]*" s)
       => (λ(m) (values (list 'ID (car m))
                        (substring s (string-length (car m)))))]

      ;; single-character tokens
      [(char=? (string-ref s 0) #\+) (values 'PLUS (substring s 1))]
      [(char=? (string-ref s 0) #\-) (values 'MINUS (substring s 1))]
      [(char=? (string-ref s 0) #\*) (values 'MUL (substring s 1))]
      [(char=? (string-ref s 0) #\/) (values 'DIV (substring s 1))]
      [(char=? (string-ref s 0) #\() (values 'LPAREN (substring s 1))]
      [(char=? (string-ref s 0) #\)) (values 'RPAREN (substring s 1))]
      [(char=? (string-ref s 0) #\;) (values 'SEMI (substring s 1))]
      [else (error (format "Unknown token '~a' near line ~a"
                           (substring s 0 1) current-line))]))

  ;; tokenize full input
  (define (loop s acc)
    (define cleaned (clean s))
    (define-values (tok rest) (next-token cleaned))
    (note-token-ending! tok)
    (if (eq? tok 'EOF)
        (reverse (cons 'EOF acc))
        (loop rest (cons tok acc))))
  (loop input '()))

;; -----------------------------
;; PARSER UTILITIES
;; -----------------------------
(define (peek) (car tokens))
(define (advance!) (set! tokens (cdr tokens)))

(define (syntax-error expected found)
  (error (format "Syntax error on line ~a: expected ~a but got ~a"
                 current-line expected found)))

(define (consume expected)
  (if (equal? (peek) expected)
      (advance!)
      (syntax-error expected (peek))))

(define (consume-comp-op)
  (define tok (peek))
  (if (and (list? tok) (eq? (car tok) 'COMP-OP))
      (begin (advance!) tok)
      (syntax-error "comparison operator" tok)))

;; -----------------------------
;; GRAMMAR RULES
;; -----------------------------
(define (parse-program)
  (define tree (parse-stmt-list))
  (consume 'EOF)
  (list 'program tree))

(define (parse-stmt-list)
  (cond
    [(member (peek) '(IF WHILE READ PRINT BEGIN))
     (define s (parse-stmt))
     (define stmts (parse-stmt-rest))
     (list 'stmt-list s stmts)]
    [(and (list? (peek)) (eq? (car (peek)) 'ID))
     (define s (parse-stmt))
     (define stmts (parse-stmt-rest))
     (list 'stmt-list s stmts)]
    [else '()]))

(define (parse-stmt-rest)
  (cond
    [(equal? (peek) 'SEMI)
     (consume 'SEMI)
     (parse-stmt-list)]
    [(member (peek) '(IF WHILE READ PRINT BEGIN))
     (parse-stmt-list)]
    [(and (list? (peek)) (eq? (car (peek)) 'ID))
     (parse-stmt-list)]
    [else '()]))

(define (parse-stmt)
  (match (peek)
    ['IF (parse-if-stmt)]
    ['WHILE (parse-while-stmt)]
    ['READ (parse-read-stmt)]
    ['PRINT (parse-print-stmt)]
    ['BEGIN (parse-compound-stmt)]
    [(list 'ID _) (parse-assign-stmt)]
    [else (syntax-error "statement" (peek))]))

(define (parse-if-stmt)
  (consume 'IF)
  (define cond (parse-expr))
  (consume 'THEN)
  (consume 'BEGIN)
  (define then-part (parse-stmt-list))
  (consume 'END)
  (define else-part
    (if (equal? (peek) 'ELSE)
        (begin
          (consume 'ELSE)
          (consume 'BEGIN)
          (let ([ep (parse-stmt-list)])
            (consume 'END)
            ep))
        '()))
  (list 'if cond then-part else-part))

(define (parse-while-stmt)
  (consume 'WHILE)
  (define cond (parse-expr))
  (consume 'BEGIN)
  (define body (parse-stmt-list))
  (consume 'END)
  (list 'while cond body))

(define (parse-assign-stmt)
  (define id (peek)) (advance!)
  (consume 'ASSIGN)
  (define e (parse-expr))
  (list 'assign id e))

(define (parse-read-stmt)
  (consume 'READ)
  (define id (peek)) (advance!)
  (list 'read id))

(define (parse-print-stmt)
  (consume 'PRINT)
  (define e (parse-expr))
  (list 'print e))

(define (parse-compound-stmt)
  (consume 'BEGIN)
  (define body (parse-stmt-list))
  (consume 'END)
  (list 'compound body))

(define (parse-expr)
  (define t (parse-term))
  (cond
    [(equal? (peek) 'PLUS) (consume 'PLUS) (list '+ t (parse-expr))]
    [(equal? (peek) 'MINUS) (consume 'MINUS) (list '- t (parse-expr))]
    [(and (list? (peek)) (eq? (car (peek)) 'COMP-OP))
     (define op (consume-comp-op))
     (define t2 (parse-term))
     (list 'comp t op t2)]
    [else t]))

(define (parse-term)
  (define f (parse-factor))
  (cond
    [(equal? (peek) 'MUL) (consume 'MUL) (list '* f (parse-term))]
    [(equal? (peek) 'DIV) (consume 'DIV) (list '/ f (parse-term))]
    [else f]))

(define (parse-factor)
  (cond
    [(equal? (peek) 'PLUS)
     (consume 'PLUS)
     (parse-factor)]
    [(equal? (peek) 'MINUS)
     (consume 'MINUS)
     (list 'neg (parse-factor))]
    [(and (list? (peek)) (eq? (car (peek)) 'ID))
     (define v (peek)) (advance!) v]
    [(and (list? (peek)) (eq? (car (peek)) 'NUM))
     (define v (peek)) (advance!) v]
    [(equal? (peek) 'LPAREN)
     (consume 'LPAREN)
     (define e (parse-expr))
     (consume 'RPAREN)
     e]
    [else (syntax-error "factor" (peek))]))

;; -----------------------------
;; MAIN ENTRY POINT
;; -----------------------------
(define (parse filename)
  (with-handlers ([exn:fail?
                   (λ (ex)
                     ;; print error message only
                     (printf "~a\n" (exn-message ex))
                     (void))])
    (set! current-line 1)
    (set! last-token-ended-statement? #t)
    (define in (open-input-file filename))
    (define src (port->string in))
    (close-input-port in)
    (define toks (tokenize src))
    (set! tokens toks)
    (define result (parse-program))
    ;; if we got here, parsing succeeded
    (printf "Accept\n")
    (pretty-print result)
    (void)))


;; Example
(parse "bad-comment-1.txt")
(parse "bad-comment-2.txt")
(parse "Correct01.txt")
(parse "Correct02.txt")
(parse "Correct03.txt")
(parse "Correct04.txt")
(parse "line-break-midstatement.txt")
(parse "Mismatched-end.txt")
(parse "Mismatched-parens.txt")
(parse "scientific-notation.txt")
