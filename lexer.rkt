#lang racket

(require (only-in srfi/1 fold))

;; Token structure
(struct token (type value line col) #:transparent)

;; Lexer for the Brag mini-language
;; =====================================

(define (lex input)
  "Convert input string into a list of tokens"
  (let loop ([chars (string->list input)]
             [pos 0]
             [line 1]
             [col 1]
             [tokens '()])
    (if (null? chars)
        (reverse (cons (token 'EOF "" line col) tokens))
        (let ([char (car chars)]
              [rest (cdr chars)])
          (cond
            ;; Whitespace
            [(char-whitespace? char)
             (if (char=? char #\newline)
                 (loop rest (+ pos 1) (+ line 1) 1 tokens)
                 (loop rest (+ pos 1) line (+ col 1) tokens))]
            
            ;; Comments: /* ... */
            [(and (char=? char #\/) (not (null? rest)) (char=? (car rest) #\*))
             (let comment-loop ([cs (cddr rest)] [p (+ pos 2)] [l line] [c (+ col 2)])
               (cond
                 [(null? cs) (error "Unterminated comment")]
                 [(and (char=? (car cs) #\*) (not (null? (cdr cs))) (char=? (cadr cs) #\/))
                  (loop (cddr cs) (+ p 2) l (+ c 2) tokens)]
                 [(char=? (car cs) #\newline)
                  (comment-loop (cdr cs) (+ p 1) (+ l 1) 1)]
                 [else
                  (comment-loop (cdr cs) (+ p 1) l (+ c 1))]))]
            
            ;; Multi-char operators
            [(and (char=? char #\:) (not (null? rest)) (char=? (car rest) #\=))
             (loop (cdr rest) (+ pos 2) line (+ col 2)
                   (cons (token 'ASSIGN ":=" line col) tokens))]
            
            [(and (char=? char #\>) (not (null? rest)) (char=? (car rest) #\=))
             (loop (cdr rest) (+ pos 2) line (+ col 2)
                   (cons (token 'GE ">=" line col) tokens))]
            
            [(and (char=? char #\<) (not (null? rest)) (char=? (car rest) #\=))
             (loop (cdr rest) (+ pos 2) line (+ col 2)
                   (cons (token 'LE "<=" line col) tokens))]
            
            [(and (char=? char #\<) (not (null? rest)) (char=? (car rest) #\>))
             (loop (cdr rest) (+ pos 2) line (+ col 2)
                   (cons (token 'NE "<>" line col) tokens))]
            
            ;; Single-char operators and delimiters
            [(char=? char #\+)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'PLUS "+" line col) tokens))]
            
            [(char=? char #\-)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'MINUS "-" line col) tokens))]
            
            [(char=? char #\*)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'TIMES "*" line col) tokens))]
            
            [(char=? char #\/)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'DIVIDE "/" line col) tokens))]
            
            [(char=? char #\=)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'EQ "=" line col) tokens))]
            
            [(char=? char #\>)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'GT ">" line col) tokens))]
            
            [(char=? char #\<)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'LT "<" line col) tokens))]
            
            [(char=? char #\()
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'LPAREN "(" line col) tokens))]
            
            [(char=? char #\))
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'RPAREN ")" line col) tokens))]
            
            [(char=? char #\;)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'SEMI ";" line col) tokens))]
            
            [(char=? char #\.)
             (loop rest (+ pos 1) line (+ col 1)
                   (cons (token 'DOT "." line col) tokens))]
            
            ;; Numbers
            [(char-numeric? char)
             (let num-loop ([cs rest] [num (string char)] [p (+ pos 1)] [c (+ col 1)])
               (cond
                 [(and (not (null? cs)) (char=? (car cs) #\.))
                  ;; Decimal number
                  (let frac-loop ([cs2 (cdr cs)] [num2 (string-append num ".")] [p2 (+ p 1)] [c2 (+ c 1)])
                    (cond
                      [(or (null? cs2) (not (char-numeric? (car cs2))))
                       (loop cs2 p2 line c2
                             (cons (token 'NUM num2 line col) tokens))]
                      [else
                       (frac-loop (cdr cs2) (string-append num2 (string (car cs2))) (+ p2 1) (+ c2 1))]))]
                 [(and (not (null? cs)) (char-numeric? (car cs)))
                  (num-loop (cdr cs) (string-append num (string (car cs))) (+ p 1) (+ c 1))]
                 [else
                  ;; Integer number - no more digits
                  (loop cs p line c
                        (cons (token 'NUM num line col) tokens))]))]
            
            ;; Identifiers and keywords
            [(or (char-alphabetic? char) (char=? char #\_))
             (let id-loop ([cs rest] [id (string char)] [p (+ pos 1)] [c (+ col 1)])
               (cond
                 [(or (null? cs) (not (or (char-numeric? (car cs)) (char-alphabetic? (car cs)) (char=? (car cs) #\_))))
                  (let ([kw (string->keyword id)])
                    (cond
                      [(eq? kw 'if) (loop cs p line c (cons (token 'IF id line col) tokens))]
                      [(eq? kw 'then) (loop cs p line c (cons (token 'THEN id line col) tokens))]
                      [(eq? kw 'else) (loop cs p line c (cons (token 'ELSE id line col) tokens))]
                      [(eq? kw 'begin) (loop cs p line c (cons (token 'BEGIN id line col) tokens))]
                      [(eq? kw 'end) (loop cs p line c (cons (token 'END id line col) tokens))]
                      [(eq? kw 'while) (loop cs p line c (cons (token 'WHILE id line col) tokens))]
                      [(eq? kw 'read) (loop cs p line c (cons (token 'READ id line col) tokens))]
                      [(eq? kw 'print) (loop cs p line c (cons (token 'PRINT id line col) tokens))]
                      [else (loop cs p line c (cons (token 'ID id line col) tokens))]))]
                 [(or (char-numeric? (car cs)) (char-alphabetic? (car cs)) (char=? (car cs) #\_))
                  (id-loop (cdr cs) (string-append id (string (car cs))) (+ p 1) (+ c 1))]
                 [else
                  (loop cs p line c
                        (cons (token 'ID id line col) tokens))]))]
            
            ;; Unknown character
            [else
             (error (format "Unexpected character '~a' at line ~a, col ~a" char line col))])))))

(define (string->keyword str)
  "Convert a string to a keyword symbol if it matches, otherwise return the string"
  (case (string->symbol (string-downcase str))
    [(if) 'if]
    [(then) 'then]
    [(else) 'else]
    [(begin) 'begin]
    [(end) 'end]
    [(while) 'while]
    [(read) 'read]
    [(print) 'print]
    [else str]))

;; Example usage:
(module+ main
  (define test-input
"if x := 10
then begin
  read y
  z := x + y
  print z
end
else begin
  print 0
end")
  
  (define tokens (lex test-input))
  (for-each (lambda (t) (displayln t)) tokens))