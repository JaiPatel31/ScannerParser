#lang brag

;; Grammar for the mini language
;; ---------------------------------------------
;; program -> stmt-list EOF
;; stmt-list -> stmt stmt-list | ε
;; stmt -> if-stmt | while-stmt | assign-stmt | read-stmt | print-stmt | compound-stmt
;; compound-stmt -> stmt {; stmt}*
;; if-stmt -> if expr comp-op expr then begin stmt-list end {else begin stmt-list end}
;; while-stmt -> while expr comp-op expr begin stmt-list end
;; assign-stmt -> id := expr
;; read-stmt -> read id
;; print-stmt -> print expr
;; expr -> term + expr | term - expr | term comp-op term | term
;; term -> factor * term | factor / term | factor
;; factor -> id | num | (expr)
;; num -> sign nonzero | sign nonzero digit* | sign nonzero digit* . digit digit*
;; sign -> + | - | ε
;; id -> alpha alnum-hy-underscore*
;; comp-op -> = | > | < | >= | <= | <>
;; comment -> /* any text (multi-line allowed) */ (handled in lexer)

program: stmt-list 

stmt-list: stmt stmt-list | ()

stmt: if-stmt
    | while-stmt
    | assign-stmt
    | read-stmt
    | print-stmt
    | compound-stmt

compound-stmt: stmt (";" stmt)*

if-stmt: IF expr comp-op expr THEN BEGIN stmt-list END [ELSE BEGIN stmt-list END]

while-stmt: WHILE expr comp-op expr BEGIN stmt-list END

assign-stmt: ID ASSIGN expr

read-stmt: READ ID

print-stmt: PRINT expr

expr: term PLUS expr
    | term MINUS expr
    | term comp-op term
    | term

term: factor TIMES term
    | factor DIVIDE term
    | factor

factor: ID
      | NUM
      | LPAREN expr RPAREN

num: sign NONZERO
   | sign NONZERO DIGIT*
   | sign NONZERO DIGIT* DOT DIGIT DIGIT*

sign: PLUS
    | MINUS
    | ()

comp-op: EQ
       | GT
       | LT
       | GE
       | LE
       | NE