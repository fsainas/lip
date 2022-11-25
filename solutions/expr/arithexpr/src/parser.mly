%{
open Ast
%}

(* Declaration of lexical tokens *)
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token LPAREN
%token RPAREN
%token EOF

%nonassoc ELSE
%left OR
%left AND
%left NOT
%nonassoc SUCC, PRED, ISZERO

(* parsing starts with a rule named prog,
 and the result of the parsing will be a value
 of type Ast.boolExpr *)
%start <expr> prog

%%

(* The rules section defines the productions of the grammar *)
prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | ZERO { Zero }
  | SUCC; e = expr { Succ(e) }
  | PRED; e = expr { Pred(e) }
  | ISZERO; e = expr { IsZero(e) }
  | LPAREN; e=expr; RPAREN {e}
;
