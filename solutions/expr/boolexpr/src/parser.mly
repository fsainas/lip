%{ (* header *)
open Ast
%}

(* Declaration of lexical tokens *)
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF

(* parsing starts with a rule named prog,
 and the result of the parsing will be a value
 of type Ast.boolExpr *)
%start <boolExpr> prog
%%

(* The rules section defines the productions of the grammar *)
prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
;
