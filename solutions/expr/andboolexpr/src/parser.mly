%{
open Ast
%}

(* Declaration of lexical tokens *)
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF

(* ELSE is not associative *)
%nonassoc ELSE

(* OR is left associative has the priority over ELSE *)
%left OR

(* AND is left associative and has the priority over OR *)
%left AND

(* NOT is right associative has the highest priority *)
%right NOT

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
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
;
