%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token NOT
%token AND
%token OR
%token ASSIGN
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SKIP
%token <string> ID
%token <int> CONST
%token SEQ
%token EOF

%left SEQ
%nonassoc ELSE
%nonassoc DO
%left OR
%left AND
%left LEQ
%left NOT

%start <cmd> prog (* takes a program which is a command *)

%%

prog:
  | e = cmd; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | x=ID; { Var x }
  | x=CONST; { Const x }
  | LPAREN; e=expr; RPAREN { e }
  | NOT; e=expr; { Not(e) }
  | e1=expr; AND; e2=expr; { And(e1,e2) }
  | e1=expr; OR; e2=expr; { Or(e1,e2) }
  | e1=expr; ADD; e2=expr; { Add(e1,e2) }
  | e1=expr; SUB; e2=expr; { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr; { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr; { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr; { Leq(e1,e2) }
;

cmd:
  | SKIP; { Skip }
  | LPAREN; c=cmd; RPAREN; { c }
  | s=ID; ASSIGN; e=expr; { Assign(s,e) }
  | c1=cmd; SEQ; c2=cmd; { Seq(c1,c2) }
  | IF; e1=expr; THEN; c1=cmd; ELSE; c2=cmd; { If(e1,c1,c2) }
  | WHILE; e=expr; DO; c=cmd; { While(e,c) }
;
