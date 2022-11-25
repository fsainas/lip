{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+  (* letter* \ {epsilon} *)

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "not " { NOT }
  | "and" { AND }
  | "or" { OR }
  | "0" { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | "=" { ASSIGN }
  | "let" { LET }
  | "in" { IN }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
