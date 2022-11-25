(* Opening the parser module in the header*)
{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf } (* the lexer encounters whitespaces it should just skip it *)
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | eof { EOF }