(* Opening the parser module in the header *)
{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  (* when the lexer encounters whitespaces it should just skip it *)
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }    
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | eof { EOF }
