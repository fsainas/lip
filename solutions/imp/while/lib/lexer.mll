{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let number = ['0'-'9']
let const = number+

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
  | "while" { WHILE }
  | "do" { DO }
  | "not " { NOT }
  | "and" { AND }
  | "or" { OR }
  | ":=" { ASSIGN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | ";" { SEQ }
  | "skip" { SKIP }

  | id { ID (Lexing.lexeme lexbuf) }
  | const { CONST (int_of_string (Lexing.lexeme lexbuf)) }

  | eof { EOF }
