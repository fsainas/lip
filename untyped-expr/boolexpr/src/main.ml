open Ast

let parse (s : string) : boolExp =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval = function
    True -> true
  | False -> false
  | If(e0,e1,e2) -> (match eval e0 with
                      true -> eval e1
                    | false -> eval e2)
;;

let interp (s : string) : bool =
  let e = parse s in eval e;;
