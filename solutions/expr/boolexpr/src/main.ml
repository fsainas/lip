open Ast

(* string_of_boolexpr : boolExpr -> string *)
let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
;;

(* parse : string -> boolExpr *)
(* parses a string iff corresponds to as syntactically correct boolExpr *)
let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
  
(* trace1 : boolExpr -> boolExpr *)
(* performs a single step of small-step semantics *)
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | _ -> raise NoRuleApplies
;;

(* trace1 : boolExpr -> boolExpr list *)
(* performs all steps of small-step semantics 
   and returns the list of them *)
let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

(* eval : boolExpr -> bool *)
(* performs big-step semantics *)
let rec eval = function
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if eval e0 then eval e1 else eval e2
;;
