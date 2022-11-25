open Ast

(* string_of_expr : expr -> string *)
let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Zero -> "0"
  | Succ e -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred e -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero e -> "IsZero(" ^ (string_of_expr e) ^ ")"
;;

(* string_of_val : expr -> string *)
let string_of_val e = string_of_int e;;

(* parse : string -> expr *)
(* parses a string iff corresponds to as syntactically correct expr *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies

(* is_nv : exprval -> bool *)
let rec is_nv = function 
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false
;;
  
(* trace1 : expr -> expr *)
(* performs a single step of small-step semantics *)
let rec trace1 = function
    True -> Succ(Zero)
  | False -> Zero
  | If(Succ(Zero),e1,_) -> e1
  | If(Zero,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)

  | Not(Zero) -> True
  | Not(Succ(Zero)) -> False
  | Not(e) -> let e' = trace1 e in Not(e')

  | And(Zero,_) -> False
  | And(Succ(Zero),e) -> Not(IsZero(e))
  | And(e0,e1) -> let e0' = trace1 e0 in And(e0',e1)

  | Or(Zero,e) -> Not(IsZero(e))
  | Or(Succ(Zero),_) -> True
  | Or(e0,e1) -> let e0' = trace1 e0 in Or(e0',e1)

  | Succ(e) -> let e' = trace1 e in Succ(e')

  | Pred(Zero) -> Zero
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')

  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')

  | _ -> raise NoRuleApplies
;;

(* trace : expr -> expr list *)
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

let rec eval = function
    True -> 1
  | False -> 0
  | If(e0,e1,e2) -> if (eval e0) != 0 then eval e1 else eval e2
  | Not(e) -> if (eval e) = 0 then 1 else 0
  | And(e0,e1) -> if ((eval e0) != 0 && (eval e1) != 0) then 1 else 0
  | Or(e0,e1) -> if ((eval e0) != 0 || (eval e1) != 0) then 1 else 0
  | Zero -> 0
  | Succ(e) -> (eval e) + 1
  | Pred(e) -> if (eval e) = 0 then 0 else (eval e) - 1
  | IsZero(e) -> if (eval e) = 0 then 1 else 0
;;
