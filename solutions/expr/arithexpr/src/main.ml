open Ast

type exprval = Bool of bool | Nat of int;;

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

(* string_of_val : exprval -> string *)
let string_of_val = function
    Bool(e) -> string_of_bool e
  | Nat(e) -> string_of_int e
;;

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
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)

  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')

  | And(False,_) -> False
  | And(True,e1) -> e1
  | And(e0,e1) -> let e0' = trace1 e0 in And(e0',e1)

  | Or(False,e1) -> e1
  | Or(True,_) -> True
  | Or(e0,e1) -> let e0' = trace1 e0 in Or(e0',e1)

  | Succ(e) -> let e' = trace1 e in Succ(e')

  | Pred(Zero) -> raise NoRuleApplies
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

(* boolexpr_to_bool : exprval -> bool *)
let boolexpr_to_bool = function 
    Bool(e) -> e 
  | Nat(_) -> failwith "a boolean was expected"
;;

(* natexpr_to_int : exprval -> int *)
let natexpr_to_int = function 
    Nat(e) -> e 
  | Bool(_) -> failwith "a natural number was expected"
;;

(* eval : expr -> exprval *)
(* performs big-step semantics *)
let rec eval = function
    True -> Bool(true)
  | False -> Bool(false)
  | If(e0,e1,e2) -> Bool(
      if boolexpr_to_bool (eval e0) 
      then boolexpr_to_bool (eval e1) 
      else boolexpr_to_bool (eval e2))
  | Not(e) -> Bool(not (boolexpr_to_bool (eval e)))
  | And(e0,e1) -> Bool(boolexpr_to_bool (eval e0) && boolexpr_to_bool (eval e1))
  | Or(e0,e1) -> Bool(boolexpr_to_bool (eval e0) || boolexpr_to_bool (eval e1))
  | Zero -> Nat(0)
  | Succ(e) -> Nat(natexpr_to_int (eval e) + 1)
  | Pred(e) -> if (natexpr_to_int (eval e)) = 0 
      then failwith "The predecessor of 0 does not exist" 
      else Nat(natexpr_to_int (eval e) - 1)
  | IsZero(e) -> Bool(natexpr_to_int (eval e) = 0)
;;
