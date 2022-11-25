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
  | Var x -> "Var(" ^ x ^ ")"
  | Let(x,e0,e1) -> "Let " ^ x ^ " = " ^ (string_of_expr e0) ^ " in " ^ (string_of_expr e1)
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
exception UnboundValue

(* is_nv : exprval -> bool *)
let rec is_nv = function 
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false
;;
 
(* is_val : expr -> bool *)
(* returns true if an expr is a value *)
let rec is_val = function
    True | False | Zero -> true
  | Succ e -> is_val e
  | _ -> false
;;

(* trace1 : (string -> expr) -> expr -> (string -> expr) * expr *)
(*  *)
let rec trace1 rho = function
    If(True,e,_) -> (rho,e)
  | If(False,_,e) -> (rho,e)
  | If(e0,e1,e2) -> let (_,e0') = trace1 rho e0 in (rho,If(e0',e1,e2))

  | Not(True) -> (rho,False)
  | Not(False) -> (rho,True)
  | Not(e) -> let (_,e') = trace1 rho e in (rho,Not(e'))

  | And(False,_) -> (rho,False)
  | And(True,e) -> (rho,e)
  | And(e0,e1) -> let (_,e0') = trace1 rho e0 in (rho,And(e0',e1))

  | Or(False,e) -> (rho,e)
  | Or(True,_) -> (rho,True)
  | Or(e0,e1) -> let (_,e0') = trace1 rho e0 in (rho,Or(e0',e1))

  | Succ(e) -> let (_,e') = trace1 rho e in (rho,Succ(e'))

  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> (rho,e)
  | Pred(e) -> let (_,e') = trace1 rho e in (rho,Pred(e'))

  | IsZero(Zero) -> (rho,True)
  | IsZero(Succ(e)) when is_nv e -> (rho,False)
  | IsZero(e) -> let (_,e') = trace1 rho e in (rho,IsZero(e'))

  | Var(x) -> (rho,rho x)
  (* performs substitution iff e0 is an exprval *)
  | Let(x,e0,e1) when is_val e0 -> 
          let (_,e1') = trace1 (fun x' -> if x = x' then e0 else rho x') e1 in (rho,e1')
  | Let(x,e0,e1) -> let (_,e0') = trace1 rho e0 in (rho,Let(x,e0',e1))

  | _ -> raise NoRuleApplies
;;

(* trace : expr -> expr list *)
(* performs all steps of small-step semantics 
   and returns the list of them *)
let trace e = 
    let rec trac rho expr = 
        try
    let (rho',e') = trace1 rho expr
    in expr::(trac rho' e')
  with 
    NoRuleApplies -> [expr]
  | UnboundValue -> [expr]
    in 
    trac (fun _ -> raise UnboundValue) e
;;

let rec print_trace = function
    [] -> print_newline()
  | [x] -> print_endline (string_of_expr x)
  | x::l -> print_endline (string_of_expr x); print_string " -> " ; print_trace l
;;

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

(* boolexpr_to_expr : exprval -> expr *)
let boolexpr_to_expr = function 
    Bool(e) -> e 
  | _ -> failwith "a boolean was expected"
;;

(* natexpr_to_expr : exprval -> expr *)
let natexpr_to_expr = function 
    Nat(e) -> e 
  | _ -> failwith "a natural number was expected"
;;


(* eval : expr -> exprval *)
(* performs big-step semantics *)
let eval expr = 

    let rec eval1 e rho = match e with
        True -> Bool true
      | False -> Bool false
      | If(e0,e1,e2) -> Bool(
          if boolexpr_to_expr (eval1 e0 rho) 
          then boolexpr_to_expr (eval1 e1 rho) 
          else boolexpr_to_expr (eval1 e2 rho) )
      | Not(e0) -> Bool(not (boolexpr_to_expr (eval1 e0 rho)))
      | And(e0,e1) -> Bool(boolexpr_to_expr (eval1 e0 rho) && boolexpr_to_expr (eval1 e1 rho))
      | Or(e0,e1) -> Bool(boolexpr_to_expr (eval1 e0 rho) || boolexpr_to_expr (eval1 e1 rho))
      | Zero -> Nat(0)
      | Succ(e0) -> Nat(natexpr_to_expr (eval1 e0 rho) + 1)
      | Pred(e0) -> 
          if (natexpr_to_expr (eval1 e0 rho)) = 0 
          then failwith "the predecessor of 0 does not exist" 
          else Nat(natexpr_to_expr (eval1 e0 rho) - 1)
      | IsZero(e0) -> Bool(natexpr_to_expr (eval1 e0 rho) = 0)
      | Var(x) -> rho x
      | Let(x,e0,e1) -> 
              (* Here the function rho is defined as an anonymous function.
                 When it is applied to some x' it returns the evalueated e0 iff x = x',
                 otherwise it tries to resolve the name of x' with the former 
                 environment function, eventually raising UnboundValue when
                 the first one is called. *)
              eval1 e1 (fun x' -> if x' = x then eval1 e0 rho else eval1 (Var x') rho)

  (* At this stage the environment is empty *)
  in eval1 expr (fun _ -> raise UnboundValue)
;;
