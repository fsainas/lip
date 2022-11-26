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

(* trace1 : (string -> expr) -> expr ->  expr *)
(* performs a single step of small-step semantics *)
let rec trace1 rho = function
    If(True,e,_) -> e
  | If(False,_,e) -> e
  | If(e0,e1,e2) -> let e0' = trace1 rho e0 in If(e0',e1,e2)

  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 rho e in Not(e')

  | And(False,_) -> False
  | And(True,e) -> e
  | And(e0,e1) -> let e0' = trace1 rho e0 in And(e0',e1)

  | Or(False,e) -> e
  | Or(True,_) -> True
  | Or(e0,e1) -> let e0' = trace1 rho e0 in Or(e0',e1)

  | Succ(e) -> let e' = trace1 rho e in Succ(e')

  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 rho e in Pred(e')

  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False
  | IsZero(e) -> let e' = trace1 rho e in IsZero(e')

  | Var(x) -> rho x
  (* performs substitution iff e0 is a value *)
  | Let(x,e0,e1) when is_val e0 -> 
          let e1' = trace1 (fun x' -> if x = x' then e0 else rho x') e1 in e1'
  | Let(x,e0,e1) -> let e0' = trace1 rho e0 in Let(x,e0',e1)

  | _ -> raise NoRuleApplies
;;

(* trace : expr -> expr list *)
(* performs all steps of small-step semantics 
   and returns the list of them *)
let rec trace e = try
    let e' = trace1 (fun _ -> raise UnboundValue) e
    in e::(trace e')
  with 
    NoRuleApplies -> [e]
  | UnboundValue -> [e]
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

    let rec eval_rec e rho = match e with
        True -> Bool true
      | False -> Bool false
      | If(e0,e1,e2) -> Bool(
          if boolexpr_to_expr (eval_rec e0 rho) 
          then boolexpr_to_expr (eval_rec e1 rho) 
          else boolexpr_to_expr (eval_rec e2 rho) )
      | Not(e0) -> Bool(not (boolexpr_to_expr (eval_rec e0 rho)))
      | And(e0,e1) -> Bool(boolexpr_to_expr (eval_rec e0 rho) && boolexpr_to_expr (eval_rec e1 rho))
      | Or(e0,e1) -> Bool(boolexpr_to_expr (eval_rec e0 rho) || boolexpr_to_expr (eval_rec e1 rho))
      | Zero -> Nat(0)
      | Succ(e0) -> Nat(natexpr_to_expr (eval_rec e0 rho) + 1)
      | Pred(e0) -> 
          if (natexpr_to_expr (eval_rec e0 rho)) = 0 
          then failwith "the predecessor of 0 does not exist" 
          else Nat(natexpr_to_expr (eval_rec e0 rho) - 1)
      | IsZero(e0) -> Bool(natexpr_to_expr (eval_rec e0 rho) = 0)
      | Var(x) -> rho x
      | Let(x,e0,e1) -> 
              (* Here the function rho is defined as an anonymous function.
                 When it is applied to some x' it returns the evalueated e0 iff x = x',
                 otherwise it tries to resolve the name of x' with the former 
                 environment function, eventually raising UnboundValue when
                 the first one is called. *)
              eval_rec e1 (fun x' -> if x' = x then eval_rec e0 rho else eval_rec (Var x') rho)

  (* At this stage the environment is empty *)
  in eval_rec expr (fun _ -> raise UnboundValue)
;;
