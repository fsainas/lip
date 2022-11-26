open Ast

type exprval = Bool of bool | Nat of int        (* value of an expression *)
type state = ide -> exprval                     (* state = map from identifiers to expression values *)
type conf = St of state | Cmd of cmd * state    (* configuration = state | (command,state) *)

(* string_of_val : exprval -> string *)
let string_of_val = function
    Bool e -> string_of_bool e
  | Nat e -> string_of_int e
;;

(* string_of_expr : expr -> string *)
let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Var x -> x
  | Const k -> string_of_int k
  | Not e -> "Not(" ^ string_of_expr e ^ ")"
  | And(e0,e1) -> "And(" ^ string_of_expr e0 ^ "," ^ string_of_expr e1 ^ ")"
  | Or(e0,e1) -> "Or(" ^ string_of_expr e0 ^ "," ^ string_of_expr e1 ^ ")"
  | Add(e0,e1) -> string_of_expr e0 ^ "+" ^ string_of_expr e1
  | Sub(e0,e1) -> string_of_expr e0 ^ "-" ^ string_of_expr e1
  | Mul(e0,e1) -> string_of_expr e0 ^ "*" ^ string_of_expr e1
  | Eq(e0,e1) -> string_of_expr e0 ^ "==" ^ string_of_expr e1
  | Leq(e0,e1) -> string_of_expr e0 ^ ">=" ^ string_of_expr e1
;;

(* string_of_cmd : cmd -> string *)
let rec string_of_cmd = function
    Skip -> "skip"
  | Assign(x,e) -> "Assign(" ^ x ^ "," ^ string_of_expr e ^ ")"
  | Seq(c1,c2) -> string_of_cmd c1 ^ ";" ^ string_of_cmd c2
  | If(e,c1,c2) -> "If(" ^ string_of_expr e ^ "," ^ string_of_cmd c1 ^ "," ^ string_of_cmd c2 ^ ")"
  | While(e,c) -> "While(" ^ string_of_expr e ^ "," ^ string_of_cmd c ^ ")"
;;

(* string_of_state : state -> ide list -> string *)
(* ide -> exprval -> ide list -> string *)
let string_of_state st idlist = 
    let rec string_of_state_rec = function 
    [] -> ""
  | id::[] -> "(" ^ id ^ "," ^ string_of_val (st id) ^ ")"
  | id::t -> "(" ^ id ^ "," ^ string_of_val (st id) ^ ")" ^ "," ^ string_of_state_rec t
    in "[" ^ string_of_state_rec idlist ^ "]"
;;

(* string_of_conf : ide list -> conf -> string *)
let string_of_conf idlist = function
    St(st) -> "St(" ^ (string_of_state st idlist) ^ ")"
  | Cmd(c,st) -> "Cmd(" ^ string_of_cmd c ^ "," ^ (string_of_state st idlist) ^ ")" 
;;

(* string_of_trace : ide list -> conf list -> string *)
let rec string_of_trace idlist = function
    [] -> ""
  | conf::[] -> string_of_conf idlist conf
  | conf::t -> string_of_conf idlist conf ^ string_of_trace idlist t 
;;
(* dedup : ide list -> ide list *)
(* deduplicate a list of identifiers *)
let rec dedup = function 
        [] -> []
      | x::t -> [x] @ (dedup (List.filter (fun x' -> not (x = x')) t))
 
(* vars_of_cmd : cmd -> ide list*)
let rec vars_of_cmd = function
    Skip -> []
  | Assign(x,_) -> [x] 
  | Seq(c1,c2) -> dedup (vars_of_cmd c1 @ vars_of_cmd c2)
  | If(_,c1,c2) -> dedup (vars_of_cmd c1 @ vars_of_cmd c2)
  | While(_,c) -> dedup (vars_of_cmd c)
;;

let rec last = function
    [] -> failwith "Empty list"
  | [x] -> x
  | _::t -> last t
;;

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;
(******************************************************************************)
(*                                  Expr semantics                            *)
(******************************************************************************)

(* boolexpr_to_bool : exprval -> bool *)
let boolexpr_to_bool = function 
    Bool(e) -> e 
  | _ -> failwith "a boolean was expected"
;;

(* natexpr_to_int : exprval -> int *)
let natexpr_to_int = function 
    Nat(e) -> e 
  | _ -> failwith "a natural number was expected"
;;

(* eval_expr : state -> expr -> exprval *)
let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false

  | Var x -> st x
  | Const k -> Nat k 

  | Not e -> Bool(not (boolexpr_to_bool (eval_expr st e)))
  | And(e0,e1) -> Bool(boolexpr_to_bool (eval_expr st e0) && boolexpr_to_bool (eval_expr st e1))
  | Or(e0,e1) -> Bool(boolexpr_to_bool (eval_expr st e0) || boolexpr_to_bool (eval_expr st e1))

  | Add(e0,e1) -> Nat(natexpr_to_int (eval_expr st e0) + natexpr_to_int (eval_expr st e1))
  | Sub(e0,e1) -> Nat(natexpr_to_int (eval_expr st e0) - natexpr_to_int (eval_expr st e1))
  | Mul(e0,e1) -> Nat(natexpr_to_int (eval_expr st e0) * natexpr_to_int (eval_expr st e1))

  | Eq(e0,e1) -> Bool(natexpr_to_int (eval_expr st e0) = natexpr_to_int (eval_expr st e1))
  | Leq(e0,e1) -> Bool(natexpr_to_int (eval_expr st e0) <= natexpr_to_int (eval_expr st e1))
;;

(******************************************************************************)
(*                               Command semantics                            *)
(******************************************************************************)

exception UnboundVar of string
exception NoRuleApplies

(* trace1 : conf -> conf *)
let rec trace1 = 
    let assign st x ev = fun x' -> if x' = x then ev else st x' 
    in function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with 
        Skip -> St st
      | Assign(x,e) -> St (assign st x (eval_expr st e))
      | Seq(c0,c1) -> (match trace1 (Cmd(c0,st)) with 
            St st' -> Cmd(c1,st')
          | Cmd(c0',st') -> Cmd(Seq(c0',c1),st'))
      | If(e,c0,c1) -> if boolexpr_to_bool (eval_expr st e) 
                       then Cmd(c0,st)
                       else Cmd(c1,st)
      | While(e,c0) -> if boolexpr_to_bool (eval_expr st e)
                       then Cmd(Seq(c0,While(e,c0)),st)
                       else St st
;;

(* trace : int -> cmd -> conf list  *)
let trace n c = 
    (* trace_rec : int -> conf -> conf list *)
    let rec trace_rec n' conf = 
        if n'<=0 then [conf]
        else try
            let conf' = trace1 conf 
            in conf::(trace_rec (n'-1) conf')
        with NoRuleApplies -> [conf]
    in trace_rec n (Cmd(c,(fun x -> raise (UnboundVar x))))
;;
