let is_posfrac (a,b) = 
        if b = 0 then failwith "this is not a fraction, the denominator is 0"
        else ((a*b) >= 0)
;;

(*
(* Calculates the greatest common divisor of two numbers *)
let rec gcd x y = match (x,y) with
          (0,0) -> failwith "every number divides 0"
        | (x,0) -> x
        | (0,y) -> y
        | (x,y) when x > y -> gcd y (x mod y)
        | (_,_) -> gcd x (y mod x)
;;

(* Calculates the least common multiple of two numbers *)
let rec lcm x y = x*y/(gcd x y);;

let compare_posfrac (a,b) (c,d) = match (a,b,c,d) with
          (a,b,c,d) when 
                not (is_posfrac (a,b)) || not (is_posfrac (c,d)) 
                -> failwith "a/b or c/d is not a positive fraction"
        | (a,b,c,d) when b = d -> if a > c then 1 else (-1) (* same denominator *)
        | (a,b,c,d) when a*((lcm b d)/b) = c*((lcm b d)/d) -> 0 (* they are equal *)
        | (a,b,c,d) -> if a*((lcm b d)/b) > c*((lcm b d)/d) then 1 else (-1)
;;*)

let compare_posfrac (a,b) (c,d) = match (a,b,c,d) with
          (a,b,c,d) when 
                not (is_posfrac (a,b)) || not (is_posfrac (c,d)) 
                -> failwith "a/b or c/d is not a positive fraction"
        | (a,b,c,d) when (a*d) = (c*b) -> 0 (* same rational number *)
        | (a,b,c,d) when (a*d) > (c*b) -> 1 (* the first is grater *)
        | (a,b,c,d) when (a*d) < (c*b) -> (-1) (* the second is grater *)
;;

let compare_frac (a,b) (c,d) = match (a,b,c,d) with
          (a,b,c,d) when is_posfrac (a,b) && not is_posfrac (c,d) -> 1
        | (a,b,c,d) when not is_posfrac (a,b) && is_posfrac (c,d) -> (-1)
        | (a,b,c,d) when (a*d) = (c*b) -> 0 (* same rational number *)
        | (a,b,c,d) when (a*d) > (c*b) -> 1 (* the first is grater *)
        | (a,b,c,d) when (a*d) < (c*b) -> (-1) (* the second is grater *)
;;

assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;
