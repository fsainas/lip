let alt l = 
    let rec alt_rec i = function (* i>0 for (>), i<0 for (<) *)
          [] -> true
        | h::[] -> true
        | h::(x::t) when h = x -> false
        | h::(x::t) when i > 0 -> if h > x then alt_rec (-1) (x::t) else false
        | h::(x::t) -> if h < x then alt_rec 1 (x::t) else false in
    (alt_rec 1 l) || (alt_rec (-1) l)
;;

let ping_pong l = 
    (* [int -> int -> int list] nets (integers) between 2 integers *)
    let rec nets_between x y = match x with
          x when x = y -> []
        | x when x < y -> if (x+1) = y then [] else [x+1] @ nets_between (x+1) y 
        | _ -> if (x-1) = x then [] else [x-1] @ nets_between (x-1) y in
    (* [a' list -> a' list -> a' list] list of elements that are present in both lists*)
    let rec intersection l1 l2 = match (l1,l2) with
          ([],_) -> []
        | (_,[]) -> []
        | (h::t,l2) -> (List.filter (fun x -> x=h) l2) @ (intersection t l2) in
    let rec ping_pong_rec nets = function
          [] -> false
        | h::[] -> false
        | h::(x::[]) -> (intersection nets (nets_between h x)) != []
        | h::(x::t) -> if (intersection nets (nets_between h x)) = [] then false 
                       else  ping_pong_rec (intersection nets (nets_between h x)) (x::t) in
    alt l && ping_pong_rec (nets_between (List.hd l) (List.hd (List.tl l))) l
;;

assert(alt [1;5;2;5;1;6] = true);;
assert(alt [1;5;2;5;4;3] = false);;
assert(alt [1;5;2;3;2;4] = true);;
assert(alt [3;1;4;2;5;3] = true);;

assert(ping_pong [1;5;2;5;4;3] = false);;
assert(ping_pong [1;5;2;3;2;4] = false);;
assert(ping_pong [1;5;2;5;1;6] = true);;
assert(ping_pong [3;1;4;2;5;3] = false);;
