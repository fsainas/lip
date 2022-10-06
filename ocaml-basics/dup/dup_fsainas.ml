(* true if x is in the list l *)
let rec find x = function
      [] -> false
    | h::l when h = x -> true
    | h::l -> find x l
;;
(* true iff the list contains duplicates *)
let rec dup = function
      [] -> false
    | h::l when find h l -> true
    | h::l -> dup l
;;

assert(dup [1;2;3;4;5;3] = true);;
assert(dup [1;2;3] = false);;
