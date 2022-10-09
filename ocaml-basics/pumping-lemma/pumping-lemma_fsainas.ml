let rec pump k i j = function
      (*[] -> failwith "empty list" *)
      l when i = 0 && j <= 0 && k = 0 -> []
    | l when i = 0 && j <= 0 -> l @ pump (k-1) 0 0 l
    | h::t when i > 0 -> [h] @ pump k (i-1) (j-1) t (* remove elements from the front until i = 0*)
    | l -> List.rev (pump k ((List.length l) - j) 0 (List.rev l)) (* when i = 0 and j > 0 revert the list and swap j <-> i, j will be 0*)
;;

assert(pump 0 2 5 [1;2;3;4;5;6;7] = [1;2;6;7]);; 
assert(pump 1 2 5 [1;2;3;4;5;6;7] = [1;2;3;4;5;6;7]);;
assert(pump 2 2 5 [1;2;3;4;5;6;7] = [1;2;3;4;5;3;4;5;6;7]);;

(*
[1;2;3;4;5;6;7]
[3;4;5;6;7]
[7;6;5;4;3]
[7;6;5;4]@[3]@[3]
[1;2]@[3;3;4;5;6;7] *)
