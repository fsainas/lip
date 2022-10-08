let rec extract i = function
      l when (i >= List.length l) || i < 0 -> failwith "i is out of the bounds of the list"
    | h::t when i = 0 -> (h, t)
    | h::t -> let (x,l) = extract (i-1) t in (x,[h]@l)
;;

assert(extract 0 [1;2;3] = (1, [2;3]));;
assert(extract 1 [1;2;3] = (2, [1;3]));;
assert(extract 2 [1;2;3] = (3, [1;2]));;
