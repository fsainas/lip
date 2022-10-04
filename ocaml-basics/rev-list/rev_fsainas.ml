let rec rev = function
        [] -> []
        | h::l -> (rev l) @ [h]
;;

assert(rev [1;2;3;4;5] = [5; 4; 3; 2; 1]);;

