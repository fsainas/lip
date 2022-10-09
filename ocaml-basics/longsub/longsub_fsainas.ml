let longsub l = 
    let rec longsub_rec x n = function
          [] -> None
        | h::[] when x = h -> Some(x,n+1) 
        | h::[] -> Some(x,n)
        | h::t when x = h -> longsub_rec x (n+1) t
        | h::t -> let Some(x0,n0) = longsub_rec h 1 t in 
        if n >= n0 then Some(x,n) else Some(x0,n0) in (* longest subsequence between the current one and the longest of the rest of the list *)
    longsub_rec (List.hd l) 1 (List.tl l)
;;

assert(longsub [1;1;3;3] = Some (1,2));;
assert(longsub [1;2;2;2;1;1] = Some (2,3));;
assert(longsub [1;1;2;2;2;1;1;1;1] = Some (1,4));;
