let is_even x = (x mod 2) = 0;;

let isValid x = (x >= 1 && x <= 5);;

let win a b = match (a,b) with
          (a,b) when not (isValid a) && not (isValid b) -> 0
        | (a,b) when not (isValid b) -> 1
        | (a,b) when not (isValid a) -> -1
        | (a,b) when is_even (a+b) -> 1
        | _ -> -1
;;

assert(win 1 3 = 1);;
assert(win 2 5 = (-1));;
assert(win (-1) 4 = (-1));;
assert(win 4 (-1) = 1);;
assert(win (-4) (-1) = 0);;
