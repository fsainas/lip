type card = Joker | Val of int;;

let isCardValid = function
          Joker -> true
        | Val(c) when c >= 1 && c <= 10 -> true
        | _ -> false

let win d p = match (d,p) with
          (Joker, Joker) -> false
        | (Joker, _) -> false
        | (_, Joker) -> true
        | (Val(d),Val(p)) when (isCardValid (Val d)) && (isCardValid (Val p)) -> d < p
        | _ -> failwith "invalid card"
;;

assert(win Joker Joker = false);;
assert(win Joker (Val 5) = false);;
assert(win (Val 5) Joker = true);;
assert(win (Val 5) (Val 5) = false);;
assert(win (Val 5) (Val 3) = false);;
assert(win (Val 3) (Val 5) = true);;
