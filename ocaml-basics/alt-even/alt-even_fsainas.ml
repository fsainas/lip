(*let is_even x = x mod 2 = 0;;*)

let alt_even x = 
    let remove_first_digit x = ((x - (x mod 10)) / 10) in
    let rec alt_even_rec e = function   (* if e = 1 then check if the last digit is even else check if is odd *)
          x when (x < 10 && x > -10) -> if e = 1 then (x mod 2) = 0 else (x mod 2) = 1
        | x when e = 1 && (x mod 2) = 0 -> alt_even_rec (-e) (remove_first_digit x)
        | x when e = (-1) && (x mod 2) = 1 -> alt_even_rec (-e) (remove_first_digit x)
        | _ -> false in
    alt_even_rec 1 x
;;

assert(alt_even 8 = true);;
assert(alt_even 72 = true);;
assert(alt_even 1234 = true);;
assert(alt_even 3 = false);;
assert(alt_even 51 = false);;
assert(alt_even 8234 = false);;
