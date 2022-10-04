let rec fib = function
          0 -> [0]
        | 1 -> (fib 0) @ [1]
        | n -> (fib (n-1)) @ [ List.hd (List.rev (fib (n-2))) + List.hd (List.rev (fib (n-1)))]
;;

(* Calculates the nth element *)
let rec fib_n = function
          0 -> 0
        | 1 -> 1
        | n -> fib_n (n-2) + fib_n (n-1)
;;

let rec fib_list = function
          0 -> [0]
        | 1 -> (fib_list 0) @ [1]
        | n -> (fib_list (n-1)) @ [fib_n n]
;;

assert(fib 9 = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]);;
