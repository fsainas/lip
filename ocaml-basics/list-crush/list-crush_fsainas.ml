let crush l = 
    let rec crushing x = function (* ['a -> 'a list -> a' list] crushes the first elements of a list*)
          [] -> []
        | h::t when x = h -> crushing x t
        | l -> l in
    let rec crush_rec = function (* crushes the list once *)
      [] -> []
    | h::(x::t) when h = x -> crush (crushing x t)
    | h::t -> [h] @ crush t in
    if List.length l = List.length (crush_rec l) (* if the list can't be reduced more then finish else crush again*)
        then crush_rec l  
        else crush_rec (crush_rec l)
;;

assert(crush [1;1;2;1;1;3;2;1;1;1;3;3;1;1;2;3] = [2]);;
