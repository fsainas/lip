type grade = Val of int | CumLaude;;

let is_valid = function
          CumLaude -> true
        | Val(x) -> (x >= 18 && x <= 30)
;;

let int_of_grade x = 
        if is_valid x then match x with
        | CumLaude -> 32
        | Val(x) -> x
        else failwith "not a valid grade"
;;

let rec is_list_valid = function
          [] -> false
        | h::[] -> is_valid h
        | h::l when is_valid h -> is_list_valid l
        | _ -> false
;;

let rec avg l = if is_list_valid l 
        then float_of_int (List.fold_left (+) 0 (List.map int_of_grade l) ) /. float_of_int (List.length l)
        else failwith "not a valid list"
;;
