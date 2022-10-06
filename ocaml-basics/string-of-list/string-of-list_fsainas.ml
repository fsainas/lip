let string_of_list l = 
    let rec string_of_list_rec = function      
          [] -> "[]"
        | h::[] -> string_of_int h
        | h::t -> string_of_int h ^ ";" ^ list_to_string t
        in "[" ^ string_of_list_rec l ^ "]"
;;

string_of_list [1;2;3;4;5];;
