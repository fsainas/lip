let rec list_to_string = function      
      [] -> "[]"
    | h::[] -> string_of_int h
    | h::t -> string_of_int h ^ ";" ^ list_to_string t
;;
let string_of_list l = "[" ^ list_to_string l ^ "]"
;;

string_of_list [1;2;3;4;5];;
