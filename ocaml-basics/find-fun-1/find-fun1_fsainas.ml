let rec f x y =
        if x y = 0 then f x (y+1)
        else x y
;;

let g y = y + 3;;
