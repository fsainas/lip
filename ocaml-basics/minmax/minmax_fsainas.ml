let minmax f a b = 
    if a > b 
    then failwith "b can't be less than a"
    else
        let rec min f a b = if a = b then a 
        (* narrow the range by removing the greatest element *)
        else if (f a) < (f b) 
            then min f a (b-1)
            else min f (a+1) b in 
        (* narrow the range by removing the minor element *)
        let rec max f a b = if a = b then a
        else if (f a) > (f b)
            then max f a (b-1) 
            else max f (a+1) b in

        (min f a b, max f a b)
;;

let g x = x*x;;

minmax g (-5) 5;;
