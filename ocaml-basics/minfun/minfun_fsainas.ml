type 'a option = Some of 'a | None;;

let rec minfun f a b = 
    if a > b 
    then failwith "a must be less or equal to b" 
    else match (a,b) with 
        (* The range is empty *)
          (a,b) when a = b -> None
        (* b is the successor of a so the minimum is a or b *)
        | (a,b) when (a+1) = b -> if (f a) <= (f b) then Some(a) else Some(b)
        (* narrow the range by removing the greatest element *)
        | (a,b) -> if (f a) < (f b) 
                    then minfun f a (b-1)
                    else minfun f (a+1) b
;;
