type 'a option = Some of 'a | None;;

let rec minfun f a b = match (a,b) with 
        (* The range is empty *)
          (a,b) when a > b -> None
        (* There is only one element in the range *)
        | (a,b) when a = b -> Some(a)
        (* narrow the range by removing the greatest element *)
        | (a,b) -> if (f a) < (f b) 
                    then minfun f a (b-1)
                    else minfun f (a+1) b
;;
