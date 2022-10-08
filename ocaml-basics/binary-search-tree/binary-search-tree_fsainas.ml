type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let rec is_bstree t comp = match t with
      Empty -> true
    | Node(_, Empty, Empty) -> true
    | Node(x, Node(y,ts,td), Empty) -> if comp x y > 0 then is_bstree (Node(y,ts,td)) comp else false
    | Node(x, Empty, Node(y,ts,td)) -> if comp x y < 0 then is_bstree (Node(y,ts,td)) comp else false 
    | Node(x, Node(y,tss,tsd), Node(z,tds,tdd)) -> if (comp x y > 0) && (comp x z < 0) 
        then (is_bstree (Node(y,tss,tsd)) comp && is_bstree (Node(z,tds,tdd)) comp)
        else false
;;

let rec search t comp x0 = match t with
      Empty -> false
    | Node(x, ts, td) when comp x0 x < 0 -> search ts comp x0
    | Node(x, ts, td) when comp x0 x > 0 -> search td comp x0
    | _ -> true
;;

assert(is_bstree (Node(7,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))) compare = true);;
assert(is_bstree (Node(7,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(2,Empty,Empty))) compare = false);;

assert(search (Node(7,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))) compare 5 = true);;
assert(search (Node(7,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))) compare 6 = false);;
