type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let is_valid (Card(x,s)) = (x >= 1 && x <= 10) && (s = Spades || s = Hearts || s = Diamonds || s = Clubs);;

let rec is_complete d =
    let rec is_suit_complete s l = function
          n when n = 10 -> List.length (List.filter (fun x -> x = Card(n,s)) l) = 1
        | n when List.length (List.filter (fun x -> x = Card(n,s)) l) = 1 -> is_suit_complete s l (n+1)
        | _ -> false in
    (List.length d = 40) && (is_suit_complete Spades d 1) && (is_suit_complete Hearts d 1) && (is_suit_complete Diamonds d 1) && (is_suit_complete Clubs d 1)
;;

let gen_deck = 
    let rec list_of_int x = if x > 1 then list_of_int (x-1) @ [x] else [x] in (* creates a list of ordered integers *)
    let rec scrumble_list nth l = if List.length l > 1
        then [List.nth l nth] @ scrumble_list (Random.int ((List.length l)-1)) (List.filter (fun x -> x != (List.nth l nth)) l) (* randomly scrumbles the list *)
        else l in 
    let int_to_card = function
          x when x < 1 || x > 40 -> failwith "int out of bounds"
        | x when x <= 10 -> Card(x,Spades)
        | x when x <= 20 -> Card(x-10,Hearts)
        | x when x <= 30 -> Card(x-20,Diamonds)
        | x -> Card(x-30,Clubs) in
    List.map int_to_card (scrumble_list (Random.int 40) (list_of_int 40))
;;

assert(is_complete gen_deck = true);;
