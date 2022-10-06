let rec rnd_list n b = if n > 1 
    then [(Random.int b) + 1] @ rnd_list (n-1) b 
    else [Random.int b + 1]
;;

rnd_list 4 5;;
