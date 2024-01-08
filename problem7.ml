  type 'a node =
    | One of 'a 
    | Many of 'a node list;;

  let rec aux_flatten = function
    | One x -> x :: []
    | Many xs -> List.fold_left (fun  acc my_node -> acc @ (aux_flatten my_node )) [] xs

  let flatten xs = List.fold_left (fun acc my_node -> acc @ (aux_flatten my_node)) [] xs

  let second_tree = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

  let rec print_int_list = function 
      [] -> ()
    | e::l -> print_int e ; print_string " " ; print_int_list l

  let rec print_string_list = function 
      [] -> ()
    | e::l -> print_string e ; print_string " " ; print_string_list l

  let tree = [One 23; Many [One 92;];]

  let () = print_string_list (flatten second_tree)
             (* [1;2;3;4;5;] *)
