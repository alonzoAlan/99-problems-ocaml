let duplicate (xs : 'a list) : 'a list = List.fold_left
                                       (fun acc x -> x :: x :: acc) [] xs |> List.rev

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let () = duplicate ["r";"23";"32"] |> print_string_list 
