let replicate  (xs : 'a list) (num: int) : 'a list = List.fold_left
                                                       (fun acc x ->
                                                         let rec adder count  my_acc =
                                                           if count = 0 then my_acc
                                                           else adder (count - 1) (x :: my_acc)
                                                        in adder num acc
                                                       ) [] xs |> List.rev

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let rec print_int_list = function 
  | [] -> ()
  | e::l -> print_int e ; print_string " " ; print_int_list l

let () = replicate [12;23;43;54;] 3 |> print_int_list
           (* ["23";"23";"32"] 13  |> print_string_list *)
