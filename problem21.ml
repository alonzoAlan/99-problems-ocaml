let insert_at num xs element =
  let rec aux acc counter = function
    | [] -> if counter = num then ( element  :: acc)  else  acc

    | y :: ys -> if counter = num then aux  (y :: element  :: acc) (counter+1) ys else aux (y::acc) (counter+1) ys
  in aux [] 0 xs |> List.rev

let rec print_int_list = function 
  | [] -> print_string "[]"
  | e::l -> print_int e ; print_string " " ; print_int_list l

let () = insert_at 6 [1;2;3;4;5;6;] 32 |> print_int_list
