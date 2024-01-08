let drop  (xs : 'a list) (num: int) : 'a list =
  let rec aux_drop acc local_num ys = 
    match ys with
    | [] -> acc
    | x :: ys -> if local_num <= 1 then aux_drop acc num ys (* reload *)
                                                 (* else if local_num == 0 then  *)
                 else aux_drop (x::acc) (local_num - 1) ys
  in
  aux_drop [] num xs |> List.rev

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let rec print_int_list = function 
  | [] -> ()
  | e::l -> print_int e ; print_string " " ; print_int_list l

let () = drop [12;23;43;54;] 0 |> print_int_list
           (* ["23";"23";"32"] 13  |> print_string_list *)
