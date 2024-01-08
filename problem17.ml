let split  (xs : 'a list) (num: int) : ('a list * 'a list) =
  let rec aux_split acc local_num ys = 
    match ys with
    | [] -> (List.rev acc,[])
    | z :: zs -> if local_num = 0 then (List.rev acc,(z :: zs))
                 else aux_split (z::acc) (local_num - 1) zs
  in
  aux_split [] num xs
  (* |> List.rev *)

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let rec print_int_list = function 
  | [] -> print_string "[]"
  | e::l -> print_int e ; print_string " " ; print_int_list l

let () = match (split [12;23;43;54;] 3) with
    (xs,ys) -> print_int_list xs;
               print_string "\n";
               print_int_list ys
         (* |> print_int_list *)
           (* ["23";"23";"32"] 13  |> print_string_list *)
