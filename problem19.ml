let split  (xs : 'a list) (num: int) : ('a list * 'a list) =
  let rec aux_split acc local_num ys = 
    match ys with
    | [] -> (List.rev acc,[])
    | z :: zs -> if local_num = 0 then (List.rev acc,(z :: zs))
                 else aux_split (z::acc) (local_num - 1) zs
  in
  aux_split [] num xs

let rotate xs num = let custom_num = if num < 0 then (List.length xs) + num else num in 
                    match split xs custom_num with
                      (front,rear) -> rear @ front

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let rec print_int_list = function 
  | [] -> print_string "[]"
  | e::l -> print_int e ; print_string " " ; print_int_list l

let () = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]  (-2) |> print_string_list
  (* rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 |> print_string_list *)
  (* rotate [1;2;3;4;5;] |>  *)

