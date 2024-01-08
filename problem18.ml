let slice (xs : 'a list)  (num1: int) (num2: int) : 'a list =
  if num1 > num2 then
    failwith "slice improper"
  else let rec aux_slice index ys acc = match ys with
         | [] -> acc
         | z :: zs -> if index < num1 then aux_slice (index+1) zs acc
                      else if index > num2 then acc
                      else aux_slice (index + 1) zs (z::acc)
       in
       aux_slice 0 xs [] |> List.rev

let rec print_int_list = function 
  | [] -> ()
  | e::l -> print_int e ; print_string " " ; print_int_list l

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let () = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 12 6 |> print_string_list
  (* slice [12;23;43;54;] 1 4 *)

