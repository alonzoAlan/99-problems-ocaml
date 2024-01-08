let num_range num1 num2 =
  if num1 > num2 then
    let rec aux counter acc =
      if counter < num2 then failwith "think wise, not a good way to code"
      else if counter > num1 then acc
      else aux (counter+1) (counter::acc)
    in aux num2 []
  else
    let rec aux counter acc =
      if counter < num1 then failwith "think wise, not a good way to code"
      else if counter > num2 then acc
      else aux (counter+1) (counter::acc)
    in aux num1 [] |> List.rev

let rec print_int_list = function 
  | [] -> print_string "[]"
  | e::l -> print_int e ; print_string " " ; print_int_list l

let () = num_range 26 32 |> print_int_list

