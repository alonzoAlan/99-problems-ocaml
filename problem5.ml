let rec non_tail_rev xs =
  match xs with
  | [] -> []
  | x :: xs -> non_tail_rev xs @ x :: []

let rev list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] list;;

let revved_list = [5;4;3;2;1;]

let () = if revved_list = rev [1;2;3;4;5;] then print_string "Yes" else print_string "No"
