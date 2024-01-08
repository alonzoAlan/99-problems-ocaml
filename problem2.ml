let rec last_second: 'a list -> ('a * 'a) option = function
        | [] | [_] -> None
        | [x; y] -> Some (x , y)
        | _ :: xs -> last_second xs

let print_option_int opt_int = 
        match opt_int with
        | Some(x,y) -> print_int x;
                       print_int y
        | None -> print_string "None"

let () = print_string "LHS: None";
        (last_second []) |> print_option_int 

let () = print_string "\nLHS: None";
        (last_second [4;]) |> print_option_int 

let () = print_string "\nRHS: 34";
        (last_second [1; 2; 3; 4;]) |> print_option_int 
(*let my_val = last_second [1;] *)
