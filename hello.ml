let () = print_string "Hello World\n"

let rec last = function
        | [] -> None
        | [x] -> Some(x)
        | _ :: xs -> last xs;;

let is_empty = function
        | [] -> true
        | _ -> false;;

let bool_of_string = function
        | true -> "true"
        | false -> "false"

let () = is_empty [1;] |> bool_of_string |> (fun x -> print_string (x ^ "\n"))

let print_option_int opt_int = 
        match opt_int with
        | Some(x) -> print_int x
        | None -> print_string "None"

let () = 
        let res1 = last [1; 2; 3; 4;] in   
        if res1 = Some(4) then print_option_int res1 else failwith "Assertion failed: x must be greater than 0"  

