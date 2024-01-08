(* find basis of index*)

let head = function
        | [] -> None
        | x :: _ -> Some(x)

let tail = function
        | [] -> []
        | (x :: xs) -> xs

let rec helper_at count key xs =
(*: int -> int -> 'a list -> 'a option = *)
        if count = key then
                if xs = [] then None
                else head xs
        else 
                if xs = [] then None
                else helper_at (count+1) key (tail xs) 

let print_option_string opt_string = 
        match opt_string with
        | Some(x) -> print_string x
        | None -> print_string "None"

let () = helper_at 0 8 ["a"; "b";"c";"d";"e"] |> print_option_string 


