let rec non_tail_length = function 
        | [] -> 0
        | _ :: xs -> 1 + non_tail_length  xs

let rec helper_tail_length count = function 
        | [] -> count
        | _ :: xs -> helper_tail_length (count + 1) xs 


let length = helper_tail_length 0
let () = length [1;2;2;3;423;4;45;] |> print_int
let () = print_newline ()
let () = length [] |> print_int
let () = print_newline ()
