
let compress xs : 'a list = 
        let rec aux_compress prev acc = function
                | [] -> acc
                | y :: ys -> if y = prev then aux_compress y acc ys else aux_compress  y (y :: acc) ys
        in
        match xs with
        | [] -> [] 
        | x :: xs ->  aux_compress x (x ::[]) xs |> List.rev

let rec print_string_list = function 
    |  [] -> ()
    | e::l -> print_string e ; print_string " " ; print_string_list l

let () = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> print_string_list

let () = if 
        compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"] 
                then print_string "yes" else print_string "no" 
