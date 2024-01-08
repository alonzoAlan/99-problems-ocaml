let pack : 'a list -> 'a list list = 
  let rec aux_pack prev acc = function
    | [] -> acc
    | x :: xs -> if prev = x 
                 then
                   match acc with
                   | [] -> aux_pack x ((x :: []) :: []) xs
                   | y :: ys -> aux_pack x ((x :: y) :: ys) xs
                 else
                   aux_pack x ((x :: []) :: acc) xs
  in
  function 
  | [] -> []
  | x :: xs -> aux_pack x ((x :: []) :: []) xs

let rec print_list_string = function
  | [] -> print_string "\n"
  | x :: xs -> print_string x;
               print_string " ";
               print_list_string xs

let rec print_nested_list = function
  | [] -> print_string "\n"
  | x :: xs -> print_list_string x;
               print_nested_list xs

let () = if  pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = List.rev [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                                                                                                ["e"; "e"; "e"; "e"]] then print_string "true" else print_string "false"
let () =  pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] |> print_nested_list
