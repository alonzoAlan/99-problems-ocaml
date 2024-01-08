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

let encode  (ls: 'a list) : ('a * int) list = List.map (fun x -> (List.hd x,List.length x)) (pack ls)

let print_tuple_int_len = List.iter (fun x -> match x with
                                               (x_string,len) -> print_string x_string;
                                                                 print_int len)
                
let test1 =  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
                                              
let () =  test1 |> encode |> print_tuple_int_len
