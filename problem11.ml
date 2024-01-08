type 'a rle =
  | One of 'a
  | Many of int * 'a

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

let encode  (ls: 'a list) : 'a rle list = List.map (fun x -> if List.length x = 1
                                                             then One (List.hd x)
                                                             else Many (List.length x,List.hd x)) (pack ls)

let print_tuple_int_len = List.iter (fun x -> match x with
                                              | One x_string -> print_string x_string
                                              | Many (len,x_string) -> print_string x_string;
                                                                 print_int len)
                
let test1 =  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
                                              
let () =  test1 |> encode |> print_tuple_int_len
