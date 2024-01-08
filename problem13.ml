type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode : 'a list -> 'a rle list =
  let rec aux_encode (prev: 'a rle) (acc: 'a rle list) = function
    | [] -> acc
    | x :: xs -> match prev with
                 | One y -> if x = y
                            then aux_encode (Many (2,x)) (Many (2,x) :: (List.tl acc)) xs
                            else aux_encode (One x) (One x::acc) xs
                 | Many (count_y,y) ->
                    if y = x
                    then aux_encode (Many (count_y+1,y)) ((Many (count_y+1,y)):: (List.tl acc)) xs
                    else aux_encode (One x) (One x :: acc) xs
  in
  function
  | [] -> []
  | x :: xs -> aux_encode (One x) [One x;] xs

let print_tuple_int_len = List.iter (fun x -> match x with
                                              | One x_string ->
                                                 print_string " ";
                                                 print_string x_string
                                              | Many (len,x_string) -> print_string x_string;
                                                                       print_string " ";
                                                                       print_int len)

let test1 =  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let () =  encode test1 |> print_tuple_int_len 
