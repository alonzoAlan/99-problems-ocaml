type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode (xs: 'a rle list) : 'a list =
  List.fold_left (fun acc x -> match x with
                           | One y -> y :: acc
                           | Many (len,y) ->
                              let rec add len y my_acc =
                                if len = 0 then my_acc
                                else add (len-1) y (y::my_acc)
                              in add len y acc) [] xs
  |> List.rev

let rec print_string_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_string_list l

let () = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] |> print_string_list
