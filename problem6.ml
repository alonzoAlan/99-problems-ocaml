let is_palindrome xs = xs = List.rev xs

let () = if is_palindrome [1;2;1;2;] then print_string "Palindrome" else print_string "Not a palindrome"
