let rec read_lines () =
  try 
    let line = read_line () in
    line :: read_lines ()
  with End_of_file -> []

let is_digit c =
  c >= '0' && c <= '9'

let find_digits s =
  s
  |> String.to_seq
  |> Seq.filter is_digit
  |> String.of_seq

let find_first_and_last_digits s =
  let digits = find_digits s in
  let first = Char.code digits.[0] - 48 in
  let last = Char.code digits.[String.length digits - 1] - 48 in
  first * 10 + last

let () =
  let total =
    read_lines ()
    |> List.map find_first_and_last_digits
    |> List.fold_left (+) 0
  in
  Printf.printf "%d\n" total