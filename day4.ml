
let rec input_lines () =
  try
    let line = read_line () in
    line :: input_lines ()
  with End_of_file -> []

let parse_numbers numbers =
  numbers
  |> String.split_on_char ' '
  |> List.filter_map int_of_string_opt

module IntSet = Set.Make(Int)

let list_to_set l =
  List.fold_left(fun k s -> IntSet.add s k) IntSet.empty l

let parse_card line =
  let numbers =
    match String.split_on_char ':' line with
    | [ _; numbers ] -> numbers
    | _ -> assert false
  in

  let numbers =
    numbers
    |> String.split_on_char '|'
    |> List.map parse_numbers
    |> List.map list_to_set
  in
  match numbers  with
  | [ winning_numbers; my_numbers ] -> (winning_numbers, my_numbers)
  | _ -> assert false

let () =
  let lines = input_lines () in
  let points =
    lines
    |> List.map parse_card
    |> List.map (fun (w, m) -> Int.shift_left 1 (IntSet.(cardinal (inter w m)) - 1))
    |> List.fold_left (+) 0
  in
  print_int points