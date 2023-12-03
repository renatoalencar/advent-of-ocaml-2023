
type contraints = 
  { red: int
  ; green: int
  ; blue: int }

type color =
  | Red of int
  | Blue of int
  | Green of int

type round = color list

type game = int * round list

let parse_color s =
  let aux count color =
    match color with
    | "red" -> Red count
    | "green" -> Green count
    | "blue" -> Blue count
  in
  match String.split_on_char ' ' s with
  | [ count; color ] -> aux (int_of_string count) color
  | _ -> assert false

let parse_round s =
  s
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map parse_color

let parse s =
  let game, rounds =
    match String.split_on_char ':' s with
    | [ game; rounds ] -> (game, rounds)
    | _ -> assert false
  in

  let game_id =
    match String.split_on_char ' ' game with
    | [ _; game_id ] -> int_of_string game_id
    | _ -> assert false
  in

  let rounds =
    rounds
    |> String.split_on_char ';'
    |> List.map String.trim
    |> List.map parse_round
  in

  (game_id, rounds)

let find_red = function
  | Red count -> Some count
  | _ -> None

let find_blue = function
  | Blue count -> Some count
  | _ -> None

let find_green = function
  | Green count -> Some count
  | _ -> None

let find_minimum rounds =
  let aux f =
    rounds
    |> List.filter_map (List.find_map f)
    |> List.fold_left Int.max 0
  in
  let red = aux find_red in
  let blue = aux find_blue in
  let green = aux find_green in
  { red; green; blue }

let power { red; green; blue } =
  red * green * blue

let rec read_lines () =
  try 
    let line = read_line () in
    line :: read_lines ()
  with End_of_file -> []

let () =
  let games = List.map parse @@ read_lines () in
  let sum =
    games
    |> List.map (fun (_, rounds) -> power @@ find_minimum rounds)
    |> List.fold_left (+) 0
  in
  print_int sum
