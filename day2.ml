
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

let match_constraints { red; green; blue } (game_id, rounds) =
  let match' = function
    | Red count -> count <= red
    | Green count -> count <= green
    | Blue count -> count <= blue
  in
  List.for_all (List.for_all match') rounds

let rec read_lines () =
  try 
    let line = read_line () in
    line :: read_lines ()
  with End_of_file -> []


let constraints =
  { red = 12; green = 13; blue = 14}

let () =
  let games = List.map parse @@ read_lines () in
  let sum =
    games
    |> List.filter (match_constraints constraints)
    |> List.map (fun (game_id, _) -> game_id)
    |> List.fold_left (+) 0
  in
  print_int sum
