let rec read_lines () =
  try 
    let line = read_line () in
    line :: read_lines ()
  with End_of_file -> []

let digit_lookup =
  [ "one"  , '1'
  ; "two"  , '2'
  ; "three", '3'
  ; "four" , '4'
  ; "five" , '5'
  ; "six"  , '6'
  ; "seven", '7'
  ; "eight", '8'
  ; "nine" , '9' ]

let digits =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let check_verbal_digit s i =
  let s = String.sub s i (String.length s - i) in
  let possible_digit =
    List.find_opt
      (fun prefix -> String.starts_with ~prefix s)
      digits
  in
  Option.bind
    possible_digit
    (fun d -> List.assoc_opt d digit_lookup)

let rec find_digits s =
  let rec aux i =
    if i >= String.length s then []
    else
      let c = s.[i] in
      match c with
      | '0'..'9' -> c :: aux (i + 1)
      | 'o' | 't' | 'f' | 's' | 'e' | 'n' -> (
        match check_verbal_digit s i with
        | Some c -> c :: aux (i + 1)
        | None -> aux (i + 1))
      | _ -> aux (i + 1)
  in
  aux 0

let rec list_last = function
  | [] -> assert false
  | [ el ] -> el
  | hd :: tl  -> list_last tl

let find_first_and_last_digits s =
  let digits = find_digits s in
  let first = Char.code (List.hd digits) - 48 in
  let last = Char.code (list_last digits) - 48 in
  first * 10 + last

let () =
  let total =
    read_lines ()
    |> List.map find_first_and_last_digits
    |> List.fold_left (+) 0
  in
  Printf.printf "%d\n" total