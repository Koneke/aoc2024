let () = print_endline "ok"

let is_asc_or_desc l =
  l = (List.sort compare l)
  || (List.rev l) = (List.sort compare l)

let rec is_in_bounds l =
  match l with
  | [] -> true
  | x :: [] -> true
  | x :: y :: xs -> x <> y && (abs (x - y)) < 4 && is_in_bounds (y :: xs)

let is_safe l =
  let is_straight = is_asc_or_desc l in
  let is_gradual = is_in_bounds l in
  print_endline (string_of_bool is_straight);
  print_endline (string_of_bool is_gradual);
  print_endline "";
  is_straight && is_gradual

let read_input =
  let ic = open_in "test.txt" in
  let rec fn acc =
    try
      let line = input_line ic in
      let split = List.filter
        (fun x -> x <> "")
        (String.split_on_char ' ' line)
      in
      fn ((List.map int_of_string split) :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
    in
    fn []

let () =
  let input = read_input in
  let safes = (List.map is_safe input) in
  List.iter
    (fun line -> print_endline (String.concat "," (List.map string_of_int line)))
    input;
  List.iter (fun x -> print_endline (string_of_bool x)) safes;
  print_endline (string_of_int (List.length (List.filter (fun x -> x) safes)))
