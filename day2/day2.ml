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
  is_straight && is_gradual

let rec except n l =
  match l with
  | [] -> []
  | x :: xs -> if n = 0 then xs else (x :: (except (n - 1) (xs)))

let is_safeish l =
(*
  let sublists = List.init (List.length l) (fun x -> except x l) in
  List.iter print_endline (List.map string_of_bool (List.map is_safe sublists));

  print_endline "";

  (fun xs -> print_endline (String.concat ", " (List.map string_of_int xs)))

  print_endline
    ((String.concat ", "
    (List.map string_of_int (List.init (List.length l) (fun x -> except x l)))));
*)

  List.fold_left
    (||)
    false
    (List.map
      (fun x -> is_safe (except x l))
      (List.init (List.length l) (fun x -> x)))

let read_input =
  let ic = open_in "input.txt" in
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
  let safeishs = (List.map is_safeish input) in
  (*
  let sample = [1;2;7;8;9] in
  print_endline (string_of_bool (is_safeish sample));
  print_endline (String.concat "," (List.map string_of_int (except 3 sample)));
  *)
  print_endline (string_of_int (List.length (List.filter (fun x -> x) safes)));
  print_endline (string_of_int (List.length (List.filter (fun x -> x) safeishs)))
