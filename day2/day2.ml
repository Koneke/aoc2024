let () = print_endline "ok"

let is_asc_or_desc l =
  l = (List.sort compare l)
  || (List.rev l) = (List.sort compare l)

let rec is_gradual l =
  match l with
  | [] -> true
  | x :: [] -> true
  | x :: y :: xs -> x <> y && (abs (x - y)) < 4 && is_gradual (y :: xs)

let is_safe l = is_asc_or_desc l && is_gradual l

let rec except n l =
  match l with
  | [] -> []
  | x :: xs -> if n = 0 then xs else (x :: (except (n - 1) (xs)))

let is_safeish l =
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
  print_endline (string_of_int (List.length (List.filter (fun x -> x) safes)));
  print_endline (string_of_int (List.length (List.filter (fun x -> x) safeishs)))
