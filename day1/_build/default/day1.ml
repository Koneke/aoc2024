let () = print_endline "ok"

(*
let left = List.sort compare [3; 4; 2; 1; 3; 3]
let right = List.sort compare [4; 3; 5; 3; 9; 3]

let () = print_endline
	(
		(String.concat ", " (List.map string_of_int left))
		^ "\n" ^
		(String.concat ", " (List.map string_of_int right))
	)

let print_list (l : int list) = print_endline (String.concat ", " (List.map string_of_int l))
let rec sum (l : int list) = match l with
	| [] -> 0
	| hd :: tl -> hd + sum tl

let () =
	let l = List.sort compare [3; 4; 2; 1; 3; 3] in
	let r = List.sort compare [4; 3; 5; 3; 9; 3] in
	let c = List.combine l r in
	let distances = List.map (fun lr -> abs ((fst lr) - (snd lr))) c in
		print_endline (string_of_int (sum distances));
		print_list distances
*)

(*
let read_lines filename =
	let ic = open_in filename in
	let rec aux acc =
		try
			let line = input_line ic in
			aux (line :: acc)
		with End_of_file ->
			close_in ic;
			List.rev acc
	in
	aux []
*)

(*
let read_input filename =
	let ic = open_in filename in
	let rec fn l r =
		try
			let line = input_line ic in
			let split = String.split_on_char ' ' line in
			fn
				((List.nth split 0) :: l)
				((List.nth split 1) :: r)
		with End_of_file ->
			close_in ic;
			(List.rev l, List.rev r)
	in
	fn [] []
*)

(*
let lines = read_lines "test.txt"

let () = print_endline ""
let () = print_endline (String.concat "\n" lines)
*)

(*
let read_input filename =
	let ic = open_in filename in
	let rec fn acc =
		try
			let line = input_line ic in
			let split = String.split_on_char ' ' line in
			let l = int_of_string (List.nth split 0) in
			let r = int_of_string (List.nth split 1) in
			fn ((l, r) :: acc)
		with End_of_file ->
			close_in ic;
			List.rev acc
	in
	fn []
*)

let read_input filename =
	let ic = open_in filename in
	let rec fn (lacc : int list) (racc : int list) =
		try
			let line = input_line ic in
			let split = String.split_on_char ' ' line in
			let l : int = int_of_string (List.nth split 0) in
			let r : int = int_of_string (List.nth split 1) in
			fn (l :: lacc) (r :: racc)
		with End_of_file ->
			close_in ic;
			(List.sort compare lacc, List.sort compare racc)
	in
	fn [] []

let rec sum l =
	match l with
	| [] -> 0
	| hd :: tl -> hd + (sum tl)


let () =
	let lr = read_input "test.txt" in
	let zipped = List.combine (fst lr) (snd lr) in
	let distances = List.map (fun pair -> abs ((fst pair) - (snd pair))) zipped in
	print_endline (string_of_int (sum distances))
	(*
	print_endline (String.concat ", " (List.map (fun x -> string_of_int x) distances))
	*)

(*
let pairs = read_input "test.txt"
let stringed_pairs = List.map
	(fun lr -> (string_of_int (fst lr)) ^ " " ^ (string_of_int (snd lr)))
	pairs
let () = print_endline (String.concat "\n" stringed_pairs)
*)

