let () = print_endline "ok"

let read_input filename =
	let ic = open_in filename in
	let rec fn lacc racc =
		try
			let line = input_line ic in
			let split = String.split_on_char ' ' line in
			let l = int_of_string (List.nth split 0) in
			let r = int_of_string (List.nth split 3) in
			fn (l :: lacc) (r :: racc)
		with End_of_file ->
			close_in ic;
			(List.sort compare lacc, List.sort compare racc)
	in
	fn [] []

let () =
	let lr = read_input "input.txt" in
	let zipped = List.combine (fst lr) (snd lr) in
	let distances = List.map (fun pair -> abs ((fst pair) - (snd pair))) zipped in
	let similarities = List.map
		(fun e -> e * (List.length (List.filter (fun x -> x = e) (snd lr))))
		(fst lr)
	in
	print_endline "Part 1:";
	print_endline (string_of_int (List.fold_left (+) 0 distances));
	print_endline "Part 2:";
	print_endline (string_of_int (List.fold_left (+) 0 similarities))

