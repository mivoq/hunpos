(* TAB separeted freq lexikon kezelő
	*)
	
	

module Mfhash = Mfhash.String

 
let field_no = 0 
	
let iterate_lines ic f =
	try
		while (true) do
			let line = input_line ic in
			let fields = Parse.split2 '\t' line in
			let token = 
				if field_no = 0 then
					List.hd fields
				else
					List.nth fields field_no
			in
			f line token		
		done
	with End_of_file -> ()

let create_flex ic =
	let lex = Mfhash.empty () in
	let add_line line token =
		Mfhash.update lex token (fun () -> 1) (succ)
	in
	iterate_lines ic add_line;
	lex


let string_compare (s1:string) (s2:string) = compare s1 s2 
	

let print_flex_entry token freq number_printer =
		print_string token;
		print_char '\t';
		number_printer freq;
		print_newline ()
;;


let print_flex_entry_int token freq =
		print_flex_entry token freq (print_int)
;;

let print_flex_entry_float token freq =
		print_flex_entry token freq (print_float)
;;


let print_flex lex =
	Mfhash.sorted_iter string_compare print_flex_entry_int lex


let rec read_next_flex_entry ic =
	let line = input_line ic in
	let fields = Parse.split2 '\t' line in
	match fields with
		token::freq::tails -> let freq = int_of_string freq in
							  (token, freq)
		| _ -> read_next_flex_entry ic

		
let merge_two_files f ic1 ic2 =
	let fetch_next ic = try Some (read_next_flex_entry ic) with End_of_file -> None in
	
	let prev_left = fetch_next ic1 in
    let prev_right = fetch_next ic2 in
	
	let rec loop prev_left prev_right =
		match prev_left, prev_right with
			None, None -> (* mindket fajl vege *) ()
		  | Some(token1, freq1), None -> 
					f token1 (Some freq1) None;	
					let prev_left = fetch_next ic1 in  
					loop prev_left prev_right;
					
		  | None, Some(token, freq) -> 
					f token (None) (Some freq);
					let prev_right = fetch_next ic2 in
					loop prev_left prev_right;
		  | Some(token1, freq1), Some(token2, freq2) ->
			       if token1 = token2 then begin
						f token1 (Some freq1) (Some freq2);
						let prev_left = fetch_next ic1 in
					    let prev_right = fetch_next ic2 in
						loop prev_left prev_right;
				   end else
				   if token1 < token2 then begin
						f token1 (Some freq1) (None);
						let prev_left = fetch_next ic1 in
						loop prev_left prev_right;
				   end else begin
						f token2 (None) (Some freq2);
						let prev_right = fetch_next ic2 in
						loop prev_left prev_right;
				   end
	in
	loop prev_left prev_right									
		
	

	
let divide_lex_by ic1 ic2 =
	let divide token freq1 freq2 =
		let ratio = match freq1, freq2 with
			| Some(freq1), None -> (float_of_int freq1)
			| None, Some(freq2) -> 0.0
			| Some(freq1), Some freq2 -> (float_of_int freq1) /. (float_of_int freq2)	
			| None, None -> failwith("Merging resulted in two Nones. Consult the programmer!")
		in
		if ratio > 0.0 then
			print_flex_entry_float token ratio ;
	in
	merge_two_files divide ic1 ic2 
	
let add_lexicons ic1 ic2 =
	let add token freq1 freq2 =
		let freq =
			match freq1, freq2 with
				| Some(freq1), None -> freq1
				| None, Some(freq2) -> freq2
				| Some(freq1), Some freq2 -> freq1 + freq2	
				| None, None -> failwith("Merging resulted in two Nones. Consult the programmer!")
		in
		print_flex_entry_int token freq
	in
	merge_two_files add ic1 ic2
	
(* main cuccok *****)
let open_chan_in_argv n =
	if Array.length Sys.argv < n + 1 then
		failwith("not enough arguments")
	else
	let filename = Sys.argv.(n) in
	if filename = "-" then stdin
	else open_in filename
	
let usage_and_exit () =
	Printf.eprintf "usage : %S command \n" Sys.argv.(0);
	exit(-1);
;;
	
let _ =
	if Array.length Sys.argv < 2 then
		usage_and_exit ()
	else
	match Sys.argv.(1) with
		| "create" -> 
			begin
				let ic = open_chan_in_argv 2 in
				let flex = create_flex ic in
				print_flex flex;
				close_in ic;
			end
		| "add" -> 
			let ic1 = open_chan_in_argv 2 in
			let ic2 = open_chan_in_argv 3 in
			add_lexicons ic1 ic2 ;
		| "divide_by" ->
			let ic1 = open_chan_in_argv 2 in
			let ic2 = open_chan_in_argv 3 in
			divide_lex_by ic1 ic2 ;
					
		| _ -> Printf.eprintf "not known command"; exit (-1);
