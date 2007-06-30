(*
let read_sentence chan = 
	(* returns the next field of the TAB separated line and the next character 
		pos after the field *)
	
  let next_item line  start = 
  	let len = String.length line in
     	let next_tab_pos = 
        	try  
          	String.index_from line start '\t'
        	with 
          	| Not_found -> len
          	| Invalid_argument _ ->
               	Printf.eprintf "ERROR: not enough fields. Input line follows\n%s\n" line;
               	raise (Failure "not enough fields") 
				in
				
       			let item = String.sub line start (next_tab_pos - start) in
						(next_tab_pos +1), item
	
	in
		let rec read_sentence empty =   (* hívhatod ugyanúgy a rekurzív segédfüggvényt *)
			let line =
				try input_line chan
				with 
				 End_of_file -> match empty with
					                | true -> raise End_of_file
									| false -> ""
			in
			let last, word = next_item line 0 in    (* beolvassuk a szót, ha nincs akkor az end 0 lesz *)
			match word, empty with
			| "", true -> read_sentence true (* consume redundant newlines if sentence is empty yet *)
			| "", false -> []                    (* genuine end of non_empty sentence *)
			| _, _ ->                           (* next real item *)
				let _, gold =
					try
							 next_item line last
					with _ -> Printf.eprintf "invalid line %d %s\n" last line; failwith "invalid line"
				in
				(word, gold) :: read_sentence false (* we already know that the sentence is nonempty *)
		in
					read_sentence true
;;


let read_sentence_no_split chan =
		let rec read_sentence empty =   (* hívhatod ugyanúgy a rekurzív segédfüggvényt *)
			let line =
				try input_line chan
				with 
				 End_of_file -> match empty with
					                | true -> raise End_of_file
									| false -> ""
			in
		
			match String.length line, empty with
			| 0, true -> read_sentence true (* consume redundant newlines if sentence is empty yet *)
			| 0, false -> []                    (* genuine end of non_empty sentence *)
			| _, _ ->                           (* next real item *)
				line:: read_sentence false (* már tudjuk, hogy nem üres a mondat *)
		in
					read_sentence true
;;

(* goes through the sentences of chan, calling f for each sentence *)
let iter_sentence_no_split chan f =
	let rec loop () = 
		f (read_sentence_no_split chan);
		loop()
	in
	try
		loop () ;
	with End_of_file -> ()

;;


*)
let read_tagged_sentence chan =
	let rec aux wacc tacc read=
		try 
		match Parse.split2 '\t' (input_line chan) with
			word :: r when (String.length word) > 0 -> 
				let gold = match r with
					x :: r -> x
					| _ -> ""
				in
				aux (word :: wacc) (gold::tacc) true
			| _ -> (wacc, tacc)
		with End_of_file -> if read then (wacc, tacc) else raise End_of_file
	in
	aux ([]) ([]) false
	
let read_fielded_sentence chan =
	let rec aux acc read=
		try 
		let fields = Parse.split2 '\t' (input_line chan) in
		match fields with
			word :: r when (String.length word) > 0 -> 
			
				aux (fields :: acc)  true
			| _ -> acc
		with End_of_file -> if read then acc else raise End_of_file
	in
	aux [] false
	
let read_sentence chan =
	let rec aux wacc read=
		try 
		 let line = input_line chan in
		 if (String.length line) > 0 then 
				aux (line :: wacc) true
		 else wacc
		with End_of_file -> if read then wacc else raise End_of_file
	in
	aux ([])  false


let iter_tagged_sentence chan f =
	try
		while(true) do
			f (read_tagged_sentence chan)
		done;
	with End_of_file -> ()
			
	
let iter_sentence chan f =
	try
		while(true) do
			f (read_sentence chan)
		done;
	with End_of_file -> ()
	
let iter_fielded_sentence chan f =
	try
		while(true) do
			f (read_fielded_sentence chan)
		done;
	with End_of_file -> ()
	
(*

(* goes through the sentences of chan, calling f for each sentence *)
let iter_sentence chan f =
	let rec loop () = 
		f (read_sentence chan);
		loop()
	in
	try
		loop () ;
	with End_of_file -> ()

;;
*)
	
let rec fold_tagged_sentence f a chan =
	 try
	 	let (words, tags) = read_tagged_sentence chan in
		let sentence = List.combine words tags in
	 	fold_tagged_sentence f (f a sentence) chan
	with End_of_file -> a

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"

(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.

   Return true iif the original string had any uppercase charater.
 *)
let lowercase s =
	let changed = ref false in
	let l = String.length s in
	if l = 0 then (s, false) else begin
		let r = String.create l in
	    for i = 0 to l - 1 do
		 	let c = unsafe_get s i in
			let c' = Char.lowercase c in
			unsafe_set r i c';
			if not !changed && c != c' then
			 changed := true
		done;
        (r, !changed)
	 end
