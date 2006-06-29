
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
				(word, gold) :: read_sentence false (* már tudjuk, hogy nem üres a mondat *)
		in
					read_sentence true
;;

(* vegigmegy a chan minden mondatan is atadja az f-nek*)
let rec iter_sentence chan f =
try 
	f (read_sentence chan);
	iter_sentence chan f

with End_of_file -> ()

;;



let print_sentence sentence =
	let ngram = ["s"] in 
	let print_pair (word, gold) = 
		Printf.fprintf stdout "%s\t%s\n" word gold;
	
		 in
		List.iter (print_pair) sentence
;;
