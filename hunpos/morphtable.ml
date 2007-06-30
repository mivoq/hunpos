(** This module loads the morphtable (words with possible tags) from
    a file to a Set. The format is:
    1. one word per line
    2. word TAB tag1 TAB tag2
    
 *)
module SSet = Set.Make(String)



let load file = 
	let ic = open_in file in (* "data/hunmorph.cache" *)
	let table = Hashtbl.create 100404 in
	try	
	while ( true ) do
		let line = input_line ic in
		let _ = 
			match (Parse.split2 '\t' line)  with
				| word :: [] -> ()
				| word :: "" :: [] -> ()
                | word::anals ->
                    let tags = List.fold_left (fun s a -> SSet.add a s) SSet.empty anals in
                    Hashtbl.replace table word (SSet.elements tags) 
				| _ -> failwith ("line error: " ^ line)
		in () 
	done ;
	table
	with  End_of_file -> table
	
let tags table word =
	let res = Hashtbl.find table word in
		if List.length res = 0 then raise Not_found else res