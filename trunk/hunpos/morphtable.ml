module SSet = Set.Make(String)


(* Split a string into a list of substrings based on a delimiter character *)
let split c str = 
  let rec aux s acc = 
    try  let ind=String.index s c in
         aux (String.sub s (ind+1) ((String.length s) - ind -1 )) 
              ((String.sub s 0 ind)::acc)       
    with Not_found -> List.rev (s::acc) 
  in aux str [];;

let load file = 
	let ic = open_in file in (* "data/hunmorph.cache" *)
	let table = Hashtbl.create 100404 in
	try	
	while ( true ) do
		let line = input_line ic in
		let _ = 
			match (split '\t' line)  with
				| word :: [] -> ()
                | word::anals ->
								 
	 							  let tags = List.fold_left (fun s a -> SSet.add a s) SSet.empty anals in
								   Hashtbl.replace table word (SSet.elements tags) 
			
				| _ -> failwith ("line error: " ^ line)
		in () 
	done ;
	table
	with  End_of_file -> table
	
let analyze table word =
	let res = Hashtbl.find table word in
		if List.length res = 0 then raise Not_found else res