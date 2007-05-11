module SHash = Mfhash.String

type parsed_anal = (string * string * string * string) list list
type t = (parsed_anal * bool * string list)
 SHash.t

let load file = 
	let ic = open_in file in (* "data/hunmorph.cache" *)
	let table = SHash.empty() in
	try	
	while ( true ) do
		let line = input_line ic in
		
		let _ = 
			match (Parse.split2 '\t' line)  with
				| word :: [] -> ()
				| word :: "" :: [] -> ()
                | word::anals ->
						
						let (guessed, anals) = Kr_parser.parse  anals in
						let value = if guessed then
										(* egyelore taggernel nem hasznaljuk
										   a guessed tageket *)
										(anals,guessed, [])
									else 
										(anals, guessed, Kr_parser.inflections anals)
						in
						let _ =
						SHash.update table word
							(fun () -> value)
							(fun ix -> failwith ("duplicated word in morphtable: " ^ word))
						in ()
				| _ -> failwith ("line error: " ^ line)
		in () 
	done ;
	table
	with  End_of_file -> table

let tags table word =
	let (anals, guessed, tags) = SHash.find table word in
		if List.length tags = 0 then raise Not_found else tags

let analyze table word =
	let (anals, guessed, tags) = SHash.find table word in
	(anals, guessed)