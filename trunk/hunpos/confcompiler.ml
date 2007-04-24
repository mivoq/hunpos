let ic = stdin 
;;

let oc_config = open_out "config.ml"
let oc_parser = open_out "config_parser"

let parse_config_spec =
	let acc = ref [] in
	let lineno = ref 0 in
	let rec loop  ()  =
		let line = (input_line ic) in
		incr lineno;
		let fields = Parse.split2 '\t' line in
		match fields with
			name :: typ:: def :: [] ->
				acc := (name, typ, def ) :: !acc; 
				loop ()
			| _ -> failwith ("can't parse line no: " ^ (string_of_int !lineno))
	in
	try
		loop ()
	with End_of_file -> !acc
;;


		
let _ =
	

	let config_spec = parse_config_spec in
	(* record kiirasa *)
	Printf.fprintf oc_config "type t = {\n";
	List.iter (fun (name, typ, def) ->
		Printf.fprintf oc_config "mutable %s : %s;\n" name typ;
	) config_spec;
	Printf.fprintf oc_config "}\n";
	
	(* set es get fuggvenyek *)
	List.iter (fun (name, typ, def) ->
		Printf.fprintf oc_config "let set_%s conf value = conf.%s <- value ;;\n" name name;
		Printf.fprintf oc_config "let get_%s conf  = conf.%s ;;\n" name name;
	) config_spec;


let s = 	
"	let is_comment l =\n" ^ 
"		let len = String.length l in\n" ^ 
"		let rec aux ix =\n" ^ 
"			if ix >= len then true\n" ^ 
"			else\n" ^ 
"			if l.[ix] = ' ' || l.[ix] = '\t' then\n" ^ 
"				aux (ix + 1)\n" ^ 
"			else if l.[ix] = '#' then true\n" ^ 
"			else false\n" ^ 
"		in\n" ^ 
"		aux 0\n" ^ 
"	;;\n" ^ 
"	\n" ^ 
"	let split c str =\n" ^ 
"		let rec aux acc idx =\n" ^ 
"         	try let idx' = String.index_from  str idx c in\n" ^ 
"               aux  ((String.sub str idx (idx' - idx))::acc) (succ idx')\n" ^ 
"     	with Not_found -> List.rev ((String.sub str idx (String.length str - idx)) :: acc)\n" ^ 
"       	in\n" ^ 
"    aux [] 0\n" ^ 
"\n" ^ 
"	\n" ^ 
"	let iterate_lines ic f =\n" ^ 
"		let lineno = ref 0 in\n" ^ 
"		try\n" ^ 
"		while(true) do\n" ^ 
"			let line = input_line ic in\n" ^ 
"			incr lineno;\n" ^ 
"			if  is_comment line then ()\n" ^ 
"			else\n" ^ 
"			match split '=' line with\n" ^ 
"				key::value::[] -> f !lineno key value\n" ^ 
"			   | _ -> failwith (\"can't parse line no: \" ^ (string_of_int !lineno)) \n"  ^ 
"		\n" ^ 
"		done;\n" ^ 
"		with End_of_file -> ()\n" in

Printf.fprintf oc_config "%s" s;
	
Printf.fprintf oc_config "let parse_file filename =\n";
Printf.fprintf oc_config "  let ic = open_in filename in\n";
	
List.iter (fun (name, typ, def) -> 
Printf.fprintf oc_config "  let %s = ref None in \n" name;	
	)	config_spec;
	
Printf.fprintf oc_config "  let process_entry lineno key value = \n     match key with\n";

List.iter (fun (name, typ, def) -> 
Printf.fprintf oc_config "       | \"%s\" -> %s := Some value" name name;	
	)	config_spec;
Printf.fprintf oc_config "       | other -> failwith (\"unknown option entry \" ^ other)\n" ;
Printf.fprintf oc_config "       in\n        iterate_lines ic process_entry;\n";
List.iter (fun (name, typ, def) -> 
	Printf.fprintf oc_config "let %s = match !%s with \n " name name;
	Printf.fprintf oc_config " None -> failwith (\"no %s specified in the config file\") \n " name; 
	Printf.fprintf oc_config "| Some v -> ";
	let s =
	match typ with
		"int" -> "int_of_string v"
		| "string" -> "v"
		| _ -> failwith ("no conversion for type : " ^ typ)
	in
	Printf.fprintf oc_config " %s\n in \n" s;
) config_spec;
Printf.fprintf oc_config "\n {\n ";
List.iter (fun (name, typ, def) -> 
Printf.fprintf oc_config "%s = %s;\n" name name;
) config_spec;

				
Printf.fprintf oc_config "\n} ";


output_string oc_config "let save conf filename = \n";	
output_string oc_config "  let oc = open_out filename in\n";
	List.iter (fun (name, typ, def) -> 
	output_string oc_config "Printf.printf \"";
	output_string oc_config name;
	output_string oc_config "=%s\\n\" ";
	let s = 
	match typ with
		"int" -> "(string_of_int"
		| "string" -> "("
		| _  -> failwith ("no conversion for type : " ^ typ)
	in
	output_string oc_config s;
	output_string oc_config " conf.";
	output_string oc_config name;
	output_string oc_config ");\n";
	) config_spec;


Printf.fprintf oc_config "  close_out oc";
	
		
    close_out oc_config;