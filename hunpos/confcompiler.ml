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
"		with End_of_file -> ()\n" ^
"      let do_replaces tbl =\n" ^
"      		\n" ^
"      	let do_entry key value =" ^
"      		let rxp = Str.regexp \"#{\\\\([^}]+\\\\)}\" in\n" ^
"      		\n" ^
"      		let _ = Str.search_forward rxp value 0 in\n" ^
"      	   	\n" ^
"      		let k = Str.matched_group 1 value in\n" ^
"      		try\n" ^
"      			let erre = Hashtbl.find tbl k in\n" ^
"      	        let ez = (String.sub value 0 (Str.match_beginning () )) ^ erre ^\n" ^
"                           (String.sub value (Str.match_end ()) (String.length value - Str.match_end ()) ) in\n" ^
"      			\n" ^
"      			Hashtbl.replace tbl key ez;\n" ^
"      		with Not_found -> failwith (\"not known token: \" ^ k)\n" ^
"      	in\n" ^
"      	Hashtbl.iter (fun k v -> try do_entry k v with Not_found -> ()) tbl\n" ^
"      ;;	\n" ^
"      	\n" ^
"      let parse_file filename =\n" ^
"        let ic = open_in filename in\n" ^
"        let map = Hashtbl.create 10 in\n" ^
"        iterate_lines ic (fun lineno key value -> Hashtbl.replace map key value);\n" ^
"        do_replaces map ; \n" in

Printf.fprintf oc_config "%s" s;
	
List.iter (fun (name, typ, def) -> 
	let s =
	match typ with
		"int" -> "int_of_string"
		| "string" -> ""
		| _ -> failwith ("no conversion for type : " ^ typ)
	in

Printf.fprintf oc_config "  let %s = try %s ( Hashtbl.find map \"%s\" ) \n" name s name;
Printf.fprintf oc_config "           with Not_found -> failwith (\"no %s specified in the config file\")\n " name;
Printf.fprintf oc_config "  in\n";	
	)	config_spec;
	

Printf.fprintf oc_config "\n {\n ";
List.iter (fun (name, typ, def) -> 
Printf.fprintf oc_config "%s = %s;\n" name name;
) config_spec;

				
Printf.fprintf oc_config "\n} ";



	
output_string oc_config "let print_values oc conf  = \n";	
	List.iter (fun (name, typ, def) -> 
	output_string oc_config "Printf.fprintf oc \"";
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
	output_string oc_config "()\n";
	
	output_string oc_config "let save conf filename entries = \n";
	output_string oc_config "  let oc = open_out filename in\n";
	output_string oc_config "  List.iter (fun (k, v) -> Printf.fprintf oc \"%s=%s\n\" k v) entries;\n";
	output_string oc_config "  print_values oc conf;\n ";

	output_string oc_config "  close_out oc";
	
		
    close_out oc_config;