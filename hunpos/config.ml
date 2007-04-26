type t = {
mutable morphtable_file : string;
mutable model_file : string;
mutable max_suffix_length : int;
mutable max_freq_for_guess : int;
mutable tag_order : int;
mutable emission_order : int;
}
let set_morphtable_file conf value = conf.morphtable_file <- value ;;
let get_morphtable_file conf  = conf.morphtable_file ;;
let set_model_file conf value = conf.model_file <- value ;;
let get_model_file conf  = conf.model_file ;;
let set_max_suffix_length conf value = conf.max_suffix_length <- value ;;
let get_max_suffix_length conf  = conf.max_suffix_length ;;
let set_max_freq_for_guess conf value = conf.max_freq_for_guess <- value ;;
let get_max_freq_for_guess conf  = conf.max_freq_for_guess ;;
let set_tag_order conf value = conf.tag_order <- value ;;
let get_tag_order conf  = conf.tag_order ;;
let set_emission_order conf value = conf.emission_order <- value ;;
let get_emission_order conf  = conf.emission_order ;;
	let is_comment l =
		let len = String.length l in
		let rec aux ix =
			if ix >= len then true
			else
			if l.[ix] = ' ' || l.[ix] = '	' then
				aux (ix + 1)
			else if l.[ix] = '#' then true
			else false
		in
		aux 0
	;;
	
	let split c str =
		let rec aux acc idx =
         	try let idx' = String.index_from  str idx c in
               aux  ((String.sub str idx (idx' - idx))::acc) (succ idx')
     	with Not_found -> List.rev ((String.sub str idx (String.length str - idx)) :: acc)
       	in
    aux [] 0

	
	let iterate_lines ic f =
		let lineno = ref 0 in
		try
		while(true) do
			let line = input_line ic in
			incr lineno;
			if  is_comment line then ()
			else
			match split '=' line with
				key::value::[] -> f !lineno key value
			   | _ -> failwith ("can't parse line no: " ^ (string_of_int !lineno)) 
		
		done;
		with End_of_file -> ()
      let do_replaces tbl =
      		
      	let do_entry key value =      		let rxp = Str.regexp "#{\\([^}]+\\)}" in
      		
      		let _ = Str.search_forward rxp value 0 in
      	   	
      		let k = Str.matched_group 1 value in
      		try
      			let erre = Hashtbl.find tbl k in
      	        let ez = (String.sub value 0 (Str.match_beginning () )) ^ erre ^
                           (String.sub value (Str.match_end ()) (String.length value - Str.match_end ()) ) in
      			
      			Hashtbl.replace tbl key ez;
      		with Not_found -> failwith ("not known token: " ^ k)
      	in
      	Hashtbl.iter (fun k v -> try do_entry k v with Not_found -> ()) tbl
      ;;	
      	
      let parse_file filename =
        let ic = open_in filename in
        let map = Hashtbl.create 10 in
        iterate_lines ic (fun lineno key value -> Hashtbl.replace map key value);
        do_replaces map ; 
  let morphtable_file = try  ( Hashtbl.find map "morphtable_file" ) 
           with Not_found -> failwith ("no morphtable_file specified in the config file")
   in
  let model_file = try  ( Hashtbl.find map "model_file" ) 
           with Not_found -> failwith ("no model_file specified in the config file")
   in
  let max_suffix_length = try int_of_string ( Hashtbl.find map "max_suffix_length" ) 
           with Not_found -> failwith ("no max_suffix_length specified in the config file")
   in
  let max_freq_for_guess = try int_of_string ( Hashtbl.find map "max_freq_for_guess" ) 
           with Not_found -> failwith ("no max_freq_for_guess specified in the config file")
   in
  let tag_order = try int_of_string ( Hashtbl.find map "tag_order" ) 
           with Not_found -> failwith ("no tag_order specified in the config file")
   in
  let emission_order = try int_of_string ( Hashtbl.find map "emission_order" ) 
           with Not_found -> failwith ("no emission_order specified in the config file")
   in

 {
 morphtable_file = morphtable_file;
model_file = model_file;
max_suffix_length = max_suffix_length;
max_freq_for_guess = max_freq_for_guess;
tag_order = tag_order;
emission_order = emission_order;

} let print_values oc conf  = 
Printf.fprintf oc "morphtable_file=%s\n" ( conf.morphtable_file);
Printf.fprintf oc "model_file=%s\n" ( conf.model_file);
Printf.fprintf oc "max_suffix_length=%s\n" (string_of_int conf.max_suffix_length);
Printf.fprintf oc "max_freq_for_guess=%s\n" (string_of_int conf.max_freq_for_guess);
Printf.fprintf oc "tag_order=%s\n" (string_of_int conf.tag_order);
Printf.fprintf oc "emission_order=%s\n" (string_of_int conf.emission_order);
()
let save conf filename entries = 
  let oc = open_out filename in
  List.iter (fun (k, v) -> Printf.fprintf oc "%s=%s
" k v) entries;
  print_values oc conf;
   close_out oc