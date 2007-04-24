type t = {
mutable morphtable_file : string;
mutable model_file : string;
mutable max_suffix_length : int;
mutable max_freq_for_guess : int;
mutable test_file : string;
mutable train_file : string;
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
let set_test_file conf value = conf.test_file <- value ;;
let get_test_file conf  = conf.test_file ;;
let set_train_file conf value = conf.train_file <- value ;;
let get_train_file conf  = conf.train_file ;;
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
let parse_file filename =
  let ic = open_in filename in
  let morphtable_file = ref None in 
  let model_file = ref None in 
  let max_suffix_length = ref None in 
  let max_freq_for_guess = ref None in 
  let test_file = ref None in 
  let train_file = ref None in 
  let tag_order = ref None in 
  let emission_order = ref None in 
  let process_entry lineno key value = 
     match key with
       | "morphtable_file" -> morphtable_file := Some value       | "model_file" -> model_file := Some value       | "max_suffix_length" -> max_suffix_length := Some value       | "max_freq_for_guess" -> max_freq_for_guess := Some value       | "test_file" -> test_file := Some value       | "train_file" -> train_file := Some value       | "tag_order" -> tag_order := Some value       | "emission_order" -> emission_order := Some value       | other -> failwith ("unknown option entry " ^ other)
       in
        iterate_lines ic process_entry;
let morphtable_file = match !morphtable_file with 
  None -> failwith ("no morphtable_file specified in the config file") 
 | Some v ->  v
 in 
let model_file = match !model_file with 
  None -> failwith ("no model_file specified in the config file") 
 | Some v ->  v
 in 
let max_suffix_length = match !max_suffix_length with 
  None -> failwith ("no max_suffix_length specified in the config file") 
 | Some v ->  int_of_string v
 in 
let max_freq_for_guess = match !max_freq_for_guess with 
  None -> failwith ("no max_freq_for_guess specified in the config file") 
 | Some v ->  int_of_string v
 in 
let test_file = match !test_file with 
  None -> failwith ("no test_file specified in the config file") 
 | Some v ->  v
 in 
let train_file = match !train_file with 
  None -> failwith ("no train_file specified in the config file") 
 | Some v ->  v
 in 
let tag_order = match !tag_order with 
  None -> failwith ("no tag_order specified in the config file") 
 | Some v ->  int_of_string v
 in 
let emission_order = match !emission_order with 
  None -> failwith ("no emission_order specified in the config file") 
 | Some v ->  int_of_string v
 in 

 {
 morphtable_file = morphtable_file;
model_file = model_file;
max_suffix_length = max_suffix_length;
max_freq_for_guess = max_freq_for_guess;
test_file = test_file;
train_file = train_file;
tag_order = tag_order;
emission_order = emission_order;

} let save conf filename = 
  let oc = open_out filename in
Printf.printf "morphtable_file=%s\n" ( conf.morphtable_file);
Printf.printf "model_file=%s\n" ( conf.model_file);
Printf.printf "max_suffix_length=%s\n" (string_of_int conf.max_suffix_length);
Printf.printf "max_freq_for_guess=%s\n" (string_of_int conf.max_freq_for_guess);
Printf.printf "test_file=%s\n" ( conf.test_file);
Printf.printf "train_file=%s\n" ( conf.train_file);
Printf.printf "tag_order=%s\n" (string_of_int conf.tag_order);
Printf.printf "emission_order=%s\n" (string_of_int conf.emission_order);
  close_out oc