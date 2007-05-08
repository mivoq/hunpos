{

module P = Aff_parser
module Bv = Array

let prefix = ref true
let cliplen = ref 0    
let clipstr = ref ""    
let append = ref ""
let state = ref 0
let var = ref ""
let mod_flag = ref ""

let process_flags flaghash flag_tbl flag_length flagcount maxflags x = 
  let len = String.length x in 
  let pseudoroot_index = !Parser_common.pseudoroot_flag in
    let bv = 
      try 
	Hashtbl.find flag_tbl x 
      with Not_found ->
	let bv = Bv.make maxflags false in 
	(* by default initiate chopping to stem by setting 
	   this flag one can cause the lemma to be the derived form *)
	let rec scan pos = 
	  if pos < len then (
	    let flag = String.sub x pos flag_length in
	    let flag_index = 
	      Parser_common.get_flag_index flagcount maxflags flaghash flag
	    in
	    Bv.unsafe_set bv flag_index true;
	    Utils.lazy_carp 4 (lazy (Printf.eprintf "%s (%d) " flag flag_index));
	    scan (pos + flag_length)
	   )
	in
	scan 0 ;
	Utils.carp 4 "\n";
	(* the inverse flag *)
	(* flag_index <> pseudoroot_index *); 
	(* set to false if root only flag otherwise true *) 
	Bv.set bv pseudoroot_index (not (Bv.get bv pseudoroot_index));
	let bv = Constraint.Bv bv in
	Hashtbl.add flag_tbl x bv;
	bv
    in
    bv

let make_charset_list string len reverse = 
  try 
    let clip, string = 
      if reverse 
      then 
	let ind0 = try String.index string '.' with Not_found -> len in 
	let ind1 = String.index string '[' in 
	let ind = min ind0 ind1 in
	String.sub string 0 ind, 
	String.sub string ind (len - ind)
      else 
	let ind0 = try String.rindex string '.' with Not_found -> 0 in 
	let ind1 = String.rindex string ']' in 
	let ind = max ind0 ind1 in
	String.sub string (ind + 1) (len - ind - 1), 
	String.sub string 0 (ind + 1)
    in
    let len = String.length string in
    let rec make_charset_list_rec pos list = 
      if pos = len then list else (
      let char = string.[pos] in
      let next_pos, cond = 
	if char = '[' then (
	  let close = 
	    try 
	      String.index_from string pos ']'
	    with Not_found -> raise Parsing.Parse_error
	  in
	  let bit, pos = 
	    if string.[succ pos] = '^' then false, pos + 2 else true, pos + 1 
	  in
	  let set = String.sub string pos (close - pos) in
	  succ close, 
	  Some(bit, set)
	 ) else 
	  (*s For nonsets, if the character is '.' the all true function is returned (matches 
	     any character), otherwise [(=) char] is returned (matches that character only)
	   *)
	  succ pos, 
	  if char = '.' 
	  then (* any character matches *)
	    None
	  else (* matches that character *)
	    Some (true, String.sub string pos 1)
      in
      make_charset_list_rec next_pos (cond :: list)
     )
    in
    let list = make_charset_list_rec 0 [] in
    let list = if reverse then List.rev list else list in
    clip, list
  with Not_found -> 
    string, []

}
let identchar = [^'#' ' ' '\t' '\n' '\r']
let ws = [' ' '\t']
let wc = [^' ' '\t']
let newline = ['\n' '\r']

rule token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char = parse 
| ws+     { token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf }      (* skip blanks *)
| ("PFX" | "SFX") ws+ wc+ ws+ wc+ ws+ ['0' - '9']+ ws* newline { 
  Utils.carp 4 "PREAMBLE (skip)\n";
  token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf
}      (* skip affixgroups preamble *)
| '#'[^'\n' '\r']*newline      { 
  Utils.carp 4 "COMMENT (skip)\n";
  token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
}     (* skip comments *)
 | ( ws* newline )+ { 
  Utils.carp 4 "\n";
  if !state > 1 then (Utils.carp 4 "EOL\n"; P.EOL)
  else ( state := 0;
	 token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
	)
}
| "ENDINPUT:" | eof              { P.EOF }
| "PFX" | "SFX" as x   { 
  Utils.lazy_carp 4 (lazy (Printf.eprintf "%s " x));
  state := 2; prefix := x = "PFX" ; P.AFFIX(x) 
}
| identchar+ as x { 
  match !state with
  | 0 -> (
      var := x; 
      let _ = 
	state := 
	  match x with 
	  | "FLAG" ->  -1  (* determines if flags are one or two chars long *)
	  | "FLAG_COUNT" -> -4 (* set flag bitv to flag_count length *)
	  | "GUESS_CHAR" -> -5 (* guessitems are looked under this char *)
	  | "COMPOUND" ->  -2 
	  | "SET" -> 1 (* we ignore character coding as yet *)
	  | _ -> 1 
      in
      Utils.lazy_carp 4 (lazy (Printf.eprintf "VAR '%s' " x));
      token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
     )
  | -2 -> (
      mod_flag := x;
      Utils.lazy_carp 4 (lazy (Printf.eprintf "COMPOUND_MOD '%s' " x));
      state := -3;
      token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
     )
  | -3 -> (
      Utils.lazy_carp 4 (lazy (Printf.eprintf "COMPOUND_CAT '%s' " x));
      let mod_flag = 
	Parser_common.get_flag_index flagcount !maxflags flaghash !mod_flag
      in
      let cat_flag = 
	Parser_common.get_flag_index flagcount !maxflags flaghash x
      in
      compound_flag_tbl := (mod_flag, cat_flag) :: !compound_flag_tbl;
      token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
     )
  | -4 -> (
      maxflags := 2 + int_of_string x;
      Utils.lazy_carp 4 (lazy (Printf.eprintf "FLAG_COUNT set to %d" !maxflags));
      token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
     )
  | -5 -> ( (* guess char *)
      guess_char := x.[0];
      Utils.lazy_carp 4 (lazy (Printf.eprintf "GUESS_CHAR set to '%c'" !guess_char));
      token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
  
     )
  | -1 -> (* flag layout *)
	let _ = match x with 
	| "long" -> flag_length := 2 
	| _ ->
	    Utils.carp 0 "ERROR: aff lexer: unknown flag layout '%s'\n" x;
	    raise Parsing.Parse_error;
	in 
	token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
    | 1 -> (* variable flag assignment *)
	Utils.lazy_carp 4 (lazy (Printf.eprintf " = %s\n" x));
      let _ = 
	try 
	  let control_flag = List.assoc !var control_flags in 
	  let i = Parser_common.get_flag_index flagcount !maxflags flaghash x in
	  control_flag := i;
	with Not_found ->
	  Utils.carp 1 "WARNING: aff parser: variable '%s' ignored\n" !var
      in
      (* state := 0; *)
      token preftrie sufftrie chars char_count flaghash flag_tbl flag_length flagcount maxflags control_flags compound_flag_tbl guess_char lexbuf 
      
  | 2 -> (* flag *)
      Utils.lazy_carp 4 (lazy (Printf.eprintf "FLAG(%s) " x));
      state := 3;
      let flag = Parser_common.get_flag_index flagcount !maxflags flaghash x in
      P.FLAG(flag)
  | 3 -> (* clip *)
      Utils.lazy_carp 4 (lazy (Printf.eprintf "CLIP(%s) " x));
      state := 4;
      if x = "0" then (
	clipstr := "";
	cliplen := 0
       ) else (
	clipstr := x;
	let len = String.length x in
	cliplen := len;
       );
      P.CLIP(!cliplen)
  | 4 -> (* morpheme *)
      Utils.lazy_carp 4 (lazy (Printf.eprintf "MORPH+FLAGS(%s) " x));
      state := 5;
      let x,bv =
	try 
	  let slash_index = String.index x '/' in
	  let m = String.sub x 0 slash_index in
	  let bv = 
	    process_flags flaghash flag_tbl !flag_length flagcount !maxflags 
	      ( String.sub x (slash_index + 1) (String.length x - slash_index - 1) ) 
	  in
	  m, bv
	with Not_found ->
	  let pseudoroot_index = !Parser_common.pseudoroot_flag in
	  x,
	  Constraint.Alt [pseudoroot_index] (* only the free (inverse) flag is set *)
      in
      append := x; 
      P.NODEINFO(x,bv);
  | 5 -> (* condition *)
      Utils.lazy_carp 4 (lazy (Printf.eprintf "COND(%s) " x));
      let x = if x = "." then "" else x in
      state := 6;
      let len = String.length x in
      let trie, (clip, char_constraints), reverse = 
	if !prefix 
	then preftrie, make_charset_list x len true  , true
	else sufftrie, make_charset_list x len false , false
      in
      let rest = 
	try
	  let len = String.length clip in
	  let elen = len - !cliplen in
	  let start0, start1 = 
	    if !prefix then 0, elen else elen, 0
	  in
	  let clip0 = String.sub clip start0 !cliplen in
	  if clip0 <> !clipstr then (
	    Utils.carp 0 "clipstring '%s' and pattern '%s' don't match" !clipstr clip0;
	    raise Parsing.Parse_error;
	   );
	  let rest = String.sub clip start1 elen in
	  rest
	with _ -> 
	  Utils.carp 0 "error reading pattern '%s'\n" x;
	  raise Parsing.Parse_error
      in
      let append = 
	if !append = "0" 
	then rest
	else if rest = "" then !append 
	else if !prefix then !append ^ rest 
	else rest ^ !append
      in
	(* I do not change this now for capitalization for the Kurdish proj *)
      let cap, append, clip, char_constraints = 
	(* ugly hack? to exchanging any letter A to a forces
	   global lowercasing on all letters... *)
	if !prefix 
	    && (let clen = String.length clip in 
	    let alen = String.length append in
	    clen = 1 && alen = 1) &&
	  clip.[0] = Char.uppercase append.[0]
	then Parser_common.Lowercase, "", "", 
          [Some (true, String.lowercase clip)]
	else Parser_common.NoCap, append, clip, char_constraints
      in

      let _, trie_key = !Parser_common.normalize append reverse chars char_count in
      let _, cont_key = !Parser_common.normalize clip reverse chars char_count in 
      let clip = if clip = "" then None else Some(clip) in
      P.COND(trie, trie_key, cont_key, clip, char_constraints, cap)
  | 6 ->
      Utils.lazy_carp 4 (lazy (Printf.eprintf "TAG(%s) " x));
      state := 6;
      P.TAG(x)
  | 10 -> 
      let cnt = 
	try 
	  int_of_string x 
	with _ ->
	  raise Parsing.Parse_error
      in P.COUNT cnt
  | _ ->  raise (Failure "oops")
}
	

	  
      
      
      

