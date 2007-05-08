{
module Bv = Array
module P = Dic_parser
(* module StringTrie = Trie_in_situ.Make( Map.Make( Char ) ) *)
(* module StringTrie = Trie_in_situ.Make( Charmap ) *)

exception Invalid_flag
let global_c = ref None
let global_free () = 
  match !global_c with Some(p) -> p | None ->
    let c = Constraint.Alt [!Parser_common.pseudoroot_flag] in
    global_c := Some(c);
    c

let state = ref 0
}
let identchar = [^'#' ' ' '\t' '\n' '\r']
let ws = [' ' '\t']
let wc = [^' ' '\t']
let newline = ['\n' '\r']

rule token preftrie sufftrie chars char_count tag_hash flaghash flag_tbl maxflags flag_length = parse 
| ws+  { token preftrie sufftrie chars char_count tag_hash flaghash flag_tbl maxflags flag_length lexbuf }      (* skip blanks *)
| '#'[^'\n' '\r']*newline      { 
  Utils.carp 4 "COMMENT (skip)\n";
  token preftrie sufftrie chars char_count tag_hash flaghash flag_tbl maxflags flag_length lexbuf 
}     (* skip comments *)
| ( ws* newline )+ { 
  state := 0;
  Utils.carp 4 "\n";
  P.EOL 
}
| "ENDINPUT:" | eof              { P.EOF }
| identchar+ as x { 
  match !state with 

  | 0 -> (* stem *)
      Utils.lazy_carp 4 (lazy (Printf.eprintf "STEM(%s) " x));
      state := 2;

      (* let pseudoroot_index = !Parser_common.pseudoroot_flag in *)
      let m,bv =
	try 
	  let slash_index = String.index x '/' in
	  let m = String.sub x 0 slash_index in
	  let mend = slash_index + 1 in
	  let len = String.length x - mend in
	  let x = String.sub x mend len in
	  let bv = 
	    try 
	      Hashtbl.find flag_tbl x 
	    with Not_found ->
	      let bv = Bv.make maxflags false in 
	      let pseudoroot_index = !Parser_common.pseudoroot_flag in
	      let rec scan pos = 
		if pos < len then ( 
		  flush stderr;
		  let flag = String.sub x pos flag_length in
		  let _  = 
		    try 
		      let flag_index = Hashtbl.find flaghash flag in
		      Bv.unsafe_set bv flag_index true;
		    with Not_found -> 
		      Utils.carp 0 "WARNING: invalid flag '%s' (skipped)\n" flag;
		      (* raise Invalid_flag *)
		  in
		  scan (pos + flag_length)
		 )
	      in
	      scan 0 ;
	      Bv.set bv pseudoroot_index (not (Bv.get bv pseudoroot_index));
	      let bv = Constraint.Bv bv in
	      Hashtbl.add flag_tbl x bv ;
	      bv
	  in
	  m,bv
	with Not_found -> 
	  x,
	  global_free()
	    (* only the free (inverse) flag is set *)
      in
      let cap, x = !Parser_common.normalize m false chars char_count (* suffix *) in
      P.NODEINFO(sufftrie, x, [] (* endnode *), cap, bv);
  | 2 | 3 ->
      Utils.lazy_carp 4 (lazy (Printf.eprintf "TAG(%s) " x));
      state := 3;
      let tag = 
	try 
	  Hashtbl.find tag_hash x
	with Not_found ->
	  Hashtbl.add tag_hash x x;
	  x
      in 
      P.TAG(tag)

  | _ ->  raise (Failure "oops")
}

	  
      
      
      

