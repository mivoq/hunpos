
type direction = Left | Right
type analyzer = Analyzer of (string -> int * (Tag.analysis list)) | Stemmer of (string -> Tag.analysis option)
type guess = Global | Fallback | NoGuess

exception Found of Tag.analysis 

let check_flags flag = 
  function 
    | Constraint.Bv bv -> 
	Array.get bv flag
    | Constraint.Alt list -> 
	List.exists ((=) flag) list

let join_compound_flags compound_flag_tbl = 
  fun compound_flags flags -> 
  let add_compound_flag compounds_flags (mod_flag, cat_flag) = 
    if check_flags cat_flag flags 
    then mod_flag :: compound_flags
    else compound_flags
  in
    List.fold_left (add_compound_flag) compound_flags compound_flag_tbl

let check_chars step = 
  let step = match step with Left -> -1 | Right -> 1 in
  let check_chars string pos list = 
    let rec check_chars pos = 
      function 
	| [] -> true
	| None :: tail -> check_chars (pos + step) tail
	| Some(true, set) :: tail -> 
	    String.contains set string.[pos]
	      && check_chars (pos + step) tail
	| Some(false, set) :: tail -> 
	    not (String.contains set string.[pos])
	      && check_chars (pos + step) tail
    in
    try check_chars pos list 
    with Invalid_argument _ -> false
  in
  check_chars
      
let check_compact_make = function
  | Left -> 
      let check_compact string lpos rpos morphs compaction = 
	let rec check_compact_chars (pos, mlist) = 
       	  function
	    | _ when pos < lpos ->
		None , mlist
	    | [] -> 
		Some (string.[pos],lpos, pred pos) , mlist
	    | (char, morphlist) :: tail -> (
		if char = string.[pos] 
		then 
		  let rpos = pred pos in
		  let mlist = 
		    if morphlist = [] then mlist 
		    else ((rpos, lpos, rpos), morphlist) :: mlist
		  in
		  check_compact_chars (rpos, mlist) tail
		else 
		  None, mlist
	       )
	in
	let mlist = if morphs = [] then [] else [((rpos, lpos, rpos), morphs)] in
	check_compact_chars (rpos, mlist) compaction
      in
      check_compact
     
  | Right -> 
      let check_compact string lpos rpos morphs compaction = 
	let rec check_compact_chars (pos, mlist) = 
	  function 
	    | _ when pos > rpos ->
		None, mlist
	    | [] -> 
		Some(string.[pos],succ pos, rpos), mlist
	    | (char, morphlist) :: tail -> (
		if char = string.[pos] 
		then 
		  let lpos = succ pos in
		  let mlist = 
		    if morphlist = [] then mlist
		    else ((lpos, lpos, rpos), morphlist) :: mlist in
		    check_compact_chars (lpos, mlist) tail
		  else 
		    None, mlist
	       )
	in
	let mlist = if morphs = [] then [] else [((lpos, lpos, rpos), morphs)] in
	check_compact_chars (lpos, mlist) compaction
      in
      check_compact
	
let get_analyses stop_at_first blocking lemma_present_flag step chars compound_flag_tbl want_to_analyze_compounds want_guess guess_flag root =

  let check_compact = check_compact_make step in
  let check_chars = check_chars step in
  let join_compound_flags = if want_to_analyze_compounds then join_compound_flags compound_flag_tbl else fun x y -> [] in
  let add_tag = 
    match step with 
      | Left -> let f t1 t2 l r = 
	  Tag.add_right t2 t1 r (* suffix, reading left, adding tags to the right side *)
	in f
      | Right -> let f t1 t2 l r =
	  Tag.add_left t2 t1 l (* prefix, reading right, adding tags to the left side *)
	in f
  in
  let clip_f = 
    match step with 
    | Left -> Tag.clip_right
    | Right -> Tag.clip_left
  in

  (* let join_tag = Tag.join in *)
  let print_tag = Tag.print in

  let rec get_analyses_rec
      prefnode suffnode
      prefhidden suffhidden 
      string cap
      lpos rpos
      stem_lpos stem_rpos
      lemma_present
      last_clip
      thisanalysis analyses
      last_flag
      last_cap
      (compound_ok, compound_flags)
      analyze
      = 
  
    let node = match step with Left -> suffnode | Right -> prefnode in
 
    assert ( Utils.lazy_carp 3 
	       (lazy (Printf.eprintf "string='%s'
		      lpos=%d, rpos=%d
		      trie=%s
		      last_flag=%d
		      stem_lpos=%d, stem_rpos=%d
		      prefhidden='%s', suffhidden='%s'\n"
		      
		 	string lpos rpos
			(match step with Left -> "suffix" | Right -> "prefix")
			last_flag 
			stem_lpos stem_rpos
	 		(match prefhidden with None -> "" | Some(p) -> p)
			(match suffhidden with None -> "" | Some(p) -> p)
 		     )); true );
    
     let morphs = StringTrie.S.morphs node in
     let compaction = StringTrie.S.compaction node in
	assert ( lpos <= rpos + 1 );
	assert ( Utils.lazy_carp 3 (lazy (Printf.eprintf "(%d, %d)\n" lpos rpos)); 
		 true ); 
	let descend, mlist = 
	  check_compact string lpos rpos morphs compaction
	in
	assert ( Utils.lazy_carp 3 (lazy (Printf.eprintf "%s \n" 
					    (match descend with 
					    | Some(c,lpos,rpos) -> Printf.sprintf "descend on '%c' with pos (%d, %d)" c lpos rpos 
					    | None -> "NOT descend"))
				   ); 
		 true ); 
	let this_blocking, analyses = (
	  match descend with 
	  | Some(char, lpos, rpos) ->  ( 
	      try
		let node = StringTrie.S.mfind char node in
		let prefnode, suffnode, prefhidden, suffhidden = 
		  match step with 
		  | Left ->
		      prefnode, 
		      node,
		      prefhidden, 
		      None
		  | Right ->
		      node,
		      suffnode, 
		      None, 
		      suffhidden
		in
		assert ( Utils.lazy_carp 3
			   (lazy (Printf.eprintf "-> [character '%c']\n" char)); true );
		get_analyses_rec
		  prefnode suffnode
		  prefhidden suffhidden 
		  string cap
		  lpos rpos
		  stem_lpos stem_rpos
		  lemma_present
		  last_clip
		  thisanalysis analyses
		  last_flag
		  last_cap
		  (compound_ok, compound_flags)
		  (analyze)
	      with Not_found -> (
		assert ( Utils.lazy_carp 3
			   (lazy (Printf.eprintf "* [no continuations with character '%c']\n" 
				    char)); true );
		(false, analyses)
	       )
	     )
	  | None -> (false, analyses)
	 )
	in
	let initial_zeros = !Parser_common.pseudoroot_flag <> last_flag in
	if mlist = [] || ( this_blocking && initial_zeros )
	then ( this_blocking, analyses )
	else (
	  let apply_to_morph (pos, lpos, rpos) = 
	    let apply_to_morph (this_blocking, analyses) = function 
	      | Constraint.Morph( morph_cap, tag, skip_key, clip_str, flag, flags, charsetlist ) 
		-> (

		  let clip = clip_f last_clip clip_str in
		  (* we have to inherit the clip from both sides *)
		  assert ( Utils.lazy_carp 2
			     (lazy (Printf.eprintf 
				      "(%d, %d) -> morph '%s' (flag %d, clipped '%s')\n"
				      lpos rpos
				      tag flag  
				      (Tag.clip_print clip)
				   )); true );
		  if (* check the flag *)
		    check_flags last_flag flags 
		|| ( compound_ok && List.exists (fun x -> check_flags x flags) compound_flags )
		  then (
		    if (* check character conditions *)
		      (charsetlist = [] || check_chars string pos charsetlist)
		    then (
		      let guess_item = want_guess && 
			(check_flags guess_flag flags)
		      in
		      let lexical = flag = 0 || guess_item in
		      let last_cap = Parser_common.cap_and (morph_cap, last_cap) in
		      let lemma_present, stem_lpos, stem_rpos, thisanalysis, blocking_analysis = 
			if lemma_present || blocking
			then (* to be read: tag is present, should not take string form to tag *)
			  let lemma_present_flag = check_flags lemma_present_flag flags in
			  let lemma_present = 
			    (not lemma_present_flag && not lexical) 
			  || (lemma_present_flag && lexical)
			  in
			  let tagged = add_tag tag thisanalysis lpos rpos in
			  let thisanalysis, blocking_analysis = 
			    match lemma_present, blocking with 
			    | true, true -> 
				tagged, (Tag.add_clip last_clip tagged)
			    | false, _ -> 
				let tagged_clip = (Tag.add_clip last_clip tagged)  in
				tagged_clip, tagged_clip
			    | true, false -> 
				tagged, tagged
			  in
			  let stem_lpos, stem_rpos = 
			    if lemma_present 
			    then lpos, rpos
			    else stem_lpos, stem_rpos
			  in
			  lemma_present, stem_lpos, stem_rpos, thisanalysis, blocking_analysis
			else 
			  (* if lemma_present if already false then there can be
			     no blocking environment *)
			  false, stem_lpos, stem_rpos, thisanalysis, thisanalysis
		      in
		      assert ( Utils.lazy_carp 2
				 (lazy (Printf.eprintf "this analysis set\n" 
					   ) ); true );
		      
		     
		      
		      let suffhidden = if step = Left then None else suffhidden in
		      let consumed = lpos > rpos || guess_item in
		      if consumed (* if the string is completely read *)
		      then (
			if prefhidden = None && suffhidden = None 
			then (
		   	if not lexical (* if it is not a dic item but an affix *)
			then this_blocking, analyses
			else (
			    let cap_ok = Parser_common.check_cap last_cap cap in
			    if cap_ok (* check capitalization *)
			    then (
			      let thisanalysis = 
				if lemma_present 
				then thisanalysis
				else (
				  let start, len = stem_lpos, (stem_rpos - stem_lpos + 1) in
				  let stem = String.sub string start len in
				  (* the guess items never preserve capitalization maybe they should : see news discussion 9/3/2006 on nlp.ner *)
				  let stem = Parser_common.recapitalize stem morph_cap in
				  assert ( Utils.lazy_carp 2
					     (lazy (Printf.eprintf "stem '%s'\n" 
						      (stem) ) ); true );
				  add_tag stem thisanalysis lpos rpos
				 )
			      in
			      (* let thisanalysis = join_tag thisanalysis in *)
			      assert ( Utils.lazy_carp 2
					 (lazy (Printf.eprintf "+ [found analysis '%s']\n" 
						   (print_tag thisanalysis) ) ); true );
				if stop_at_first then raise ( Found thisanalysis ) else
				  let no_of_analyses, analyses_list = analyses in
				  let analyses_list = thisanalysis :: analyses_list in
			      let analyses = no_of_analyses + 1, analyses_list in
			      assert ( Utils.lazy_carp 3 
					 (lazy (Printf.eprintf "Analyses sofar: %s\n"
						  ( String.concat "  " 
						      (List.rev_map (print_tag) analyses_list) 
						   )
					       )
					 ); 
				       true
				      );
			      (blocking, analyses)
			     ) else (
			      assert ( Utils.lazy_carp 2
					 (lazy (Printf.eprintf "* [capitalization mismatch]\n")); true );
			      (false, analyses)
			     )
			 )
			 ) else ( (* if there is sg left in hidden *)
			  let hiddenword = 
			      match prefhidden, suffhidden with 
			      | None, Some(s) -> s
			      | Some(p), Some(s) -> p ^ s
			      | Some(p), None -> p
			      | _, _ -> raise (Failure "oops")
			    in
			    let lpos , rpos = 0, ( String.length hiddenword ) - 1 in
			    if not lemma_present then (); (* FIXME: should compute stem from string *)
			    let analyses = analyze 
			      prefnode suffnode
			      None None
			      hiddenword cap
			      lpos rpos
			      lpos rpos
			      lemma_present
			      Tag.empty_clip
			      thisanalysis analyses
			      flag
			      last_cap
				(compound_ok, compound_flags)
			    in
			    (false, analyses)
			   )
		       ) else (   (* the string is not completely read descend on chars and there is no guessing *)
			(* this is the only disgusting bit... *)
			(*
			 * join all compound-category flags to cumulative compound category flags 
			 * if flag is lexical then set compound_ok component true
			   this makes sure that it doesn't matter if we compile compound-category
			   triggers as a separate morph (but we have to make sure the very same 
			   morph makes sure of proper suffixation)
			 *)
			let compound_flags = 
			  join_compound_flags compound_flags flags
			in
			if lexical && compound_flags = []
			    (* if no compound category is available, then a lexical morph must end the analysis *)
			then (this_blocking, analyses)
			else 
			(
			 let compound_ok = lexical in 
			 let lemma_present, thisanalysis, stem_lpos, stem_rpos = 
			   if lexical && want_to_analyze_compounds
			   then (* want_to_analyze_compounds *) (
			     let thisanalysis, stem_lpos, stem_rpos = 
			       if want_to_analyze_compounds
			       then (
				 let thisanalysis = 
				   if lemma_present 
				   then thisanalysis 
				   else (
				     assert ( Utils.lazy_carp 3
						(lazy (Printf.eprintf "setting stem\n" 
						      ) ); true );
				     let start, len = rpos + 1, (stem_rpos - rpos) in
				     assert ( Utils.lazy_carp 3
						(lazy (Printf.eprintf "start %d len %d \n" 
						      start len) ); true );
				     let stem = String.sub string start len in
				     assert ( Utils.lazy_carp 3
						(lazy (Printf.eprintf "before cap: stem '%s'\n" 
							 (stem) ) ); true );
				     let stem = Parser_common.recapitalize stem last_cap in
				     assert ( Utils.lazy_carp 3
						(lazy (Printf.eprintf "ok: stem '%s'\n" 
							 (stem) ) ); true );
				     add_tag stem thisanalysis lpos rpos
				    )
				 in
				 add_tag "+" thisanalysis lpos rpos, lpos, rpos
				)
			       else (
				 if lemma_present 
				 then thisanalysis, lpos, rpos
				 else thisanalysis, stem_lpos, stem_rpos
				)
			     in
			     want_to_analyze_compounds, thisanalysis, stem_lpos, stem_rpos
			    )
			   else (
			     lemma_present, thisanalysis, stem_lpos, stem_rpos 
			    )
			 in

			 let block_ok = (not blocking)
			|| 
			  let blocking_analysis = 
			    if lemma_present 
			    then thisanalysis
			    else (
			      let start, len = stem_lpos, (stem_rpos - stem_lpos + 1) in
			      let stem = String.sub string start len in
			      let stem = Parser_common.recapitalize stem last_cap in
			      assert ( Utils.lazy_carp 3
					 (lazy (Printf.eprintf "stem '%s'\n" 
						  (stem) ) ); true );
			      add_tag stem thisanalysis lpos rpos
			     )
			  in
			  (* let blocking_analysis = join_tag blocking_analysis in *)
			  let _, analyses_list = analyses in
			  not ( List.exists ((=) blocking_analysis) analyses_list )
			in
			if block_ok then (

			  let new_prefnode, new_suffnode, new_prefhidden, new_suffhidden = 
			    let node = 
			      StringTrie.S.node_find skip_key root
			    in
			    
			    match step with 
			    | Left -> 
				prefnode, node, prefhidden, clip_str
			    | Right -> node, suffnode, clip_str, suffhidden
			  in
			    let analyses = analyze
				new_prefnode new_suffnode 
				new_prefhidden new_suffhidden
				string cap
				lpos rpos 
				stem_lpos stem_rpos
				lemma_present
				clip
				thisanalysis analyses
				flag
				last_cap
				(compound_ok, compound_flags)
			    in (* this applies with non-lexical affixes 
				  but never in compaction *)
			    (false, analyses)

			 ) else (
			  assert ( Utils.lazy_carp 3
				     (lazy (Printf.eprintf "* [lexical form blocking]\n" )); true );
			  (false, analyses)
			 )
			) 
		       )
		     ) else (
		      assert ( Utils.lazy_carp 3
				 (lazy (Printf.eprintf "* [incompatible characters]\n" )); true );
		      (this_blocking, analyses)
		     )
		   ) else (
		      assert ( Utils.lazy_carp 3
				 (lazy (Printf.eprintf "* [incompatible with flag %d]\n" last_flag));
			       true );
		    (this_blocking, analyses)
		   )
		 )
	    in
	    apply_to_morph
      	  in
	  let apply_to_morphs (this_blocking, analyses) (pos, morphlist) = 
	    (* make a function for the position that iterates over morphs *)
	    if this_blocking 
		(* when there is are lexical morphs in the compaction list 
		 * inherit the fact of blocking
		 * AND not even bother to inspect the other (non-lexical) morphs
		 *)
	    then (this_blocking (* true *), analyses) 
	    else (
	    let apply_to_morph = apply_to_morph pos in
	    assert (  let _, analyses_list = analyses in
	    Utils.lazy_carp 3 
		       (lazy (Printf.eprintf "Analyses before: %s\n"
				( String.concat "  " (List.rev_map (print_tag) analyses_list ) )
			     )
		       ); true
		    );
            assert ( Utils.lazy_carp 3 
		       (lazy (Printf.eprintf " fold morphs\n" )); true );

	    let this_blocking, analyses = 
	      List.fold_left (apply_to_morph) (this_blocking (* false *), analyses) morphlist 
	    in
	    (* this_blocking can never become true 
	       if the compaction block does not end the string *)

	    assert ( let _, analyses_list = analyses in
	    Utils.lazy_carp 3 
	      (lazy (Printf.eprintf "Analyses after: %s\n"
		       ( String.concat "  " (List.rev_map (print_tag) analyses_list) )
		    )
	      ); true
		    );
	    this_blocking, analyses
	   )
	  in
	  let this_blocking, analyses = List.fold_left (apply_to_morphs) (false, analyses) mlist in
	  this_blocking, analyses
	 )
  in
  get_analyses_rec

 
  
 
let make_function stop_at_first blocking preftrie sufftrie pseudoroot_flag lemma_present_flag chars compound_flag_tbl want_to_analyze_compounds want_guess guess_flag guess_char
    =  

  let guess_trie = 
    try
      let guess_trie =
        StringTrie.S.mfind guess_char sufftrie ;
	(* | _ -> raise (Failure "expected final node") *)
      in
      Some(guess_trie)
    with Not_found -> None
  in

  let make guess = 
    
    let get_suffix_analyses = get_analyses stop_at_first blocking lemma_present_flag Left chars compound_flag_tbl want_to_analyze_compounds guess guess_flag sufftrie in
    let get_prefix_analyses = get_analyses stop_at_first blocking lemma_present_flag Right chars compound_flag_tbl want_to_analyze_compounds guess guess_flag preftrie in
    
  
    let rec analyze 
	prefnode suffnode
	prefhidden suffhidden
	string cap
	lpos rpos
	stem_lpos stem_rpos
	lemma_present
	last_clip
	thisanalysis analyses
	last_flag 
	last_cap 
	(compound_ok, compound_flags)
	= 
      let _, suffix = 
	get_suffix_analyses 
	  prefnode suffnode
	  prefhidden suffhidden
	  string cap
	  lpos rpos
	  stem_lpos stem_rpos
	  lemma_present
	  last_clip
	  thisanalysis analyses
	  last_flag 
	  last_cap 
	  (compound_ok, compound_flags)
	  (analyze)
      in
      let _, prefix = 
	get_prefix_analyses 
	  prefnode suffnode
	  prefhidden suffhidden
	  string cap
          lpos rpos
          stem_lpos stem_rpos
          lemma_present
          last_clip
          thisanalysis suffix
          last_flag  
          last_cap
          (compound_ok, compound_flags)
          (analyze)
      in
      let guess = 
	if guess then 
	  match guess_trie with 
	  | Some guess_trie -> 
              assert ( Utils.lazy_carp 3 
			 (lazy (Printf.eprintf " guess branch\n" )); true );
	     
	      let _, guess = get_suffix_analyses
		  prefnode guess_trie
		  prefhidden suffhidden
		  string cap
		  lpos rpos
		  stem_lpos stem_rpos
		  lemma_present
		  last_clip
		  thisanalysis prefix
		  last_flag  
		  last_cap
		  (compound_ok, compound_flags)
		  (analyze)
	      in
	      guess
	  | None -> prefix
	else prefix
      in
      guess
    in
    analyze
  in
  
  let newchars = Array.make 256 0 in
  let newchar_count = ref 0 in
  (* define an in_channel -> analysis function *)

  let analyze = 
    let analyze, fallback = 
      match want_guess with 
      | Global -> make true, None
      | Fallback -> make false, Some(make true)
      | NoGuess -> make false, None
    in 
    match fallback with 
    | None -> 
	let analyze string cap len = 
	  analyze
	    preftrie sufftrie
	    None None
	    string 
	    cap
	    0 (len - 1)
	    0 (len - 1)
	    true
	    Tag.empty_clip
	    Tag.empty
	    (0, [])
	    pseudoroot_flag
	    Parser_common.NoCap
	    (false, [])
	in
	analyze
    | Some fallback -> 
	let analyze string cap len = 
	  let n, res = 
	    analyze
	      preftrie sufftrie
	      None None
	      string 
	      cap
	      0 (len - 1)
	      0 (len - 1)
	      true
	      Tag.empty_clip
	      Tag.empty
	      (0, [])
	      pseudoroot_flag
	      Parser_common.NoCap
	      (false, [])
	  in
	  if n = 0 
	  then 
	    fallback 
	      preftrie sufftrie
	      None None
	      string 
	      cap
	      0 (len - 1)
	      0 (len - 1)
	      true
	      Tag.empty_clip
	      Tag.empty
	      (0, [])
	      pseudoroot_flag
	      Parser_common.NoCap
	      (false, [])
	  else 
	    n, res
	in
	analyze
  in
  let analyze string = 
    let len = String.length string in
    let cap, _ = !Parser_common.normalize string true newchars newchar_count in
    analyze string cap len
  in
  analyze 
    
let make_functions preftrie sufftrie pseudoroot_flag lemma_present_flag chars compound_flag_tbl guess_flag guess_char = 
  let make stop_at_first blocking want_to_analyze_compounds want_guess =
    if stop_at_first 
    then 
      let analyze = ( make_function true blocking preftrie sufftrie pseudoroot_flag lemma_present_flag chars compound_flag_tbl want_to_analyze_compounds want_guess guess_flag guess_char )
      in
      let analyze string = 
	try 
	  let _ = analyze string in 
	  None
	with Found analysis -> Some(analysis)
      in
      Stemmer (analyze)
    else
      let analyze = ( make_function false blocking preftrie sufftrie pseudoroot_flag lemma_present_flag chars compound_flag_tbl want_to_analyze_compounds want_guess guess_flag guess_char )
      in
      Analyzer (analyze)
  in
  make

let make_marshal bin_file no_caps = 

  Parser_common.normalize := if no_caps 
    then 
      Parser_common.normalize_no_caps 
    else Parser_common.normalize_caps;

  let marshal = open_in_bin bin_file in
  let (preftrie, sufftrie, pseudoroot_flag, lemma_present_flag, guess_flag, guess_char, chars, compound_flag_tbl) =
    Marshal.from_channel marshal
  in
  close_in marshal;
  make_functions preftrie sufftrie pseudoroot_flag lemma_present_flag chars compound_flag_tbl guess_flag guess_char
    
let make aff_file dic_file bin_file minimize no_caps = 

  Parser_common.normalize := if no_caps 
    then 
      Parser_common.normalize_no_caps 
    else Parser_common.normalize_caps;
  
  
  (* constructing the tries *)
  let preftrie = ref StringTrie.S.empty in
  let sufftrie = ref StringTrie.S.empty in
  let char_count = ref 0 in
  let chars = Array.make 256 0 in

  let compound_flag_tbl, lemma_present_flag, guess_flag, guess_char = 
    (* constructing buffer from the affix file *)
    let aff_channel, aff_buffer = 
      try 
	let aff_channel = open_in aff_file in
	aff_channel, Lexing.from_channel aff_channel
      with error ->
	Utils.carp 0 "Error opening affix file '%s' for reading\n"
	  aff_file;
	raise error
    in
    let entry_cnt, dic_channel, dic_buffer = 
      try 
	let ch = open_in dic_file in
	let entry_cnt = int_of_string (input_line ch) in (* consume dic-entry count *)
	entry_cnt, ch, Lexing.from_channel ch
      with error ->
	Utils.carp 0 "Error opening dictionary file '%s' for reading\n"
	  dic_file;
	raise error
    in
    
    (* parse the aff file *)
    let dummy = ref 0 in
    let lemma_present_flag = ref 0 in
    let guess_flag = ref 0 in
    (* guess_flag is the flag for guessable pseudoroots *)
    Utils.carp 1 "reading affix file '%s'..." aff_file ;
    let control_flags = [
      ("PSEUDOROOT", Parser_common.pseudoroot_flag);
      ("SET", dummy ); (* should set String module to character encoding *)
      ("LEMMA_PRESENT", lemma_present_flag);
      ("GUESS_FLAG", guess_flag );  
      ("CIRCUMFIX", dummy );
      ("COMPOUNDBEGIN", dummy );
      ("COMPOUNDMIDDLE", dummy );
      ("COMPOUNDEND", dummy );
      ("ONLYINCOMPOUND", dummy );
      ("FORBIDDENWORD", dummy );
      
    ] in

    (* flags are stored in a hash, since their shape can vary (1 or 2 chars) *)
    (* with maxflags, we make hash perfect *)
    let flaghash = Hashtbl.create 5000 in
    let flag_tbl = Hashtbl.create 5000 in



    (* the flagcounter index starts from 0 (to be preincremented, 0 is the root-only flag) *)
    let maxflags = ref 0 in
    let flagcount = ref 0 in
    let compound_flag_tbl = ref [] in
    let guess_char = ref '*' in (* !guess char is asterisk by default *)
    let flag_length = ref 1 in
    let _ = try 
      Aff_parser.main (Aff_lexer.token 
			 preftrie sufftrie
			 chars char_count
			 flaghash 
			 flag_tbl
			 flag_length
			 flagcount
			 maxflags
			 control_flags
			 compound_flag_tbl
			 guess_char
		      )
	aff_buffer;
      close_in aff_channel;

    with Parsing.Parse_error ->
      raise Parsing.Parse_error
    in
    Utils.carp 1 "ok\n";
    

    (* parse the dic file *)
    Utils.carp 1 "reading dic file '%s'..." dic_file ;
    let _ = try 
      let tag_hash = Hashtbl.create 10000 in
      Dic_parser.main (Dic_lexer.token 
			 preftrie sufftrie
			 chars char_count
			 tag_hash
			 flaghash
			 flag_tbl
			 !maxflags
			 !flag_length)
	dic_buffer;
      close_in dic_channel;

      Utils.carp 1 "%d tags\n" (Hashtbl.length tag_hash);
    with Parsing.Parse_error ->
      raise Parsing.Parse_error
    in
    Utils.carp 1 "%d paradigms\n" (Hashtbl.length flag_tbl);
    Utils.carp 1 "%d flags\n" (Hashtbl.length flaghash) ;
    !compound_flag_tbl, !lemma_present_flag, !guess_flag, !guess_char
  in

  Utils.carp 1 "minimizing prefix trie\n";
  flush stderr;
  let preftrie = if minimize then Minimize.minimize preftrie else preftrie in
  Utils.carp 1 "compacting prefix trie\n";
  flush stderr;
  let preftrie = Minimize.compact preftrie chars !char_count in
  
  Utils.carp 1 "minimizing suffix trie\n";
  flush stderr;
  let sufftrie = if minimize then Minimize.minimize sufftrie else sufftrie in
  Utils.carp 1 "compacting suffix trie\n";
  flush stderr;
  let sufftrie = Minimize.compact sufftrie chars !char_count in

  (* control variables are in Parser_common if they require special processing 
     in dic and aff 
     e.g., pseudo_root  and 
     but are passed to the aff lexer if not (they only affect processing) 
     e.g., compound_flag_tbl guess_char flag_length
     also control flags guess_flag and lemma_present
   *)
  let _ =
    match bin_file with 
    | Some(bin_file) ->
	let marshal = open_out_bin bin_file in
	Marshal.to_channel marshal (!preftrie, !sufftrie, !Parser_common.pseudoroot_flag, lemma_present_flag, guess_flag, guess_char, chars, compound_flag_tbl) [];
	close_out marshal;
    | None -> ()
  in
  let make = 
    make_functions !preftrie !sufftrie !Parser_common.pseudoroot_flag lemma_present_flag chars compound_flag_tbl guess_flag guess_char 
  in
  make

let c_make make = 
  let make a b c d = 
    let d = match d with 
      0 -> NoGuess
    | 1 -> Fallback
    | 2 -> Global 
    | _ -> raise (Failure "invalid guess value (should be 0,1,2)")
    in
    match make a b c d with 
    | Stemmer f -> 
	let f s = 
	  match f s with 
	   Some tag -> 1, Array.make 1 (Tag.print tag)
	  | None -> 0, Array.make 0 ""
	in
	f
    | Analyzer f ->
	let f s = 
	  let n, a = try 
	    let n, a = f s in
	    n, Array.of_list (List.rev_map (Tag.print) a)
	  with x -> Printf.printf "caught exception\n"; flush stdout; raise x
	  in
	  n, a
	in
	f
  in
  make
  
let c_init_from_aff_dic aff dic bin minimize no_caps = 
  let bin = if bin = "" then None else Some(bin) in
  let make = make aff dic bin minimize no_caps in 
  c_make (make)

let c_init_from_bin bin no_caps = 
  let make = make_marshal bin no_caps in 
  c_make (make)

let _ = Callback.register "init_from_aff_dic" c_init_from_aff_dic
let _ = Callback.register "init_from_bin" c_init_from_bin
  
 
