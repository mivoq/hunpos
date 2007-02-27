module SNgramTree = Ngramtree.Make(Mfhash.String)
let emission_order = 3 
let tag_order = 3
	
let vocab = Vocab.create () 
	
let build_modell chan = 
	let tt = SNgramTree.empty () in
	let ot = SNgramTree.empty () in
	let st = SNgramTree.empty () in
	let add_word word tags order = 
		SNgramTree.add ot ((word)::tags) order;
		let spec = Special_tokens.to_lex word in
		if spec != word then
			 SNgramTree.add st ((spec)::tags) order;
	
	in
	let add_sentence (words, tags) =
	
		let rec aux words tags =		
			match words, tags with
			 |  (word::[], first_tag::start_tags) -> add_word word tags emission_order;
													SNgramTree.add tt tags tag_order;
													SNgramTree.add_bos tt start_tags 3;
			
			 |	(word::word_tails),
			 	(tag::tag_tails)   -> SNgramTree.add tt tags (tag_order + 1);
									  add_word word tags (emission_order + 1);
						  			  aux word_tails tag_tails
			 | (_,_) -> ()
		in
		SNgramTree.add tt ("<s>" :: tags) (tag_order + 1);
		let tags = tags @ ("<s>":: "<s>" :: "<s>" :: []) in
	
	    aux words tags;
	in
	Io.iter_tagged_sentence chan add_sentence;
	(tt, ot, st)
	
	

let usage () = 
	Printf.eprintf "usage : %s corpusfile modelfile \n" Sys.argv.(0)
;;

let _ =	
if (Array.length Sys.argv) < 3 then 
	let _ = usage () in	exit 1 
else

let chan = open_in Sys.argv.(1) in
	(* test.train *) 
	(* szeged.ful.0.test *)



prerr_endline "building frequency trees";
let (ttree, otree, stree) = build_modell chan in
(*let node = SNgramTree.move_to_child otree "@CARDSEPS" in
Printf.printf "%d " (SNgramTree.freq node);
*)

let lsuffixtrie = ref Suffix_guesser.empty  in
let usuffixtrie = ref Suffix_guesser.empty  in
let lrarewords = ref 0 in
let urarewords = ref 0 in
let maxlength = 10 in
let do_word word node =

	if (SNgramTree.freq node) <= maxlength then
		let (lword, is_upper) = Io.lowercase word in
		let used_suftrie, counter = 
		if not is_upper then
			lsuffixtrie, lrarewords
		else
			usuffixtrie, urarewords
		in
			SNgramTree.iter_childs (fun tag node ->
				let ix = Vocab.toindex vocab tag in
				used_suftrie := Suffix_guesser.add_word !used_suftrie maxlength lword ix (SNgramTree.freq node);
				 counter:= !counter + (SNgramTree.freq node);
				) node
in


prerr_string "building suffix trie ";
SNgramTree.iter_childs  do_word otree;
(*Suffix_guesser.print_stat !suffixtrie;
*)

prerr_int !lrarewords; prerr_string " lowercase, ";
prerr_int !urarewords; prerr_endline " uppercase ";



let oc = open_out Sys.argv.(2) in
Marshal.to_channel oc (ttree, otree, stree, !lsuffixtrie, !usuffixtrie, vocab) [];
close_out oc;
