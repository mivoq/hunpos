module SNgramTree = Ngramtree.Make(Mfhash.String)

let vocab = Vocab.create () 
	
let build_modell chan = 
	let tt = SNgramTree.empty () in
	let ot = SNgramTree.empty () in
	let add_sentence (words, tags) =
	
		let rec aux words tags =		
			match words, tags with
			 |  (word::[], first_tag::start_tags) -> SNgramTree.add ot ((word)::tags) 2;
													SNgramTree.add tt tags 2;
													SNgramTree.add_bos tt start_tags 1;
			
			 |	(word::word_tails),
			 	(tag::tag_tails)   -> SNgramTree.add tt tags 3;
									  SNgramTree.add ot ((word)::tags) 3;
						  			  aux word_tails tag_tails
			 | (_,_) -> ()
		in
		SNgramTree.add tt ("<s>" :: tags) 3;
		let tags = tags @ ("<s>" :: "<s>" :: []) in
	    aux words tags;
	in
	Io.iter_sentence chan add_sentence;
	(tt, ot)
	
	

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
let (ttree, otree) = build_modell chan in
(* Ngramtree.print otree; *)
(*Ngramtree.print ttree; 
*)
(*
let n = ref 0 in
let suffixtrie = Suffix_guesser.empty () in
let suffix gram freq =
	if freq >= 15 then () else
	let (word::tag::t) = gram in
	incr n ;
	Suffix_guesser.add_word suffixtrie word tag freq
in
prerr_endline "building suffix trie";
SNgramTree.iter_level 2 suffix otree;

print_int !n; print_endline " words added";
*)
let n = ref 0 in
let suffixtrie = ref Suffix_guesser.empty  in
let suffix gram freq =
	if freq >= 15 then () else
	let (word::tag::t) = gram in
	incr n ;
	let ix = Vocab.toindex vocab tag in
	suffixtrie := Suffix_guesser.add_word !suffixtrie word ix freq;
(*	Printf.printf "after %s %s\n" word tag ;
	Suffix_guesser.print !suffixtrie;
*)in
prerr_endline "building suffix trie";
SNgramTree.iter_level 2 suffix otree;
(*Suffix_guesser.print_stat !suffixtrie;
*)

let theta = 0.0001 in
let (tagprob, tagprobs) = Suffix_guesser.guesser_from_trie !suffixtrie theta 1000. vocab  in
let a = tagprobs "Zamárdiba"  in
List.iter (fun (t,p) -> Printf.printf "%s %f\n" t p) a;

let p = tagprob "Zamárdiba" "NOUN<CAS<ILL>>" in
Printf.printf "NOUN<CAS<ILL>> %f\n" p;

let oc = open_out Sys.argv.(2) in
Marshal.to_channel oc (ttree, otree,!suffixtrie, vocab) [];
close_out oc;
