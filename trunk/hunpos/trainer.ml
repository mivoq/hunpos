module SNgramTree = Ngramtree.Make(Mfhash.String)

let build_modell chan = 
	let tt = SNgramTree.empty () in
	let ot = SNgramTree.empty () in
	let add_sentence (words, tags) =
	
		let rec aux words tags =		
			match words, tags with
			 |  (word::[], tag::[]) -> SNgramTree.add tt tags 2;
			 |	(word::word_tails),
			 	(tag::tag_tails)   -> SNgramTree.add tt tags 3;
									  SNgramTree.add ot (word::tags) 3;
						  			  aux word_tails tag_tails
			 | (_,_) -> ()
		in
		let tags = ("</s>"::tags) @ ("<s>"::[]) in
		let words  = ("</s>"::words) @ ("<S>"::[]) in
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

let suffixtrie = Suffix_guesser.empty () in
let suffix gram freq =
	if freq > 10 then () else
	let (word::tag::t) = gram in
	Suffix_guesser.add_word suffixtrie word tag freq
in
prerr_endline "building suffix trie";
SNgramTree.iter_level 2 suffix otree;
prerr_endline "estimating theta";
let t = Suffix_guesser.theta suffixtrie in
Suffix_guesser.toprob suffixtrie t;
prerr_endline "calculating probs";



let oc = open_out Sys.argv.(2) in
Marshal.to_channel oc (ttree, otree,suffixtrie) [];
close_out oc;
