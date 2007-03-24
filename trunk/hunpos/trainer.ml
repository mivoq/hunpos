


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

let model = Hmm_tagger.start_train 2 2 in
prerr_endline "reading training corpus";
Io.iter_tagged_sentence chan (Hmm_tagger.add_sentence model);
(*Hmm_tagger.save model Sys.argv.(2);

let model = Hmm_tagger.load Sys.argv.(2) in
*)
prerr_endline "compiling probabilities";
Hmm_tagger.calculate_probs model;


prerr_endline "constructing suffix guesser";
Hmm_tagger.build_suffixtries model 10 10;

Hmm_tagger.save model Sys.argv.(2);

