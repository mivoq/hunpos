
let usage () = 
	Printf.eprintf "usage : %s configfile \n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 2 then 
	let _ = usage () in	exit 1 
else

let config = Config.parse_file Sys.argv.(1) in


let chan =  stdin in
	(* test.train *) 
	(* szeged.ful.0.test *)
let emission_order = (Config.get_emission_order config)  in
let tag_order = (Config.get_tag_order config) in
let max_freq_for_guess = Config.get_max_freq_for_guess config in
let max_suffix_length = Config.get_max_suffix_length config in
let model_file = Config.get_model_file config in
	
	
let model = Hmm_tagger.start_train tag_order emission_order in
prerr_endline "reading training corpus";
Io.iter_tagged_sentence chan (Hmm_tagger.add_sentence model);
(*Hmm_tagger.save model Sys.argv.(2);

let model = Hmm_tagger.load Sys.argv.(2) in
*)
prerr_endline "compiling probabilities";
Hmm_tagger.calculate_probs model;


prerr_endline "constructing suffix guesser";

Hmm_tagger.build_suffixtries model max_freq_for_guess max_suffix_length;

Hmm_tagger.save model model_file;

