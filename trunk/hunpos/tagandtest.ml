module Hmm_tagger = Hmm_tagger

let usage () = 
	Printf.eprintf "usage : %S config_file \n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 2 then 
	let _ = usage () in	() (*exit 1*) 
else
let config = Config.parse_file Sys.argv.(1) in
let tagorder = (Config.get_tag_order config) in
let emorder = (Config.get_emission_order config)in
let model_file = (Config.get_model_file config)in
let hunmorph_file = (Config.get_morphtable_file config) in
let max_guessed_tags = (Config.get_max_guessed_tags config) in	

let total_matrix = Array.make_matrix 2 2 0 in
let false_matrix = Array.make_matrix 2 2 0 in
let unseen_disambig = ref  0 in
	
let rec iter3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; iter3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "List.iter3"
		
in

let eval obs gtags tags = 
	let eval_token obs gold tag = 
			(* ha benne van a modell lexikonjában, akkor látott szó *)
		let seen = match  obs.Hmm_tagger.seen with Hmm_tagger.Seen -> 0 | _-> 1 in
		let oov = if obs.Hmm_tagger.oov then 1 else 0 in
		total_matrix.(seen).(oov) <- total_matrix.(seen).(oov) +1 ; 
		
		if (compare gold tag) != 0 then begin
			false_matrix.(seen).(oov) <- false_matrix.(seen).(oov) + 1;
		(*	if oov = 1 && seen == 1 then
				Printf.printf "%s %s\n" obs.Hmm_tagger.word tag
		*) end
		in
	iter3 eval_token obs gtags tags
in
		
let tag_sentence tagger sentence =

	let words, gtags = sentence in
    let obs, tags = tagger words in
	eval obs gtags tags
in
	
let total = ref 0 in
let falses = ref 0 in
	

let hunmorph = Morphtable.load hunmorph_file in
prerr_endline "morphtable loaded";
let model = Hmm_tagger.load model_file in
prerr_endline "model loadad";
let tagger = Hmm_tagger.compile_tagger  model (Morphtable.tags hunmorph) tagorder emorder max_guessed_tags in
prerr_endline "tagger compiled";
let ic =  stdin in
	
prerr_endline "tagging";
Io.iter_tagged_sentence ic (tag_sentence tagger);
prerr_endline "tagged";
close_in ic;

for i = 0 to 1 do
	for j = 0 to 1 do
		total := !total + total_matrix.(i).(j);
		falses := !falses + false_matrix.(i).(j);
	done;
done ;
Printf.printf "tokens count: %d\n" !total;
Printf.printf "tokens:\n";
Printf.printf "        %8s %8s\n"  "known" "unknown";
Printf.printf "seen    %8d %8d\n" total_matrix.(0).(0) total_matrix.(0).(1);	
Printf.printf "unseen  %8d %8d\n" total_matrix.(1).(0) total_matrix.(1).(1);	



let p n = float n *. 100.0 /. float !total in

Printf.printf "\ntokens percent:\n" ;
Printf.printf "        %8s %8s\n"  "known" "unknown";
Printf.printf "seen    %8.2f %8.2f\n" (p total_matrix.(0).(0)) (p total_matrix.(0).(1));	
Printf.printf "unseen  %8.2f %8.2f\n" (p total_matrix.(1).(0)) (p total_matrix.(1).(1));	

let prec = Array.create_matrix 2 2 0.0 in
	
for i = 0 to 1 do
	for j = 0 to 1 do
		prec.(i).(j) <- float (total_matrix.(i).(j) - false_matrix.(i).(j)) *. 100.0 /. float total_matrix.(i).(j) 
	done;
done ;
	
	
Printf.printf "\nprecision:\n" ;
Printf.printf "        %13s %13s\n"  "known" "unknown";
Printf.printf "seen    (%4d/%5d) %8.2f (%4d/%5d) %8.2f\n" false_matrix.(0).(0) total_matrix.(0).(0) prec.(0).(0) false_matrix.(0).(1) total_matrix.(0).(1) prec.(0).(1);	
Printf.printf "unseen  (%4d/%5d) %8.2f (%4d/%5d) %8.2f\n" false_matrix.(1).(0) total_matrix.(1).(0) prec.(1).(0) false_matrix.(1).(1) total_matrix.(1).(1) prec.(1).(1);	
Printf.printf "unseen and disambig %d\n" !unseen_disambig;
Printf.printf "\noverall precision: %8.2f\n" (float (!total - !falses) /. float !total *. 100.) ;


Printf.printf "false seen tokens\t%d\n" (false_matrix.(0).(0) + false_matrix.(0).(1));
Printf.printf "false unseen tokens\t%d\n" (false_matrix.(1).(0) + false_matrix.(1).(1));
Printf.printf "false inlex tokens\t%d\n" (false_matrix.(0).(0) + false_matrix.(1).(0));
Printf.printf "false oov tokens\t%d\n" (false_matrix.(0).(1) + false_matrix.(1).(1));
Printf.printf "oovtotal\t%d\n" (total_matrix.(0).(1) + total_matrix.(1).(1));
Printf.printf "falsetotal\t%d\n" !falses;
Printf.printf "total\t%d\n" !total;

