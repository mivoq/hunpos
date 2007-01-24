


let total_matrix = Array.make_matrix 2 2 0 
let false_matrix = Array.make_matrix 2 2 0 
		
let eval tagged_sentence = 
	let eval_token (obs, gold, tag) = 
 		(* ha benne van a modell lexikonjában, akkor látott szó *)
		
		let seen = if  obs.Tagger_hmm.seen then 0 else 1 in
		let oov = if obs.Tagger_hmm.oov then 1 else 0 in
		total_matrix.(seen).(oov) <- total_matrix.(seen).(oov) +1 ;  
		if (compare gold tag) != 0 then
		begin
			false_matrix.(seen).(oov) <- false_matrix.(seen).(oov) + 1;
			if  seen = 1 then Printf.printf "%s\t%s\t%s\t\tmorph: %s\n" obs.Tagger_hmm.word gold tag (String.concat "@"  obs.Tagger_hmm.anals);
	
		end
	in
	List.iter eval_token tagged_sentence 

		
let tag_sentence model morphtable wma sentence =
    let observations_and_tags = Tagger_hmm.tag_sentence model morphtable wma sentence in
	List.map2 (fun (obs, tag) (word, gold) -> (obs, gold, tag)) observations_and_tags sentence

	
let total = ref 0

let falses = ref 0
	

let usage () = 
	Printf.eprintf "usage : %s  modelfile morphtable \n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in	exit 1 
else
	
let model = Tagger_hmm.load_model Sys.argv.(1) in
let hunmorph = Morphtable.load Sys.argv.(2) in
	
	
let base = "/Users/hp/work/lm/data/lm/" in

let classes =
		"0-0"::
		"ABBR"::
		"ADJ"::
		"ADV"::
		"ART"::
		"CONJ"::
		"DET"::
		"E"::
		"NOUN"::
		"NUM"::
		"ONO"::
		"POSTP"::
		"PREV"::
		"PUNCT"::
		"UTT-INT"::
		"VERB"::
		[]
in

let files = List.map (fun s -> base ^ s) classes in
let files = List.combine classes files in
let lms = Lmclassifier.load_lms files in
let wma = (fun w -> 
			let seq = Lmclassifier.string2charlist w in
			Lmclassifier.weight_seq lms 4 seq)	
in


let ic =  stdin in
	
Io.iter_sentence ic (fun sentence -> eval (tag_sentence model hunmorph wma sentence ));
	
for i = 0 to 1 do
	for j = 0 to 1 do
		total := !total + total_matrix.(i).(j);
		falses := !falses + false_matrix.(i).(j);
	done;
done ;
 
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
Printf.printf "        %8s %8s\n"  "known" "unknown";
Printf.printf "seen    %8.2f %8.2f\n" prec.(0).(0) prec.(0).(1);	
Printf.printf "unseen  %8.2f %8.2f\n" prec.(1).(0) prec.(1).(1);	

Printf.printf "\noverall precision: %8.2f\n" (float (!total - !falses) /. float !total *. 100.) ;
