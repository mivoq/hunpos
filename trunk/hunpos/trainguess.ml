

let chan = open_in "/Users/hp/work/oca/data/szeged.ful.newest.0.train"
		(* test.train *) 
	(* szeged.ful.0.test *)
 (* 	data/szeged.ful.newest.0.train *)

let morph = OcamorphWrapper.init ()

let tokens = ref 0
	
let oov    = ref 0	

let mem =  Maxent.create ()

(*
let surface_predicates word = 
	let preds = ![] in
	if Str.regexp 
*)

let unique l =
	let rec aux last output input  = match input with
		[] -> output
	  | head :: tail -> if last = head then (aux last output tail ) else (aux head (head::output) tail)
	in
	match (List.sort compare l) with 
		[] -> l
	|	head::tail -> aux head (head::[]) tail
		
		
let token (word, gold) =
	incr (tokens) ;

	let anal = OcamorphWrapper.analyze morph word in
	if OcamorphWrapper.oov anal then begin 
	(* csak oov szavakkal foglalkozunk *)
		incr (oov);

		
		let lemmas =    OcamorphWrapper.lemmas_with_suffixes anal in
		let preds = List.map (fun (lemma, suffixes) -> String.concat "$" (List.sort compare suffixes)) lemmas in
		let preds = unique preds in
		let preds = OcamorphWrapper.tags anal in
		if List.length preds > 1 then 
		Maxent.add_event mem (preds) gold 1 ;
		
		
		Printf.printf "%s %s\n" gold (String.concat " " preds);
		flush_all ();		
	end

;;

let _ = 
			
Printf.eprintf "extracting features\n";
Io.iter_tokens chan token ;

Maxent.train mem ;

Maxent.save mem "maxent.model";

Printf.eprintf "saving cache\n";
OcamorphWrapper.save_cache morph


