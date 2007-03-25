

module Hmm_tagger = Hmm_tagger

let typ2string = function
	Hmm_tagger.Seen -> "S"
	| Hmm_tagger.LowerCasedSeen -> "*L"
	| Hmm_tagger.SpecialToken -> "*D"
	| Hmm_tagger.UnSeen -> "*"

	
let tag_sentence tagger  sentence =
    let obs, tags = tagger sentence in
	let print_tagged obs tag =
		print_string obs.Hmm_tagger.word; print_char '\t'; print_string tag;
		print_char '\t'; print_endline (typ2string obs.Hmm_tagger.seen)
	in
	List.iter2 print_tagged (List.rev obs) (List.rev tags);
	print_newline ()
;;

let total = ref 0




let usage () = 
	Printf.eprintf "usage : %s  modelfile tagtable  [tag-order [emission-order]] \n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in	exit 1 
else

let tagorder =
	if( Array.length Sys.argv) > 3 then (int_of_string Sys.argv.(3)) else 2
in

let emorder =
	if (Array.length Sys.argv) > 4 then (int_of_string Sys.argv.(4)) else 2
in	
let hunmorph = Morphtable.load Sys.argv.(2) in
let model = Hmm_tagger.load Sys.argv.(1) in
prerr_endline "model loadad";
let tagger = Hmm_tagger.compile_tagger  model (Morphtable.tags hunmorph) tagorder emorder in
prerr_endline "tagger compiled";

let ic =  stdin in


Io.iter_sentence ic (tag_sentence  tagger )
