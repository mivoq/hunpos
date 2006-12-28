let usage () = 
	Printf.eprintf "usage : %s  modelfile morphtable \n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in	exit 1 
else
	
let model = Tagger_hmm.load_model Sys.argv.(1) in
let hunmorph = Morphtable.load Sys.argv.(2) in
	
let ic =  stdin in
	
let print_tagged tagged_sentence = 
	let print_line (obs, tag) = 
		Printf.printf "%s\t%s\n" obs.Tagger_hmm.word   tag ;
	in
	List.iter print_line tagged_sentence ;
	print_newline()
in
	
let tag_sentence sentence = 
	(* egyelore a tagger gold taget is var *)
	let golded = List.map (fun s -> (s, "GOLD")) sentence in
	let tagged = Tagger_hmm.tag_sentence model hunmorph golded 
	in print_tagged tagged

in Io.iter_sentence_no_split ic (tag_sentence) 


