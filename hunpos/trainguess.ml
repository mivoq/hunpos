

let chan = open_in "data/szeged.ful.newest.0.train" 
	(* test.train *) 
	(* szeged.ful.0.test *)


let morph = OcamorphWrapper.init ()

let tokens = ref 0
	
let oov    = ref 0	

let mem =  Maxent.create ()


let token (word, gold) =
	incr (tokens) ;
	let anal = OcamorphWrapper.analyze morph word in
	if OcamorphWrapper.oov anal then begin
	(* csak oov szavakkal foglalkozunk *)
		incr (oov);
		Printf.printf "%s\n" word;
		Maxent.add_event mem (OcamorphWrapper.tags anal) gold 1 ;

	end
;;

let _ = 
			
Printf.eprintf "extracting features\n";
Io.iter_tokens chan token ;

Maxent.train mem ;

Printf.eprintf "saving cache\n";
OcamorphWrapper.save_cache morph