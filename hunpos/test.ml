

let chan = open_in "/Users/hp/work/oca/data/szeged.ful.newest.0.test" 
	(* test.train *) 
	(* szeged.ful.0.test *)


let morph = OcamorphWrapper.init ()

let tokens = ref 0
	
let oov    = ref 0	

let mem =  Maxent.load "/Users/hp/work/oca/maxent.model" ;;
	
print_endline "loaded" ;;

let token (word, gold) =
	incr (tokens) ;

	let anal = OcamorphWrapper.analyze morph word in
	if OcamorphWrapper.oov anal then begin 
	(* csak oov szavakkal foglalkozunk *)
		incr (oov);
		Printf.printf "%s %s\n" word gold;
		flush_all ();
		let c = (OcamorphWrapper.tags anal) in
		Printf.printf "anals: %s\n"	(String.concat "\t" c);
		let fl = Maxent.sort_outcomes (Maxent.eval_all mem c) in
		List.iter (fun (s,p) -> Printf.printf "%s %f\n" s p ) fl;

	end

;;

let _ = 
			
Io.iter_tokens chan token ;


Printf.eprintf "saving cache\n";
