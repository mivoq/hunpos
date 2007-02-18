module SHash = Mfhash.String

let tokens = ref 0 
let sentences = ref 0 
let lex = SHash.empty() 
	
let calc_sentence sent =
	tokens := !tokens + (List.length sent);
	let update_word w = 
		let _ = SHash.update lex w (fun () -> 1) (succ) in ()
	in
	List.iter update_word sent;
	incr sentences;
;;


Io.iter_sentence stdin (calc_sentence) ;
let types = SHash.size lex in
Printf.printf "tokens: %d\n" !tokens;
Printf.printf "types: %d\n" types;
Printf.printf "sentences: %d\n" !sentences;
Printf.printf "avg sentence length: %8.2f\n" ((float !tokens) /. (float !sentences));
Printf.printf "types/tokens: %f\n" ((float types) /. (float !tokens));