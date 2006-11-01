

let chan = open_in "data/szeged.ful.newest.0.test" 
	(* test.train *) 
	(* szeged.ful.0.test *)


let morph = OcamorphWrapper.init ()

let tokens = ref 0
	
let oov    = ref 0	

let guesstree = ref Ngramtree.empty


let token (word, gold) =
	incr (tokens) ;
	let anal = OcamorphWrapper.analyze morph word in
	if OcamorphWrapper.oov anal then begin
	(* csak oov szavakkal foglalkozunk *)
		incr (oov);
		Printf.printf "%s " word;
		let tags =  String.concat "/" (List.sort compare (OcamorphWrapper.tags anal)) in
		guesstree := Ngramtree.add !guesstree (gold::tags::[])

	end
;;

let _ = 
			
Printf.eprintf "building guess tree\n";
Io.iter_tokens chan token ;
Printf.eprintf "saving guess tree\n";

let oc = open_out "data/guessed_tree" in
Marshal.to_channel oc (!guesstree) [];
close_out oc;

Printf.eprintf "saving cache\n";
OcamorphWrapper.save_cache morph