module State = struct
	type t = Ngram.t
	let compare = Ngram.compare
end ;;

module StateViterbi = Viterbi.Make(State)


type observation = {word : string; mutable seen : bool; mutable oov : bool; mutable anals : string list}
 
let word2observation w = { word = w; seen = true; oov = false; anals = []}
	
(* ez adja meg, hogyan lepkedunk *)	
let next_state state tag =
		Ngram.shift_right state tag
	


(* ez szamolja a P(tag | state_from) + P(o | state_from) *)
let prob potree pttree = 
	let f from_state tag obs =
		let w = obs.word in
		let op = (Deleted_interpolation.lprob_lf potree (w::Ngram.shift_right from_state tag)) in
    	let tp = (Deleted_interpolation.lprob_lf pttree (tag :: from_state )) in
		let tp = 	if tp == neg_infinity then log 0.00001 else tp in
		tp +. op
	in
	f
	
let unseenprob potree pttree = 
	let f wtags from_state tag o = 
	 let tp = (Deleted_interpolation.lprob_lf pttree (tag :: from_state )) in
	 let tp = (if (classify_float tp) == FP_infinite then log 0.0001 else tp) in
	 let op = try List.assoc tag wtags with Not_found -> log 0.0000000001 in

	tp +. op
	in
	f
	
let trans (potree, pttree) morphtable wma =
	(* milyen cimkei lehetnek a szonak. Ha AB allapotban voltunk es
	   C tagje lehet, akkor BC egy koveto allapot 
	*)
	let prob = prob potree pttree in
	let unseenprob = unseenprob potree pttree in
	let f obs = 
		let w = obs.word in
		let (oov, anals) = try (false, Morphtable.analyze morphtable w) with Not_found -> (true, []) in
		obs.oov <- oov ; 
		obs.anals <- anals;
		try 
			let onode = Deleted_interpolation.child_node potree w in
			let tags = Deleted_interpolation.edges onode in
				obs.seen <- true;
			(fun state_from -> List.map (fun tag -> let next_state = Ngram.shift_right state_from tag in 
				(next_state, prob state_from tag obs) )   tags )
	
		with Not_found ->
			let wtags = wma w in
			let tags =  
				if oov then
				 	let (labels ,_) = List.split wtags in labels
				else 
					anals 
			in

		
		(*	Printf.printf "check=%s=%s=\n" w (String.concat "@" tags); *)
			obs.seen <- false;
			(fun state_from -> List.map (fun (tag) -> let next_state = Ngram.shift_right state_from tag in 
				(next_state, unseenprob wtags state_from (tag) obs) )   tags)	
		in f
		
let tag model morphtable wma observations =
		let state_seq = StateViterbi.decode (Ngram.empty 2)  observations  (trans model morphtable wma) in
		List.map (fun state -> List.hd (state)) state_seq
		
let load_model filename = 
	let ic = open_in filename in
	let  (potree, pttree) = Marshal.from_channel ic in
	close_in ic;
	(potree, pttree)
	(* 
	let hunmorph = OcamorphWrapper.init () in
	(potree, pttree, hunmorph)
*)
let tag_sentence 	((potree, pttree) as model) morphtable wma  sentence  =
	
	let sentence2observations sentence =
		List.map (fun (word, gold) -> word2observation word) sentence
	in
	let observations = sentence2observations sentence in
	let tags = tag model morphtable wma observations in
	List.combine observations tags
	