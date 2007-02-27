module SNgramTree = Ngramtree.Make(Mfhash.String)
type seen_type = Seen | LowerCasedSeen | SpecialToken | UnSeen

type observation = {word : string;
 			
					mutable seen : seen_type; 
					mutable oov : bool; 
					mutable anals : string list;
					mutable guessed: (string * float ) list;
					}

let load filename   morphtable tag_order emission_order = 
	let ic = open_in filename in
	let  (ttree, otree, stree, lsuffix_trie, usuffix_trie, vocab) = Marshal.from_channel ic in
	close_in ic;

	prerr_string "tag order = "; prerr_int tag_order; prerr_newline();
		prerr_string "emission order = "; prerr_int emission_order; prerr_newline();
(*	Printf.eprintf "calculating tag transition lamdas\n";
*)	let tlamdas = SNgramTree.calculate_lambdas ttree ttree (tag_order + 1) in
(*	Array.iteri (fun i v -> Printf.eprintf "%d = %f\n" i v) tlamdas; 
*)	
(*	Printf.eprintf "calculating observation lamdas\n";
*)	let olamdas = SNgramTree.calculate_lambdas otree ttree (emission_order + 1) in
(*	Array.iteri (fun i v -> Printf.eprintf "%d = %f\n" i v) olamdas; 
*)
	let slamdas = SNgramTree.calculate_lambdas stree ttree (2) in
	Array.iteri (fun i v -> prerr_float v; prerr_newline ()) slamdas; 	
(*	Printf.eprintf "deriving observation probabilities\n";
*)	let potree = SNgramTree.probtree otree ttree olamdas in
	(*
	SNgramTree.iter  (fun l freq -> Printf.printf "%s\t%f\n" (String.concat " " l) freq ) potree;
	*)	
(*	Printf.eprintf "deriving tag transition probabilities\n";
*)	let pttree = SNgramTree.probtree ttree ttree tlamdas in
(*	SNgramTree.iter  (fun l freq -> Printf.printf "%s\t%f\n" (String.concat " " l) freq ) pttree;	
	Printf.eprintf "model is built.\n";
*)	
	let pstree = SNgramTree.probtree stree ttree slamdas in

	prerr_endline "calculating theta";
	let theta = Suffix_guesser.theta ttree in
	prerr_string "theta = "; prerr_float theta; prerr_newline ();
	
	let (ltagprob, ltagprobs) = Suffix_guesser.guesser_from_trie lsuffix_trie ttree theta 100. vocab  in
	let (utagprob, utagprobs) = Suffix_guesser.guesser_from_trie usuffix_trie ttree theta 100. vocab  in
	
let module State = struct
	type t = string Ngram.t
	let compare ng1 ng2  = Ngram.compare ng1 ng2 tag_order
	let print ngram = Ngram.print ngram tag_order (print_string)
end 
in

let module StateViterbi = Viterbi.Make(Ocamap.Make(State))
in

let module SNgramTree = Ngramtree.Make(Mfhash.String)
in

let start_state = "<s>" :: "<s>" :: [] in

let end_of_sentence =
	let transition from = ((Ngram.add "<s>" from), SNgramTree.wordprob pttree from "<s>")::[] in
	let emission state = 0.0 in
	(transition, emission)
in

let next obs =
		let w = obs.word in
		let debug =  false in
		(* this is the end of sentence token; calculating only transition probs *)
		if w = "<s>" then
			end_of_sentence
		else 
		(* has any uppercased char? *)
		let (lw, is_upper) = Io.lowercase w in
		
		(* is it known words? *)	
		let (oov, anals) = try (false, Morphtable.analyze morphtable ( w)) 
						   with Not_found -> (true, []) in
		obs.oov <- oov ; 
		obs.anals <- anals;	
		(* a következő esetek lehetnek: 
		   benne van a lexikonban, as is
		   nagybetus, es kibetusen benn van a lexikonban
		   special token
		   nincs sehol
		*)
		(* check whether we have lexikon info *)
		try
			let (obs_node, seen) = try (SNgramTree.move_to_child potree w, Seen) with Not_found -> 
								   try (SNgramTree.move_to_child potree lw, LowerCasedSeen) with Not_found ->
								   (* megprobaljuk regexp szerint *)
								   SNgramTree.move_to_child pstree (Special_tokens.to_lex w), SpecialToken
			
			in
			obs.seen <- seen;
			
			(* milyen tagjei lehetnek *)
			let tags = SNgramTree.edges obs_node in
			
			let transition from = 
				List.map (fun tag -> 
							let next_state = Ngram.add tag from in
							let tp = SNgramTree.wordprob pttree from tag in
									if debug then begin
									print_string w; print_char ' '; print_string tag; print_newline();
									let _ = State.print from ; print_string "->"; State.print next_state in
									print_float tp ; print_newline ()
									end;
							(next_state, (tp )) 
				) tags
			in
			let emission state =
				SNgramTree.seq_prob obs_node state 
			in
			(transition, emission)		
					
		with Not_found -> (* not seen word *)
			obs.seen <- UnSeen;
			
			let anals2transtion_fun anals =
				let transition from = List.map (fun tag  ->
						let next_state = Ngram.add tag from in
						let tp = SNgramTree.wordprob pttree from tag in
							
						(next_state, tp )) anals
				in
				transition
			in
			if (List.length obs.anals) = 1 then
				let transition = (anals2transtion_fun obs.anals)
				in 
				let emission state = 0.0 in
				(transition, emission)
			else 
				let (tagprobs, tagprob) = if is_upper then  (utagprobs, utagprob) 
										  else (ltagprobs, ltagprob) 
				in
				
				if not obs.oov then
					let transition = (anals2transtion_fun obs.anals)
					in 
					let emission state = let tag = List.hd state in
					tagprob w tag in
					(transition, emission)
				else
				let tags_probs = tagprobs  (lw) in
				obs.guessed <- tags_probs;
				if debug then
					List.iter (fun (tag, prob) -> Printf.printf "%s %f\n" tag prob;) tags_probs;
		
				let transition 	from = List.map (fun (tag,logprob) -> 
					let next_state = Ngram.add tag from in
					let tp = SNgramTree.wordprob pttree from tag in
							if debug then begin
							print_string w; print_char ' '; print_string tag; print_newline();
							let _ = State.print from ; print_string "->"; State.print next_state in
							print_float tp ; print_newline ()
							end;
					(next_state, (tp )) 
				) tags_probs			
				in
				let emission state =
					let tag = List.hd state in
						List.assoc tag tags_probs 
				in
				(transition, emission)		
in		
		

let tag_sentence words  =


	let word2observation w = { word = w; seen = Seen; oov = false; anals = []; guessed = []} in

	let observations = 	List.map (word2observation) (List.rev ( "<s>"::words))  in
	let state_seq = StateViterbi.decode  next start_state observations in
	( List.tl (List.rev  observations), List.tl (List.rev (List.map  (Ngram.newest) (state_seq))))
		
 in tag_sentence
