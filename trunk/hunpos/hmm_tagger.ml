(* ez szamolja a tag atmeneteket. A tagek helyet int-et
	hasznalunk
*)
module TagProbLM = Linear_interpolated_lm.Make( struct
	module CMap = Mfhash.Int 
	module WMap = Mfhash.Int
	let bos = -1
end )

(* a kimeneti valoszinusegek
*)
module ObsProbLM = Linear_interpolated_lm.Make ( struct
	module CMap = Mfhash.Int 
	module WMap = Mfhash.String
	let bos = -1
end )

(* egy szonak milyen tagjei lehetnek *)
module ObsLexicon = Lexicon.Make(Mfhash.String)

let eos_tag = "</s>"
let bos_tag = "<s>"
	
type model = { 
  			  tag_order : int ;
			  emission_order : int;
			  mutable tag_lm : TagProbLM.t;
              mutable obs_lm : ObsProbLM.t;
			  mutable spec_lm : ObsProbLM.t;
			  mutable obs_lex : int ObsLexicon.t;
			  mutable spec_lex : int ObsLexicon.t;
			 
			  tag_vocab : Vocab.t;
			  mutable low_suffixes: Suffix_guesser.t;
			  mutable upp_suffixes: Suffix_guesser.t;
			  eos : int;
			  bos : int;
			  mutable apriori_tag_probs : float array;
			  mutable theta : float
}

type train_stat = {
			mutable tokens : int;
			mutable sentences : int;
			mutable types : int;
			mutable rare_low : int;
			mutable rare_upp : int;
}
let start_train tag_order emission_order =
	let tag_vocab = Vocab.create () in
	let eos_ix = Vocab.toindex tag_vocab eos_tag in
	let bos_ix = Vocab.toindex tag_vocab bos_tag in
	let m = {
		tag_order = tag_order;
		emission_order = emission_order;
		tag_lm = TagProbLM.empty_freq_counter () ;
		obs_lm = ObsProbLM.empty_freq_counter () ;
		spec_lm = ObsProbLM.empty_freq_counter () ;
		obs_lex = ObsLexicon.empty ();
	    spec_lex = ObsLexicon.empty ();
		tag_vocab = tag_vocab ;
		low_suffixes = Suffix_guesser.empty ;
		upp_suffixes = Suffix_guesser.empty ;
		eos  = eos_ix;
		bos = bos_ix;
		apriori_tag_probs = Array.make 0 0.0;
		theta = 0.0;
	
	} in
	
	let stat = {
		tokens = 0;
		sentences = 0;
		types = 0;
		rare_low = 0;
		rare_upp = 0;
	} in
	(m, stat)
;;

	
let add_sentence (m, stat) (words, tags) =
		stat.sentences <- stat.sentences + 1;
		
		(* atallunk intekre *)
		let tags = List.map (Vocab.toindex m.tag_vocab) tags in
		(* biztos, ami biztos, zaro *)
	    m.tag_lm <- TagProbLM.add_word m.tag_lm tags m.tag_order m.eos;
		let tags = tags @ (m.bos :: []) in
		let words = words @ ("<S>" :: []) in
			
		let rec aux words tags = match words, tags with
			wbos::[], tbos::[] -> (* bos-t nem kell betenni *) ()
			| (word::word_tails),
			(tag::tag_tails)   ->
				stat.tokens <- stat.tokens + 1;
				ObsLexicon.add_word_tag m.obs_lex word tag;	
				m.tag_lm <- TagProbLM.add_word m.tag_lm tag_tails m.tag_order tag;
			    m.obs_lm <- ObsProbLM.add_word m.obs_lm tags m.emission_order word;
				let (is_spec, name) = Special_tokens.to_lex word in
				if is_spec then begin
					m.spec_lm <- ObsProbLM.add_word m.spec_lm tags 2 name;
					ObsLexicon.add_word_tag m.spec_lex word tag;
				end;	
				aux word_tails tag_tails
			| (_,_) -> ()
		in
		aux words tags
;;
	
let calculate_probs (m,stat) =
	(* apriori cimke valoszinusegek szamitasa *)
	let total_freq = TagProbLM.total_context_freq m.tag_lm in
	let tag_types  = Vocab.max m.tag_vocab in

	m.apriori_tag_probs <- Array.make (tag_types) 0.0;
	TagProbLM.iter_words (fun tag freq -> 
		try m.apriori_tag_probs.(tag) <-  ( freq /. total_freq) with _ -> Printf.printf "error %d %s\n" tag (Vocab.toword m.tag_vocab tag)) m.tag_lm;	

	let tlambdas = TagProbLM.calculate_lambdas m.tag_lm m.tag_order in
	let olambdas = ObsProbLM.calculate_lambdas m.obs_lm m.emission_order in
	
	let slambdas = ObsProbLM.calculate_lambdas m.spec_lm 1 in
	 
	TagProbLM.counts_to_prob m.tag_lm tlambdas;
	ObsProbLM.counts_to_prob m.obs_lm olambdas;
	ObsProbLM.counts_to_prob m.spec_lm slambdas;
	
;;

let build_suffixtries (m,stat) maxfreq maxlength =
(*	for i = 0 to (Array.length m.apriori_tag_probs - 1) do
		Printf.printf "%d %s %f\n"i
		(Vocab.toword m.tag_vocab i) m.apriori_tag_probs.(i);
	done;
*)	let do_word word freq tags =
	
		if freq <= maxfreq then begin
			
			let (lword, is_upper) = Io.lowercase word in
			let add_tag (tag, freq) =
				if is_upper then begin
					m.upp_suffixes <- Suffix_guesser.add_word m.upp_suffixes maxlength lword tag freq;
					stat.rare_upp <- stat.rare_upp + freq
				end
				else begin
					m.low_suffixes <- Suffix_guesser.add_word m.low_suffixes maxlength lword tag freq;
					stat.rare_low <- stat.rare_low + freq
				end
			in
			List.iter add_tag tags
		end
	in
	
	ObsLexicon.iter do_word m.obs_lex;

	(* theta szamolasa *)
	m.theta <- Suffix_guesser.calculate_theta m.apriori_tag_probs;

;;
		
let print_stat (m,stat) =
    prerr_endline "Traning corpus:";
    prerr_int stat.tokens; prerr_endline " tokens";
    prerr_int stat.sentences; prerr_endline " sentences";
    prerr_int (Vocab.max m.tag_vocab); prerr_endline " different tag";
    
    prerr_endline "\nGuesser trained with";
    
    prerr_int stat.rare_low; prerr_endline " lowercase ";
	prerr_int stat.rare_upp; prerr_endline " uppercase tokens";
	prerr_string "theta = "; prerr_float m.theta; prerr_newline()
;;
	
let save (m, stat) file_name =
	let oc = open_out_bin file_name in
	Marshal.to_channel oc (m, stat, m.apriori_tag_probs) [];
	close_out oc;
;;

let load file_name =
	let ic = open_in_bin file_name in
	let (m, stat, apriori_tag_probs) = Marshal.from_channel ic in
	close_in ic;
	m.apriori_tag_probs <- apriori_tag_probs ;
	(m, stat)	

type seen_type = Seen | LowerCasedSeen | SpecialToken | UnSeen

type observation = {word : string;
 					mutable is_first : bool; (* eleg ronda hack arra, h az elso szot kisbetusithessuk *)
					mutable seen : seen_type; 
					mutable oov : bool; 
					mutable anals : string list;
					mutable guessed: (string * float ) list;
					}

let compile_tagger (m, stat) morphtable  max_guessed_tags logtheta = 
	let tag_order = m.tag_order in
	let emission_order = m.emission_order in
	let (ltagprob, ltagprobs) = Suffix_guesser.guesser_from_trie 
									m.low_suffixes  m.theta in
									
	let (utagprob, utagprobs) = Suffix_guesser.guesser_from_trie 
									m.upp_suffixes m.theta in
	let suffix_accu = Array.make (Array.length m.apriori_tag_probs) 0.0 in
	let suffix_accu_length = Array.length suffix_accu in
	let max_known_tagid = Vocab.max m.tag_vocab in
		
let module State = struct
	type t = int list
	
	let compare ng1 ng2 =
		let rec compare ngram1 ngram2 n =
			if n <= 0 then 0 else
			match (ngram1, ngram2) with
				| ((t1:int)::h1, t2::h2)  -> 
					if t1 < t2 then -1
					else if t1 > t2 then 1
					else compare h1 h2 (pred n)
				| ([] , []) -> 0	(* ha mindketto ures, akkor egyenloek *)
				| (t::_ , [] ) -> 1
				| ([], t::_)  -> -1
		in
		compare ng1 ng2 tag_order
		
	let equal ng1 ng2 = compare ng1 ng2 = 0
	let hash ng = 
		let rec aux ngram n h = 
			if n <=0  then h else
			match ngram with
			[] -> h
			| (gram:int)::t -> aux t (pred n) ((Hashtbl.hash gram) + h )
		in
		aux ng tag_order 0
			
	
end 
in

let module StateViterbi = Viterbi.Make(Mfhash.Make(State))
in



let start_state = 
	let rec aux n l = if n < 0 then l 
		              else aux (n - 1) (m.bos :: l) 
	in aux tag_order []
in

let end_of_sentence =
    let transition from = (Ngram.add m.eos from, 
						   TagProbLM.wordprob m.tag_lm m.eos from)::[] in
	let emission state = 1.0 in
	(transition, emission)
in

(* fogja a feltoltott suffix_accu -t es abbol kivalaszt nehanyat, amivel tovabb
	megyunk. Most veszi az elso 20-t, amik azert minnel nagyobbak. *)
let k = max_guessed_tags in
let suf_theta = log 10. in
let prune_guessing max =
	let min = max -. suf_theta in
	let l = ref [] in
	let n = ref 0 in
	(* az eleg nagy sulyu elemek kivalasztasa *)
	let add_to_list (tag:int) (w:float) =
		if w > min then begin
			incr n;
			l := (tag, w) :: !l
		end
	in
	Array.iteri (add_to_list) suffix_accu;
	if !n < k then !l
	else
	(* ha tobb, mint k elem lett, akkor vesszuk az elso k-t *)
	let compar (_, (w1:float)) (_, (w2:float)) = compare w2 w1 in 
	let sorted = List.sort compar !l in
	l := [];
	n := 0;
	let rec aux sorted acc =
		match sorted with
			h::t when !n < k -> incr n; aux t (h :: acc)
		  | _ -> acc
	in
	aux sorted []
in
	
let next obs =
		let w = obs.word in
		(* this is the end of sentence token; calculating only transition probs *)
		if w = "<s>" then
			end_of_sentence
		else 
		begin
		let debug = false in (*  w = "Windowst" in *)
		(* has any uppercased char? *)
		let (lw, is_upper) = Io.lowercase w in
	
		(* is it known words? *)	
		let (oov, anals) = try (false,  morphtable ( w)) 
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
			let (wordprob, tags, seen) = 
			try 
			    let tags = ObsLexicon.find_nofreq m.obs_lex w in
				(ObsProbLM.wordprob m.obs_lm w, tags, Seen)
			 with Not_found ->
			 try
				if obs.is_first && is_upper then
					let tags = 	ObsLexicon.find_nofreq m.obs_lex lw in
					(ObsProbLM.wordprob m.obs_lm lw, tags, LowerCasedSeen)
				else raise Not_found
			 with Not_found ->
			 	let (is_spec, name) = Special_tokens.to_lex w in
				if is_spec then
					let tags = 	ObsLexicon.find_nofreq m.spec_lex name in
					(ObsProbLM.wordprob m.spec_lm name, tags, SpecialToken)
					
				else raise Not_found
		
			in
			obs.seen <- seen;
			
			
			let transition from = 
				List.map (fun tag -> 
							
							let next_state = Ngram.add tag from in
							let tp = TagProbLM.wordprob m.tag_lm tag from in
								
								
									
							(next_state, (tp )) 
				) tags
			in
			let emission state =
				let p = wordprob state in
			
				p
				
			in
			(transition, emission)		
					
		with Not_found -> (* not seen word *)
			obs.seen <- UnSeen;
			let anals2transtion_fun anals =
				let transition from = List.map (fun tag  ->
						let tagid = Vocab.toindex m.tag_vocab tag in
						let next_state = Ngram.add tagid from in
						let tp = TagProbLM.wordprob m.tag_lm tagid from in
							
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
					let emission state = 
						let tag = List.hd state in
						if tag > (max_known_tagid - 1) then -99.0
						else
							(tagprob lw tag  -. (log m.apriori_tag_probs.(tag)))
						in
					(transition, emission)
				else begin
				(* lekerdezzuk a suffix guessertol a tagid->prob parokat *)
				let max_value = tagprobs  (lw) suffix_accu in
				let pruned_guessing = prune_guessing max_value in
				
				let transition 	from = 
					List.map (fun (tagid,w) -> 
							let next_state = Ngram.add tagid from in
							let tp = TagProbLM.wordprob m.tag_lm tagid from in
								if debug then
								begin
									Printf.printf "trans %s %f eredeti w=%f\n" (Vocab.toword m.tag_vocab tagid) tp w;
								end;
					(next_state, (tp )) 
				) pruned_guessing			
				in
				let emission state =
					let tagid = List.hd state in
					let prob =  suffix_accu.(tagid)  -. (log m.apriori_tag_probs.(tagid)) in
							if debug then
							begin
								Printf.printf "emission %s %f\n" (Vocab.toword m.tag_vocab tagid) prob;
							end;
					prob
				in
				(transition, emission)
			end
		end		
in		
		

let tag_sentence words  =


	let word2observation w = { word = w; is_first=false; seen = Seen; oov = false; anals = []; guessed = []} in

	let first::observations = 	List.map (word2observation) (List.rev ( "<s>"::words))  in
	first.is_first <- true;
	let observations = first :: observations in
	let state_seq = StateViterbi.decode  next logtheta start_state observations in
	let state_seq = List.map (fun state -> let tag_id = Ngram.newest state in Vocab.toword m.tag_vocab tag_id) state_seq in
		
	( List.tl (List.rev  observations), List.tl (List.rev  (state_seq)))
		
 in tag_sentence

