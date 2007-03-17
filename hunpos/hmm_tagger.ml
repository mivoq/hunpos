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
	
		let rec aux words tags = match words, tags with
			(word::word_tails),
			(tag::tag_tails)   ->
				stat.tokens <- stat.tokens + 1;
				
				ObsLexicon.add_word_tag m.obs_lex word tag;	
				m.tag_lm <- TagProbLM.add_word m.tag_lm tag_tails m.tag_order tag;
                m.obs_lm <- ObsProbLM.add_word m.obs_lm tag_tails m.emission_order word;
				let (is_spec, name) = Special_tokens.to_lex word in
				if is_spec then begin
					m.spec_lm <- ObsProbLM.add_word m.spec_lm tag_tails m.emission_order name;
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
	let tag_types  = TagProbLM.word_count_at_context m.tag_lm in
	m.apriori_tag_probs <- Array.make (tag_types) neg_infinity;
	TagProbLM.iter_words (fun tag freq -> m.apriori_tag_probs.(tag) <-  ( freq /. total_freq)) m.tag_lm;	
	
	prerr_endline "apriori tag probs calculated";
	
	
	
	let tlambdas = TagProbLM.calculate_lambdas m.tag_lm m.tag_order in
	prerr_endline "tag lambdas calculated";
   (* Array.iteri (fun i v -> Printf.eprintf "%d = %f\n" i v) tlambdas;
 *)
	let olambdas = ObsProbLM.calculate_lambdas m.obs_lm m.emission_order in
	prerr_endline "observation lambdas calculated";
(*	Array.iteri (fun i v -> Printf.eprintf "%d = %f\n" i v) olambdas;
*)	
	let slambdas = ObsProbLM.calculate_lambdas m.spec_lm m.emission_order in
	prerr_endline "spec token lambdas calculated";
(*)	Array.iteri (fun i v -> Printf.eprintf "%d = %f\n" i v) slambdas;
*)	
	 
	TagProbLM.counts_to_prob m.tag_lm tlambdas;
	prerr_endline "tag probs calculated";
	ObsProbLM.counts_to_prob m.obs_lm olambdas;
	prerr_endline "observation probs calculated";
	ObsProbLM.counts_to_prob m.spec_lm slambdas;
	prerr_endline "spec token probs calculated";
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
	prerr_string "theta = "; prerr_float m.theta; prerr_newline();
;;
		
let save (m, stat) file_name =
	let oc = open_out file_name in
	Marshal.to_channel oc (m, stat, m.apriori_tag_probs) [];
	close_out oc;
;;

let load file_name =
	let ic = open_in file_name in
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

let compile_tagger (m, stat) morphtable tag_order emission_order = 
	
	let (ltagprob, ltagprobs) = Suffix_guesser.guesser_from_trie 
									m.low_suffixes m.apriori_tag_probs m.theta in
									
	let (utagprob, utagprobs) = Suffix_guesser.guesser_from_trie 
									m.upp_suffixes m.apriori_tag_probs m.theta in
	prerr_endline "guessers initialized";						
	let suffix_accu = Array.make (Array.length m.apriori_tag_probs) 0.0 in
	let suffix_accu_length = Array.length suffix_accu in
		
let module State = struct
	type t = int Ngram.t
	let compare ng1 ng2  = Ngram.compare ng1 ng2 tag_order
	let print ngram = Ngram.print ngram tag_order (print_int)
end 
in

let module StateViterbi = Viterbi.Make(Ocamap.Make(State))
in



let start_state = 
	let rec aux n l = if n < 0 then l 
		              else aux (n - 1) (TagProbLM.bos :: l) 
	in aux tag_order []
in

let end_of_sentence =
    let transition from = (Ngram.add m.eos from, 
						   TagProbLM.wordprob m.tag_lm m.eos from)::[] in
	let emission state = 1.0 in
	(transition, emission)
in

let next obs =
		let w = obs.word in
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
				wordprob state
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
				let emission state = 1.0 in
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
						try 
						tagprob lw tag 
						with Not_found -> Printf.eprintf "new tag for %s" w; -99.0
						in
					(transition, emission)
				else
				(* lekerdezzuk a suffix guessertol a tagid->prob parokat *)
				
				let max_value = tagprobs  (lw) suffix_accu in
				let min_value = max_value -. log 1000. in
				(* listaba tesszuk, ami megfelel nekunk *)
				let rec aux acc ix =
					if ix <0 then acc else 
					if suffix_accu.(ix) > min_value then aux ((ix) :: acc) (ix - 1)
					else aux acc (ix - 1)
				in
				let possible_tags = aux [] (suffix_accu_length - 1) in
						
				let transition 	from = 
					List.map (fun (tagid) -> 
							let next_state = Ngram.add tagid from in
							let tp = TagProbLM.wordprob m.tag_lm tagid from in
					(next_state, (tp )) 
				) possible_tags			
				in
				let emission state =
					let tagid = List.hd state in
					suffix_accu.(tagid); 
				in
				(transition, emission)		
in		
		

let tag_sentence words  =


	let word2observation w = { word = w; is_first=false; seen = Seen; oov = false; anals = []; guessed = []} in

	let first::observations = 	List.map (word2observation) (List.rev ( "<s>"::words))  in
	first.is_first <- true;
	let observations = first :: observations in
	let state_seq = StateViterbi.decode  next start_state observations in
	let state_seq = List.map (fun state -> let tag_id = Ngram.newest state in Vocab.toword m.tag_vocab tag_id) state_seq in
		
	( List.tl (List.rev  observations), List.tl (List.rev  (state_seq)))
		
 in tag_sentence
