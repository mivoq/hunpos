(* This is a language model to calculate P(C | A, B) with linear
   interpolation i.e
   P(C| A B) = l3 ML (C| A B) + l2 ML (C | B) + l1 ML (C) + l0
   	
   the calculation of lambdas are the same to Brants 2000.

   The datastructure is similar to the datastructure of SRILM:
   there is a context tree holding the null context as root (unigrams)
   B for bigrams starting with B , and B->A node for trigrams staring with AB.
   Every node of the context node has a word map storing the frequency of
   the words given the context.
   
   We don't need that the contexts and the words have the same type,
   this is important for tagging where tags are integers but words
   are strings.
   
   This module first can calculate the frequencies, than calculate the
   lambdas of linear interpolation and finally transform frequencies to
   probabilities.
   
   TODO: the freq counting and the lambda calculation should be separeted.
    	
*)

module type S = sig
	type t
	type word
	type context 
	val empty_freq_counter : unit ->  t
	val add_word :  t -> (context list) -> int -> word ->  t
	val total_context_freq : t -> float
    val iter_words : (word -> float -> unit) -> t -> unit
    val calculate_lambdas : t -> int -> float array
    val counts_to_prob : t -> float array -> unit
    val wordprob : t -> word -> context list -> float
 
 (*
    val get_words_from_node : t -> float M.WMap.t
     val add_word : t -> M.CMap.key list -> int -> M.WMap.key -> t
     val iterate_paths : (int -> t list -> unit) -> t -> int -> unit
     val total_context_freq : t -> float
     val word_count_at_context : t -> int
     val iter_words : (M.WMap.key -> float -> unit) -> t -> unit
     val iter_ngrams :
       (M.CMap.key list -> M.WMap.key -> float -> unit) -> t -> unit
     val freqs : t -> M.WMap.key -> M.CMap.key list -> (float * float) list
 *)
 
end

module type Maps = sig
	module  WMap : Amap.S
	module  CMap : Amap.S

end


module Make(M : Maps) : (S with type word = M.WMap.key and type context=M.CMap.key) = struct

	
type word = M.WMap.key
type context =  M.CMap.key
		
type  t = Parent of (float * t  M.CMap.t *  float M.WMap.t) |
			Terminal of (float * float M.WMap.t)


let empty_freq_counter () = Terminal(0.0, M.WMap.empty ())


let get_words_from_node node =
	match node with
		Terminal(_, words) -> words
		| Parent(_, _, words) -> words


let add_word context_node context n word  =
    (* context is a list of [B; A] if you add A B C trigram
       just go down in the tree and add word at every level *)
 
	let rec add_word context_node context n  =
	    (* adding word *)
	    let words = get_words_from_node context_node in
	    let _ = M.WMap.update (fun () -> 1.0) (fun x -> x +. 1.0) words word  in
	
	    (* functional tree stepping *)
	    match context with
		    head :: tail when n > 0 -> 
	
		        let freq, childs = match context_node with
			        Terminal(freq, _) -> freq, M.CMap.empty()
			        | Parent(freq, childs,_) -> freq, childs
		        in
		        let _ = M.CMap.update  
				    (fun () -> add_word (empty_freq_counter ()) tail (pred n) )
				    (fun child -> add_word child tail (pred n))
				    childs head
		        in
		        Parent(freq +. 1.0, childs, words)
 	        | _ -> 
 	            begin
		            match context_node with
				        Parent(freq, childs, _) -> Parent(freq +. 1.0, childs, words)
				        | Terminal(freq, _) -> Terminal(freq +. 1.0, words)
	            end		
	in 

	add_word context_node context n 
	
(* iterate over the context tree and calls f with the node iif
   gets to a terminal or reaches max level
*)
let iterate_paths f context_node max_level =
	let rec aux node acc level  =
		if level = max_level then
			f level (node :: acc)
		else
			(* meg megyunk lefele *)
		match node with
			Terminal(_, _) -> f level (node :: acc) 
			| Parent(freq, childs, words) -> 
				let acc = node :: acc in
				M.CMap.iter (fun gram child -> aux child acc (succ level)) childs
	in
	aux context_node [] 0



let total_context_freq node =
	match node with
		Terminal(freq, _) -> freq
		| Parent(freq, _, words) -> freq
;;

let word_count_at_context node =
	let words = get_words_from_node node
	in M.WMap.size words
	
let iter_words f node =
	let words = get_words_from_node node in
	M.WMap.iter f words
	
let iter_ngrams f trie =
	let rec aux node acc =
		iter_words (f acc) node;
		match node with
			Parent(_,childs, _) ->
				let do_child gram node =
					aux node (gram::acc)
				in
				M.CMap.iter do_child childs
			| _ -> ()
	in
	aux trie []
		
(** FROM THIS: HANDLING PROBABILITES
    *)
    
let calculate_lambdas context_node level =
	let lambdas = Array.create (level+2) 0.0 in
	
	
    (* see Brants 2000 (http://citeseer.ist.psu.edu/brants00tnt.html) 
       this algorithm is detailed on Figure 1.
    *)
	let adjust_lambda level context_nodes =
	    
	    (* this is "for each trigram" *)
		let do_word word freq = 
			let rec search_max  nodes i max maxi =
				
				match nodes with 
					node::tail ->
						let context_freq, words = match node with
							Terminal(freq, words) -> freq, words
				   			| Parent(freq, _, words) -> freq, words
						in
						let word_freq = try M.WMap.find words word  
										with Not_found -> failwith "error in the tree";
						in
						
						let ratio = 
						    (* note. Brants doesn't discuss the case of trigrams
						       with frequency of 1. [i think this is a mistake in 
						       his paper]. We increment lambda_0 in this case
						       which is maxi = 0 in the first call of search_max
						    *)
							if context_freq = 1.0 || word_freq = 1.0 then -1.0 
						    else  (word_freq -. 1.0) /. (context_freq -. 1.0)
						in
						let max, maxi = if ratio > max then (ratio, i) else (max, maxi) in
						search_max  tail (pred i) max maxi
					| _ -> (max,maxi)
			
			in
			let (max, maxi) = search_max context_nodes (level + 1) (0.0) (0) in 
			lambdas.(maxi) <-lambdas.(maxi) +. freq;
		in
		let words = get_words_from_node (List.hd context_nodes) in
		M.WMap.iter (do_word) words
	in
	iterate_paths adjust_lambda context_node level;
	lambdas.(0) <- 0.0;
	
	(* normalize lambdas *)
	let sum = Array.fold_left (fun sum x -> sum +. x) 0.0 lambdas in
	let lambdas = Array.map (fun x ->  x /. sum) lambdas in
	lambdas
	
(* translate frequencies to log probabilities. At the level n we
    has to know the probability of word at level n-1. For example
    
	P(C| A B) = l3 ML (C| A B) + l2 ML (C | B) + l1 ML (C) + l0, which is
	P(C| A B) = l3 ML (C| A B) + P (C | B) 
	
	P(C|B) is calculated first
*)
let counts_to_prob context_trie lambdas =

	let rec estimate_at_context node parent_words lambdas =
		match lambdas with
			[] -> (* if there is no more lambda we can't calculate
			    	 the probs of larger ngrams. But this solution
			    	 could cause error!? *)
					()
			| l :: tl -> begin
				let context_freq, words = match node with
						Terminal(freq, words) -> freq, words
					  | Parent(freq, _, words) -> freq, words
				in
				(* calc prob of  all words *)
				let word_updater word freq =
					try
					let prob_to = M.WMap.find parent_words word in
					prob_to +. l *. (freq /. context_freq)	  
					with Not_found -> failwith ("hmm. some error in the tree")
				in
				M.WMap.update_all words word_updater;
				
				(* walking in the tree *)
				match node with
					Terminal(_,_) -> ()
					| Parent(_, childs, _) ->
					M.CMap.iter (fun gram child -> 
									estimate_at_context child words tl) childs
			end
	in
	(* first is l0 then unigram... *)
	match  (Array.to_list lambdas) with
	[] -> failwith "List.length lambdas < 1"
	| l0 :: l1::lambdas -> begin
	
	let null_context_freq, childs, words = match context_trie with
			Terminal(freq, words) -> failwith ("empty context_trie")
		  | Parent(freq, childs, words) -> freq, childs, words
	in
	(* the first level is a bit different from the other because of the
	    lazyness of the programmer
	*)
	M.WMap.update_all words (fun word freq -> l0 +. l1 *.(freq /. null_context_freq));
	M.CMap.iter (fun gram child -> 
						estimate_at_context child words lambdas) childs
	end
	
(* go down to the max level where the word can be found *)
let wordprob trie word context =
	let rec aux node context prob_to =
		let words = get_words_from_node node in
		try
			let prob_here = M.WMap.find words word in
		
			(* can we continue? *)
			match (node, context) with 
				(Parent(_, childs, _), h::t) ->
				    begin try
			            let child_node = M.CMap.find childs h in
						aux child_node t prob_here;
					with Not_found ->  prob_here
				    end
			  | (_,_) -> (* no *)   prob_here
		with Not_found ->   prob_to
	in
	log (aux trie context 0.0)

(* TODO is this functio used? *)
let freqs trie word context =
	let rec aux node context acc =
		let words = get_words_from_node node in
			try
			let freq_here = M.WMap.find words word in
			match node with
				Parent(cfreq, childs, _) ->
					begin
						let acc = (freq_here, cfreq) :: acc in
						match context with
							h::t ->
									let child_node = M.CMap.find childs h in
									Printf.printf "found a child\n";
									aux child_node t ( acc);
							| _ -> acc
					end
			  | Terminal(cfreq,_) -> (freq_here, cfreq)::acc
			with Not_found -> failwith ("no word")
	in
	aux trie context []
			
end

