
(* The task of the suffix guesser is to predict a tag-distribution based on the suffix of the word.
In training phase, it calculates for each suffix its count in the corpus, in total and for each tag separately.
Let's assume a word ending with ABCDE. During prediction, it linearly interpolates the looked-up predictions
for the ABCDE, BCDE, CDE, DE, E, "" suffices. Interpolation is done successively with weights 1 and theta,
so weights are basically powers of 1/(1+theta), with the shorter suffix getting the larger weight. *)

module C = Map.Make (struct type t = char let compare = compare end)
(*module T = Assoclist.Make(struct type t = string let equal s1 s2 = s1 = s2 end)
*)
module T = Map.Make (struct type t = int let compare = compare end)


type trie_node = Terminal | OneChild of char * count_node | Branch of count_node C.t and
counts = (int * int T.t) and
count_node =  Empty | Node of  trie_node * counts option

type t = count_node

let empty_counts =  (0, T.empty)

let empty = Empty
	
let add_word trie n word tag count =

(*	print_string "add word: "; print_string word; print_char ' '; print_int (String.length word); print_newline ();
*)	let start = (String.length word - 1) in
	let stop = max 0 (start - n) in
	let rec add_char node legacy_counts after_branch ix =
		let update (scount, tagcounts) =
			let tagcount = try T.find  tag tagcounts with Not_found -> 0 in
			let tagcounts = T.add tag (tagcount + count) tagcounts in
			(scount + count, tagcounts)
		in
		let inherited_counts, new_counts = match node with
			Empty -> legacy_counts, None
			| Node(_, Some x) ->  x, Some(update x)
			| _ -> legacy_counts, None
		in

		match node with
			Empty when ix < stop -> Node(Terminal, if after_branch then Some (update inherited_counts) else None)
		  | Node(OneChild(c, child),x) when ix < stop ->
						(* after abc, we also insert bc. ; TODOMAGYAR eddigi gyerekhez adunk counts - ot *)
						(* bc utan bc -vel mi van? *)
						let child = match child with
						
							| Node (trie_node , None) -> Node(trie_node, (Some inherited_counts))
							| _ ->  child
						in
							Node (OneChild(c, child), if after_branch then Some (update inherited_counts) else None)

						
					
		  | Node(trie_node, _) when ix < stop ->
			      		Node(trie_node, if after_branch then Some (update inherited_counts) else None)
		
		  | Empty -> 
						let child = add_char Empty inherited_counts false (ix - 1) in
						Node (OneChild(word.[ix], child), if after_branch then Some (update inherited_counts) else None)
		 
		  | Node(Terminal, _) ->
						(* bc utan abc hozzaadasa*)
						let child = add_char Empty empty_counts true (ix - 1) in
						Node(OneChild(word.[ix], child), if after_branch then Some (update inherited_counts) else None)
		  
		  | Node(OneChild(c', child),_) when word.[ix] = c'->
							let oldchild = 
							 begin
								
									(* after abc comes xabc *)
									add_char child inherited_counts false (ix - 1)
								end
							in
							Node(OneChild(c', oldchild), if after_branch then Some (update inherited_counts) else None)
		 
		  | Node(OneChild(c', child),_)  ->
				
							(* after bca we insert dca or efca *)
							(* we have to pass the tag_info to the former children *)
							let oldchild = match child with
								Node (trie_node, None) -> Node(trie_node, (Some inherited_counts))
								| _ -> child
							in
							(* map for the children *)

							let childs = C.add c' oldchild C.empty  in
							let newchild = add_char Empty empty_counts true (ix - 1) in
							let childs = C.add word.[ix] newchild childs in
							Node(Branch (childs), if after_branch then Some (update inherited_counts) else None)
			| Node(Branch(childs),_) ->
							
							let c = word.[ix] in
							let child = 
							try 
								let child =  C.find  c childs in
								add_char child inherited_counts true (ix - 1) 
							with Not_found -> add_char Empty empty_counts true (ix - 1) 
												
							 in
							 let childs = C.add c child childs in
						Node(Branch(childs),  if after_branch then Some (update inherited_counts) else None)
	in
	add_char trie empty_counts true start

;;
		
let calculate_theta apriori_tag_probs =

	
	let pow n = n *. n in
	(*let p_av = 1.0 /. s in   (*-- use uniform distribution over tag-probs *)
	*)
	(* we follow libmmoot's solution here, not Brants 2000's formula.
	   p_av = E_{P_t}(P_t()) [weighted avg: stddev]
		
	*)
	let p_av = Array.fold_left (fun sum tagp -> sum +. (pow tagp)) 0.0 apriori_tag_probs in	
	let theta = Array.fold_left (fun sum tagp -> sum +.  tagp *.pow (tagp -. p_av)) 0.0 apriori_tag_probs in
		
	sqrt(theta )
;;



let guesser_from_trie trie theta  =
	let theta_plus_one = theta +. 1.0 in

	let trie_iterator word f = 

		let start = (String.length word - 1) in
		let stop = 0 in

		let rec aux (Node(trie_node, tag_info)) legacy_counts ix  =
			let ((scount, tag_counts) as inherited_counts ) = match tag_info with
				Some(x) -> x
				| _ -> legacy_counts 
			in
			let scount = float scount in

			f scount tag_counts ;

			if ix >= stop then
			match trie_node with
				Terminal -> ()
	               | OneChild(c, child) -> if c = word.[ix] then aux child  inherited_counts (ix - 1) 
				   | Branch(childs)     -> try 
									  	      let child = C.find word.[ix] childs in
										      aux child  inherited_counts (ix - 1) 
								  	          with Not_found -> ()		
		in
		aux trie (0, T.empty) start; 

	in

	let tag_prob word tagid =
	
		let accu = ref 0.0 in
		let roll_prob suff_count tag_counts =
			let tag_prob = try (float (T.find  tagid tag_counts)) /. suff_count with Not_found -> 0.0 in
	        accu := (!accu     +. (tag_prob *. theta)) /. theta_plus_one 

		in
		trie_iterator word roll_prob;
		accu :=  log !accu ;
		!accu
	in	

	let tagprobs  word accu =
		(* zero out the accu *)
		for i= 0 to (Array.length accu) - 1 do
			accu.(i) <- 0.0
		done;
		let start = (String.length word - 1) in
		let stop =  0 in

		let roll_prob suff_count tag_counts =
				let calc_tag_prob tagid freq =
					let tag_prob = float freq /. suff_count in
					accu.(tagid) <- (accu.(tagid) +. (tag_prob  *. theta )) /. theta_plus_one;
				in
				T.iter (calc_tag_prob) tag_counts;	
		in
		trie_iterator word roll_prob;
		(* take logarithm and search for the maximum.
		bayesian inversion is not done here anymore, it is now done in hmm_tagger.
		This way 1. a maxent model will more easily be pluggable here.
		         2. the hmm_tagger can throw away unlikely tags before inversion.
		*)
		let max = ref neg_infinity in
		for i = 0 to (Array.length accu - 1)  do
	(*		Printf.printf "P(t|w)=%f P(t)=%f P(w|t)=%f\n" (log accu.(i)) apriori_tag_probs.(i) (log accu.(i)  -. log apriori_tag_probs.(i) );
	*)		let prob = log accu.(i)   in
			accu.(i) <- prob;
			if prob > !max then max := prob
		done;
	
	   !max
		in
	   (tag_prob, tagprobs)
	

;;
