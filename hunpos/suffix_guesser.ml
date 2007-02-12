module C = Map.Make (struct type t = char let compare = compare end)
(*module T = Assoclist.Make(struct type t = string let equal s1 s2 = s1 = s2 end)
*)
module T = Map.Make (struct type t = int let compare = compare end)
let n = 10 ;;

type trie_node = Terminal | OneChild of char * count_node | Branch of count_node C.t and
counts = (int * int T.t) and
count_node =  Empty | Node of  trie_node * counts option



let empty_counts =  (0, T.empty)

let empty = Empty
	
let add_word trie word tag count =
	let start = (String.length word - 1) in
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
						(* abc utan bc - t is beszurunk; eddigi gyerekhez adunk counts - ot*)
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
								
									(* abc utan xabc jon *)
									add_char child inherited_counts false (ix - 1)
								end
							in
							Node(OneChild(c', oldchild), if after_branch then Some (update inherited_counts) else None)
		 
		  | Node(OneChild(c', child),_)  ->
				
							(* bca utan dca -t v. efca-t szurunk be *)
							(* eddig gyereknek at kell adnunk a mostani tag_infot *)
							let oldchild = match child with
								Node (trie_node, None) -> Node(trie_node, (Some inherited_counts))
								| _ -> child
							in
							(* map a gyerekeknek *)

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
	
let theta trie =
	match trie with 
		Node (_, Some (count , tags)) ->
				let pow i = i *. i in
				let count = float count in
				let s = ref 0 in
				T.iter (fun tag freq -> incr s) tags;
				let s = float (!s) in
				let p_av = 1.0 /. s in   (*-- use uniform distribution over tag-probs *)
				let theta = ref 0.0 in
				T.iter (fun tag tagfreq -> let tagp =  (float) tagfreq /. count in
                        theta := !theta +. pow (tagp -. p_av)) tags;
				!theta /. (s -. 1.)
		| _ -> failwith("furcsa trie")



(*
let calculate_probs trie theta =
	let rec aux node legacy_counts legacy_probs =
		(* minden csomopontbol csinal egy ujat; ha volt ott counts, akkor kicsereli valoszinusegekre *)
*)	


let guesser_from_trie trie theta vocab =
	let mx = Vocab.max vocab - 1  in
	let theta_plus_one = theta +. 1.0 in
	let log_alfa = log 10000. in
	let apriori_tag_prob = Array.make (mx + 1) 0.0 in
	let (childs) = 
	match trie with
		Node(Branch(childs), Some (total, tag_counts)) ->
			let total = float total in
			T.iter (fun tagid freq -> 
						apriori_tag_prob.(tagid) <-  log (float freq /. total) 
			) tag_counts ;
			childs
						
		| _ -> failwith ("furcsa trie")
	in
	
		
let tagprobs  word  =

	let start = (String.length word - 1) in
	let stop = max 0 (start - n) in
    let accu = Array.make (mx + 1) 0.0 in
		
	let rec aux (Node(trie_node, tag_info)) legacy_counts ix  =

			let ((scount, tag_counts) as inherited_counts ) = match tag_info with
				Some(x) -> x
				| _ -> legacy_counts 
			in
			let scount = float scount in
			
			for i = 1 to mx  do
				accu.(i) <- accu.(i) *. theta;
			done;
			
			T.iter (fun tagid freq -> accu.(tagid) <- accu.(tagid) +. (float freq) /. scount ) tag_counts;	
		
			for i = 1 to mx  do
				accu.(i) = accu.(i) /. theta_plus_one
			done;
		
		
			if ix >= stop then
		
			match trie_node with
				Terminal -> ()
                | OneChild(c, child) -> if c = word.[ix] then aux child  inherited_counts (ix - 1) 
				 					  
				|Branch(childs) -> try 
									let child = C.find word.[ix] childs in
									aux child  inherited_counts (ix - 1) 
								  with Not_found -> ()
	in
	aux trie (0, T.empty) start;
			(* to log, bayes inversion and search the maximum *)
			let max = ref neg_infinity in
			for i = 1 to mx  do
				let prob = log accu.(i)  -. apriori_tag_prob.(i)  in
				accu.(i) <- prob;
				if prob > !max then max := prob
			done;
			let min = !max -. log_alfa in
			(* convert to list and do beam pruning *)
			let tagprobs = ref [] in
			for i = 1 to mx  do
				let prob = accu.(i) in
				if prob > min then
					tagprobs := (Vocab.toword vocab i, prob) :: !tagprobs
			done;
			!tagprobs
   in
	tagprobs
(*
let tagprobs trie theta word =
	let theta_plus_one = theta +. 1.0 in
	let start = (String.length word - 1) in
	let stop = max 0 (start - n) in
			let debug = if word = "Zamárdiba" then true else false in	
	(* bayes inversion 
	   P(suffix | tag ) = P(tag and suffix) / P(tag) 
	   P(tag|suffix) = P(tag and suffix) / P(suffix) 
	   P(suffix | tag ) = P(tag|suffix) * P(suffix) / P(tag)
	*)
			
	let rec aux (Node(trie_node, tag_info)) prev_tag_info ix  probs =

			let ((scount, tag_counts) as new_tag_info) = match tag_info with
				Some(x) -> x
				| _ -> prev_tag_info 
			in
			let scount = float scount in
			let probs = P.mapi (fun tag prob ->
                        let tag_count = try T.find tag tag_counts with Not_found -> 0 in
									if debug then begin
					                Printf.printf "%s %f %d\n" tag prob tag_count
									end;
								
						(theta *. prob +. (float tag_count /. scount)) /. theta_plus_one 
				 ) probs
			in
			if ix < stop then probs
			else
		
			match trie_node with
				Terminal -> probs
                | OneChild(c, child) -> if c = word.[ix] then aux child  new_tag_info (ix - 1) probs
				 					  else probs
				|Branch(childs) -> try 
									let child = C.find word.[ix] childs in
									aux child  new_tag_info (ix - 1) probs
								with Not_found -> probs 
			
	in
	match trie with
		Node(Branch(childs), Some(total, tags)) ->
			let total = float total in
			let probs = T.map (fun _ -> 0.0) tags
			in
			if debug then begin
				print_string "word:" ;print_string word; print_char word.[start]; print_newline()
			end;
			let probs = aux trie (0, T.empty) start probs in
			(* bayes inversion *)
			let max = ref neg_infinity in
			let probs = P.mapi (fun tag prob -> 
									let tagfreq = try T.find tag tags with Not_found  -> 0 in
									let prob = prob /. (float tagfreq /. total) in
									if prob > !max then max := prob;
									prob
								) probs in
			(* beam *)
			let max = log !max in
			let tagprobs = P.fold  (fun tag prob acc -> (tag, log prob)::acc)  probs [] in
			List.filter (fun (_, prob) -> prob > max -. log 10000.) tagprobs
			
		| _ -> failwith("furcsa trie")

		let print trie =
			let print_freq_info (count, tags) =
				T.iter (fun  tag count -> print_char ' ' ; print_int count; print_char ' '; print_string tag) tags;
				print_newline ()
			in

			let print_freq_info_option freq_info =
				match freq_info with
					None -> print_endline "none";
					|Some(x) -> print_freq_info x
			in
			let rec aux c node level =
				for i = 0 to level do
					print_char ' ';
				done;
				print_char c; print_char ' ';
				match node with
				 | Empty -> print_endline "empty";
				 |	Node(Terminal ,freq_info) -> print_string "term "; print_freq_info_option freq_info; print_newline() ;
				 |  Node(OneChild(c, child), freq_info) -> print_string "one "; print_freq_info_option freq_info; print_newline (); aux c child (level+1)
				 | Node(Branch(childs), freq_info) -> print_string "branch "; print_freq_info_option freq_info; print_newline (); C.iter (fun c child -> aux c child (level+1)) childs
			in
			aux '$' trie  0


		let print_stat trie =

				let print_freq_info_option freq_info =
					match freq_info with
						None -> Printf.printf "00\n";
						|Some(count,tags) -> let s = T.fold (fun t f acc -> acc + 1) tags 0  in Printf.printf "%d\n" s;
				in
				let rec aux c node level =

					match node with
					 | Empty -> print_endline "empty";
					 |	Node(Terminal ,freq_info) -> print_endline "term "; print_freq_info_option freq_info; print_newline() ;
					 |  Node(OneChild(c, child), freq_info) -> print_endline "one "; print_freq_info_option freq_info; print_newline (); aux c child (level+1)
					 | Node(Branch(childs), freq_info) -> print_endline "branch "; print_freq_info_option freq_info; print_newline (); C.iter (fun c child -> aux c child (level+1)) childs
				in
				aux '$' trie  0
		
*)