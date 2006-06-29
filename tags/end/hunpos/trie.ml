(*s The dictionary is implemented as a \emph{trie}. It is a multi-branching
    tree, where branches are labelled with characters. Each node contains
    a boolean which says if the word corresponding to the path from the root
    belongs to the dictionary. The branches are implemented as maps from
    characters to dictionaries. *)
module Cmap = Map.Make(struct type t = char let compare = compare end)
	
	type tree = Node of bool * int * tree Cmap.t
	
	let empty = Node (false, 0, Cmap.empty)

	(*s Insertion of a new word in the trie is just a matter of descending in
	    the tree, taking the branches corresponding to the successive characters.
	    Each time a branch does not exist, we continue the insertion in a new
	    empty tree. When the insertion is done in the subtree, we update the
	    branching to the new subtree. *)

	let add t w =
	  let n = String.length w in
	  
	  let rec addrec i (Node (b,f, m) as t) =
	    if i = n then  
	       Node (true, (succ f), m)
	    else
	      let c = w.[i] in
	
	      let br = try Cmap.find c m with Not_found -> empty in
	      let t' = addrec (succ i) br in
	      Node (b, (succ f), Cmap.add c t' m)
	  in
	  addrec 0 t 
	
	(*s Even if it is not necessary, here is the function [mem] to tests
	    whether a word belongs or not to the dictionary. *)

	let mem t w =
	  let n = String.length w in
	  let rec look i (Node (b, f, m)) =
	    if i = n then
	      b
	    else
	      try look (succ i) (Cmap.find w.[i] m) with Not_found -> false
	  in
	  look 0 t
	
	(* jumps to a node representing the prefix.
		raises Not_found exception if the prefix not found in the trie *)
	let look_for_node t w =
		let n = String.length w in
		let rec look i (Node (b, f, m) as t) =
			if i = n then
				t
			else
				look (succ i) (Cmap.find w.[i] m)  
		in
		look 0 t
			
	let rec print_prefix = function
		  | [] -> ()
		  | c::l -> print_prefix l; print_char c		
	
		(*s The following function [print_all] prints all the words of a given 
		    dictionary. Only used for checks (option \texttt{-a}). *)

		
	let print_all d =
		  let rec traverse pref (Node (b,f, m)) = 
		    if b then begin print_prefix pref; print_char ' '; print_int f; print_newline () end;
		    Cmap.iter (fun c t -> traverse (c::pref) t) m
		  in
		  traverse [] d
		