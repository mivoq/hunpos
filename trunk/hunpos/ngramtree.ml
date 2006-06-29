module Cmap = Map.Make(struct type t = string let compare = compare end)
	
(* Az ngram fa egy trie az ngrammokon, ami tetszoleges mely lehet.
   [A; B; C] ngrambol A gyereke lesz B annak gyereke C. Minden csomopont
   tartalmazza a gyakorisagat a resz ngramnak, B csomopont [A;B] bigram
   gyakorisagat adja meg.

   A csomopont gyerekei most meg egy balanced binary tree-ben vannak.
 	*)

type tree =  Node of int * tree Cmap.t 

(* function to create an empty tree *)	
let empty = Node ( 0, Cmap.empty)

	(*s Insertion of a new ngram in the tree is just a matter of descending in
	    the tree, taking the branches corresponding to the successive gram.
	    Each time a branch does not exist, we continue the insertion in a new
	    empty tree. When the insertion is done in the subtree, we update the
	    branching to the new subtree. *)

	let rec add  tree ngram = match (tree, ngram) with 
			| ((Node (freq, childs) as n) , h::t) ->
				(* sima eset, megyunk lefele, es ezt noveljuk eggyel *)
	               let child = try Cmap.find h childs with Not_found -> empty in
			          let t' = add child t in
						  Node ((succ freq), Cmap.add h t' childs)
			| ((Node (freq, childs) as n) , []) ->
				Node ((succ freq), childs)
		
	
	let print t =
		let rec print_tree level str (Node (f, m) as t) =
				for i = 1 to level do
					print_char '\t'
				done;
				Printf.printf "%s\t%d\n" str f;
			    Cmap.iter (fun k v -> print_tree (succ level) k v) m
		in
		print_tree 0 "root" t
				
	(* jumps to a node representing the prefix.
		raises Not_found exception if the prefix not found in the trie 
	let look_for_node t w =
		let n = String.length w in
		let rec look i (Node (b, f, m) as t) =
			if i = n then
				t
			else
				look (succ i) (Cmap.find w.[i] m)  
		in
		look 0 t
*)