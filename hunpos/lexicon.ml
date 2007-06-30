(* Here we store the possible tags of each word of the training corpus.
   It is basically an on-the-fly-built morphtable.
 
   A single word type cannot have too many possible tags, so we store these in a simple list.
*)

(*
module S = sig
	(* TODO *)
end
*)
module Make (M : Mfhash.S ) = struct
	
type 'a t = (('a * int) list) M.t

let empty () = M.empty ()
	
(* We add element k to list l, if not already in it.
   A trick: the operation moves this k element to the head of the list if k was already present.
   The supposed advantage is that the looked-up word will be often the head or near the head.
 *)
let add_to_list l k =
	(* just an optimization: we look up the first element, and if it is a match, then we don't have to rearrange anything.*)
	match l with
		(h, freq)::t when h = k -> (h, succ freq) :: t
		| _ ->
			
	let rec aux l = match l with
		[] -> (k, 0, [])
	   | (h, freq) :: t when h = k -> (h, freq, t)
	   | h::t -> (* this is not tail-recursive *)
	 			 let (k, freq, t) = aux t in
				 (k, freq, h::t)
	in
	let (k, freq, l) = aux l in
	(k, succ freq) :: l
		
let add_word_tag lex word tag =
	let _ = M.update 
	(fun () -> (tag, 1) :: [])
	(fun tags -> add_to_list tags tag)
	lex word 
	in ()
	
let iter f lex =
	let aux word tags =
		(* we calculate the number of occurrences of the word *)
		let wfreq = List.fold_left (fun  wfreq (tag, tfreq)  -> wfreq + tfreq) 0 tags in
		f word wfreq tags
	in
	M.iter aux lex
	
let find_nofreq lex w =
	let l = M.find lex w in
	List.map (fun (tag, freq) -> tag) l
	
end
