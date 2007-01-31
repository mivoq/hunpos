type 'a t = 'a list 
let gram_compare = String.compare
let gram_hash = Hashtbl.hash
		
let empty = []
	
let add word ngram = word :: ngram
	
let rec compare ngram1 ngram2 n =
	if n <= 0 then 0 else
	match (ngram1, ngram2) with
		| (t1::h1, t2::h2)  -> 
			let c = gram_compare t1 t2 in if c != 0 then c else compare h1 h2 (pred n)
		| ([] , []) -> 0	(* ha mindketto ures, akkor egyenloek *)
		| (t::_ , [] ) -> 1
		| ([], t::_)  -> -1
	
let equal ngram1 ngram2 n =
	(compare ngram1 ngram2 n) = 0
	
let hash ngram n =
	let rec aux ngram n h = 
		if n < 1 then 1 else
		match ngram with
		[] -> h
		| gram::t -> aux t (pred n) ((gram_hash gram) * 31)
	in
	aux ngram n 1 
	
let newest = function
	newest :: _ -> newest
   | _ -> failwith ("empty ngram") 