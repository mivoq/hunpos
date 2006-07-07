type t = string list 

let rec compare ngram1 ngram2 =
	match (ngram1, ngram2) with
		  ([] , []) -> 0	(* ha mindketto ures, akkor egyenloek *)
		| (t::_ , [] ) -> 1
		| ([], t::_)  -> -1
		| (t1::h1, t2::h2) -> let c = String.compare t1 t2 in if c != 0 then c else compare h1 h2
	
	
	
let rec empty l = match l with
	0 -> []
	| l -> "<s>" :: empty (pred l)
	
let print ngram =
  let s = String.concat " " ngram in
  print_string s;
  print_newline();
;;

let shift_left ngram ne = match ngram with 
	| []  -> ne :: []
 	| head::tail -> (tail @ (ne :: []))

let shift_right ngram ne = match ngram with
	| []  -> ne :: []
	| _ -> ne :: List.rev ( List.tl (List.rev ngram) )
	
let rec chop_right ngram = match ngram with
	[] -> []
	| l :: [] -> []
	| l :: h -> l :: chop_right h