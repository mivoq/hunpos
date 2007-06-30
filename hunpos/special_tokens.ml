(* TODO. THIS IS HARD WIRED!!!! PUT IT IN A SIMPLE CONFIG FILE *)

let patterns = [("@CARD"        , "^[0-9]*$"        ); 
	           ("@CARDPUNCT"  , "^[0-9]+\\.$"      );
			   ("@CARDSEPS"    , "^[0-9\\.,:-]+[0-9]+$");
			   ("@CARDSUFFIX" , "^[0-9]+[a-zA-Z][a-zA-Z]?[a-zA-Z]?$"  );
			   ("@HTMLENTITY" , "^&[^;]+;?$"  );
			  ]
;;			
let patterns = List.map (fun (name, pattern) -> (name, Str.regexp pattern)) patterns;;
	
let matching r s  =
	try
	let ix = Str.search_forward r s 0 in
	true
	with Not_found -> false
;;
	
	
let to_lex w = 
	let rec aux patterns = match patterns with
		| [] -> (false, w)
		| (name,pattern)::tail -> if matching pattern w then (true, name)
								 else aux tail
	in
	aux patterns

(*
let _ = 
    let _, n = to_lex "&bull;" in
	print_string (n)
	
*)