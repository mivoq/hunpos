let empty  = ["<s>";"<s>";"<s>"]

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