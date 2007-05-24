module H = Mfhash.String

let load_stop_words file =
  let ic =  open_in file in
  let lex = H.empty () in
  begin
	try
	  while(true) do
	    let line = input_line ic in
		H.add_or_replace lex line line;
	  done
	with End_of_file -> ()
  end;
  close_in ic;
  lex

let is_ok lex word =
	(* ha van benne 0-9 *)
	let rec is_ok n =
	  if n < 0 then true
	  else match word.[n] with
		'0' | '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9' -> false
		| _ -> is_ok (n - 1)
	in
	if not (is_ok (String.length word - 1)) then false
	else
	try
		let _ = H.find lex (String.lowercase word) in
		false
	with Not_found -> true
	
