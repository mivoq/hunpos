



let split2 c str =
	let rec aux acc idx =
	  try let idx' = String.index_from  str idx c in
		aux  ((String.sub str idx (idx' - idx))::acc) (succ idx')
      with Not_found -> List.rev ((String.sub str idx (String.length str - idx)) :: acc)
	in
	aux [] 0
	

let split = split2