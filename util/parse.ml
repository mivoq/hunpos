let split c str = 
  let rec aux s acc = 
    try  let ind=String.index s c in
         aux (String.sub s (ind+1) ((String.length s) - ind -1 )) 
              ((String.sub s 0 ind)::acc)       
    with Not_found -> List.rev (s::acc) 
  in aux str []



let split2 c str =
	let rec aux acc idx =
	  try let idx' = String.index_from  str idx c in
		aux  ((String.sub str idx (idx' - idx))::acc) (succ idx')
      with Not_found -> List.rev ((String.sub str idx (String.length str - idx)) :: acc)
	in
	aux [] 0
	
