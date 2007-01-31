type ('a, 'b) t =  {mutable contents : (('a, 'b) entry list); mutable size: int} 
and ('a, 'b) entry = {key: 'a; mutable value :'b}

let empty () = {contents = []; size = 0}
	
let  find l key = 
	let rec aux l = match l with
    	[] -> raise Not_found
  		| e::l -> if compare e.key key = 0 then e.value else aux l
	in
	aux l.contents
	
let size l = l.size
	
let  add_or_update l k dval update =
	let rec aux = function
		[] -> l.contents <- {key = k; value = dval} :: l.contents  ; l.size <- l.size+ 1; dval
	   | e::l -> if compare e.key k = 0 then begin 
					(* megvan, update *)
					e.value <- update e.value;
					e.value
				 end
				 else aux l
	  in aux l.contents

let find_or_add l k dval =
		add_or_update l k dval (fun id -> id)
	
let iter f l =
	List.iter (fun e -> f e.key e.value) l.contents


let _ =
	let a = empty() in
	try while(true) do
		add_or_update a (input_line stdin) 1 (succ)
	done
	with End_of_file ->
		iter (fun w f -> Printf.printf "%s\t%d\n" w f) a