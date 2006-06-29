(************************************************************************
*
*  viterbi.ml
*
*
*
************************************************************************)

(* A viterbit nem erdekli a hmm, csak P(s1 | s0, o1) = P(s1 | s0) * P(o1 | s1)
   szorzat. A hmm-tol ezeket egyben kerjuk el, amit visszakapunk az egy tuple
   lista: milyen allapotokba lehet eljutni es milyen sullyal (log P)
*)

module Cmap = Map.Make(struct type t = int let compare = compare end)
	
(* start után minden csomópontról feljegyezzük az addigi súlyt és
	hogy honnan mentünk oda
	*)
type 'a node = Start |  Token of float * 'a * 'a node 


let decode observation trans_probs  = 
	
	(* egy lepes elore: a current_node-okbol hova lehet eljutni? *)
	let step_forward o current_nodes =
		let transition_fun = trans_probs o in
		
		let check_node  from_state from_node new_nodes =	
			(* vegig megyunk a lehetseges koveto allapotokon es megnezzuk jobb-e odamenni:
				itt tehat from_state, from_node parbol ellenorizzuk, h state-be transw sullyal jok vagyunk-e *)
			let check_follow_state new_nodes (state, transw )  =
				let new_weight = 
					match from_node with
						| Token(weight_dueto, cstate,  pptoken) ->weight_dueto *. transw 
						| Start -> transw
					 in
				try 
					match (Cmap.find state new_nodes) with
						Token(weight, cstate, ptoken) -> if new_weight > weight then Cmap.add state (Token(new_weight, cstate, from_node)) new_nodes else new_nodes
						| Start ->  failwith "confused"
				 with Not_found ->
					Cmap.add state ( Token(new_weight, state, from_node)) new_nodes
			in	
			(* hmm megmondja, hogy hova milyen sullyal lehet menni from_state-bol o eseten. Mindegyikre vegigprobaljuk a sulyt *)
		
			List.fold_left check_follow_state new_nodes (transition_fun from_state)  
		
	  	in
			Cmap.fold check_node current_nodes Cmap.empty;

	in
	let rec forward observation current_nodes =
		match observation with
			| [] -> current_nodes
			| h :: l -> forward l (step_forward h current_nodes )
	in
	
    let start_node = Cmap.add (-1) Start Cmap.empty in 
	let end_nodes = forward observation start_node in
		Cmap.iter (fun k (Token(w, cstate, ptoken)) -> Printf.printf "%d %f %d\n" k w cstate) end_nodes;
	let max = ref neg_infinity in 
	let competition (Token(w,cstate,ptoken) as node) competitor =
		 Printf.printf "trying  %f %d max: %f\n"  w cstate !max; 
		match competitor with
			Start ->  Printf.printf "against start\n" ; max:=w; node
            | _ ->  if w > !max then (max:=w;  Printf.printf "max = %f\n" !max; node) else (Printf.printf "competitor winner\n" ; competitor)
		in 
    let  best_node = Cmap.fold (fun k  node  competitor ->competition node competitor) end_nodes Start	in

		(* visszafejteskor egy listaba kell beraknunk*)
		let rec back states node =
			match node with
				| Start -> states
				| Token(w, state, ptoken) -> back (state :: states) ptoken
		in
		back [] best_node