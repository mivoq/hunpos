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



module Make (OrderedState : Map.OrderedType) = struct
	module Cmap = Map.Make(OrderedState)
	type ot = int
    type st = OrderedState.t
	

(* start után minden csomópontról feljegyezzük az addigi súlyt és
	hogy honnan mentünk oda
	*)
type  node = Start |  Token of float * st * node 


let decode start_state observation trans_probs  = 
	
	(* egy lepes elore: a current_node-okbol hova lehet eljutni? *)
	let step_forward o current_nodes =
		let transition_fun = trans_probs o in
		let max_weight = ref neg_infinity in
		
	
		let check_node  from_state from_node new_nodes =	
		
			(* csak aux, amikor jobb allapotot talaltunk kovetonek *)
			let add_node_to_trellis weight pstate pnode new_nodes =
          		if weight > !max_weight then max_weight := weight ;
				Cmap.add pstate (Token(weight, pstate, pnode)) new_nodes 
			in 
			(* vegig megyunk a lehetseges koveto allapotokon es megnezzuk jobb-e odamenni:
				itt tehat from_state, from_node parbol ellenorizzuk, h state-be transw sullyal jok vagyunk-e *)
			let check_follow_state new_nodes (state, transw )  =
				let new_weight = 
					match from_node with
						| Token(weight_dueto, cstate,  pptoken) ->weight_dueto +. transw 
						| Start -> transw
					 in
				try 
					match (Cmap.find state new_nodes) with
						Token(weight, _, _) -> 
							if new_weight > weight then 
							    add_node_to_trellis new_weight state from_node new_nodes
								(* Cmap.add state (Token(new_weight, state, from_node)) new_nodes *)
							else new_nodes
						| Start ->  failwith "confused"
				 with Not_found ->
					add_node_to_trellis new_weight state from_node new_nodes
			in	
			(* hmm megmondja, hogy hova milyen sullyal lehet menni from_state-bol o eseten. Mindegyikre vegigprobaljuk a sulyt *)
		
			List.fold_left check_follow_state new_nodes (transition_fun from_state)  
		
	  	in
			let new_nodes = Cmap.fold check_node current_nodes Cmap.empty in
          (*	Printf.printf "new nodes: %d \n" (Cmap.fold (fun k d n -> n+1)   new_nodes 0);*)
			
  			Cmap.fold (fun state (Token(w, _, _))  map -> if w < (!max_weight -. (log 1000.)) then Cmap.remove state map else map) new_nodes new_nodes 
	in
	let rec forward observation current_nodes =
		match observation with
			| [] -> current_nodes
			| h :: l -> forward l (step_forward h current_nodes )
	in
	
    let start_nodes = Cmap.add (start_state) Start Cmap.empty in 
	let end_nodes = forward observation start_nodes in
		
 	(* Itt mar megvan a trellisszeru adatunk, csak ritka matrixkent tarolva.
	   Most a vegen kivalasztjuk a legjobbat, majd felfejtj-k *)
	
	
	let max = ref neg_infinity in 
	let competition node competitor =
		match node with 
			Start -> node
			| (Token(w,cstate,ptoken) as node) ->
				 if w > !max then (max:=w;  node) else (competitor)
		in 
    let  best_node = Cmap.fold (fun k  node  competitor ->competition node competitor) end_nodes Start	in

		(* visszafejteskor egy listaba kell beraknunk*)
		let rec back states node =
			match node with
				| Start -> states
				| Token(w, state, ptoken) -> back (state :: states) ptoken
		in
		back [] best_node
	
 end;;