(*
module type S = 
sig

	type state
	type observation
	type hmm
	val decode :  hmm ->  state ->  observation list ->  state list

end


module Make(M: Amap.S) : (S with type state =  M.key) = struct
*)

(*module M = Mfhash.Make(struct type t = string Ngram2.t 
					   let compare n1 n2 = Ngram2.compare n1 n2 2
					   let hash ngram = Ngram2.hash ngram 2 
					   let equal n1 n2 = Ngram2.equal n1 n2 2
					   end)
*)					
module Make (M : Amap.S)  = struct
	
type state = M.key
type node = {state : state ; mutable from : node option; mutable weight : float }

let logtheta = log 100.0 


let decode  hmm start_state observations =
		
	let step_forward current_nodes obs =
		let (transition_prob, emission_prob) = hmm obs in
		let next_nodes = M.empty () in 	
		let from_node node =
			let next_states = transition_prob node.state in
			List.iter (fun (to_state, w) ->
				(* check that coming from node.state to to_state is better *)
				let w = w +. node.weight in
                let _ = M.update next_nodes to_state 
						(fun () -> {state=to_state; from=Some node;weight = w}) 
						(fun old -> let _ = 
									if old.weight < w then begin
											old.weight <- w;
											old.from <- Some node
									end in 
									old
						)
				in ()
				) next_states
		in
		List.iter from_node current_nodes;
		(* now 1. add emission probs,  2.  search the max weight, 3. map it to a list, *)
		(* but: if there is only one state, we don't need to do anything *)
		
		let next_nodes = M.fold (fun state node l ->
									node::l
			 					) [] next_nodes in
		if (List.length next_nodes) = 1 then next_nodes else
			let max = ref neg_infinity in
			(* beam pruning *)
			List.iter (fun node ->
						node.weight <- node.weight +. (emission_prob node.state);
						if node.weight > !max then max:=node.weight ;
						) next_nodes ;
				

			let rec filter l acc = match l with
	             h::t -> let acc = if h.weight < (!max -. logtheta) then acc else h::acc in
						filter t acc
			 | [] -> acc
			in
			filter next_nodes []
	in
	let  start_node = {state = start_state; from = None; weight = 0.0} in
	let end_nodes = List.fold_left step_forward (start_node::[]) observations in
	
	(* search the best end_node *) 
	let end_node = match end_nodes with
		h::t -> List.fold_left (fun best n -> if best.weight < n.weight then n else best) h t
		| [] -> failwith "no stop node"	
	in
	
	(* make a list *)
	let rec back states node = 
		match node.from with
			None -> states
			| Some(from) ->back (node.state :: states) from
	in
	back [] end_node
end