(** This is the good old viterbi algorith for HMM decoding,
    in functional style.

    Viterbi doesn't deal with the structure and order of HMM. 
    It only needs a start state and a black box that calculates
    following states with transition and emission probabilities.
    
    We do not have any trellis: at every step we only record the
    set of possible states with their weight and preceeding state.
    This structure is called node. The nodes which are accessible
    at step t. are stored in a map (hashtable or tree). The keys
    are states, the values are nodes. The type of states isn't
    restricted.

    The HMM from the viewpoint of viterbi is two functions. The
    first calculates the following states with transition probs
    from a current state. The second function calculates the
    emission probs.
        
*)				


module Make (M : Amap.S)  = struct
	
type state = M.key
type node = {state : state ; mutable from : node option; mutable weight : float }

(** Returns the most probable state sequence from the start_state given
    the observation sequence.
  *)
let decode  hmm logtheta start_state observations =
		
	(** returns the next states given the observation obs and the
	    previous states.
	 *)
	let step_forward current_nodes obs =
	    (* hmm is only two functions *)
		let (transition_prob, emission_prob) = hmm obs in
		
		(* the following states are stored in a map; this
		    is similar to the column of trellis *)
		let next_nodes = M.empty () in 	
	
		(* we iterate over all previous states, and refresh the map *)	
		let from_node node =
		    (* the HMM knows the set of following 
		        states from the prev state *)
			let next_states = transition_prob node.state in
			List.iter (fun (to_state, w) ->
				(* check that coming from node.state to to_state is better *)
				let w = w +. node.weight in
                let _ = M.update 
						(fun () -> {state=to_state; from=Some node;weight = w}) 
						(fun old -> let _ = 
									if old.weight < w then begin
											old.weight <- w;
											old.from <- Some node;
									end in 
									old
						)
						next_nodes to_state 
			
				in ()
				) next_states
		in
		List.iter from_node current_nodes;
		(* now 1. add emission probs,  
		       2. search for the max weight, 
		       3. convert it to a list, 
		   but: if there is only one state,
		   we don't need to do anything 
		*)
		
		let next_nodes = M.fold (fun state node l ->
									node::l
			 					) [] next_nodes in
		if (List.length next_nodes) = 1 then next_nodes else
			let max = ref neg_infinity in
			    
			(* add emission prob, and search for the max *)
			List.iter (fun node ->
						node.weight <- node.weight +. (emission_prob node.state);
						if node.weight > !max then max:=node.weight ;
						) next_nodes ;
				
			(* beam pruning *)
			let rec filter l acc = match l with
	             h::t -> let acc = if h.weight < (!max -. logtheta) then acc else h::acc in
						filter t acc
			 | [] -> acc
			in
			let next_nodes = filter next_nodes [] in
				
			next_nodes
	in
	let start_node = {state = start_state; from = None; weight = 0.0} in
	let end_nodes = List.fold_left step_forward (start_node::[]) observations in
	
	(* search for the best end_node *) 
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
