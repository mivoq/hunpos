module type S = sig
	
    type 'a t
	type gram
	val empty : unit -> int t
	val add :  int t -> gram list -> int -> unit
	val iter : (gram list -> 'a -> unit) ->  'a t -> unit
	val calculate_lambdas :  int t ->  int t -> int -> float array
(*	val probtree nt dt lambdas =
*)
end
module Make (M : Amap.S with type key = string) = struct 

(*(S with type gram = M.key)   = struct
*)
type 'a t = {mutable freq   : 'a ;
             mutable childs :  (( 'a t) M.t) option  }

type gram = M.key 

let empty () = {freq = 0; childs = None}
	
let rec add (t: int t) ngram n =

	t.freq <- t.freq + 1;
	if n = 0 then () else
	
	match ngram with
		head :: tail -> 

			let childmap =
			match t.childs with
				| None -> let e = M.empty() in
						  t.childs <- Some(e);
						  e
				| Some(v) -> v
			in
            let child = M.find_or_add childmap head (empty) in
			add child tail (pred n)
		| [] -> ()	
;;

let freq node = node.freq
	
let add_bos t ngram n =
	(* ez most hack arra az esetre ha <s>-t adunk hozza *)
	add t ngram n;
	t.freq <- t.freq - 1;
;;

let iter  f t =
	let rec aux t acc =
		f (List.rev acc) t.freq;
		match t.childs with
			| None -> ()
            | Some(c) -> M.iter (fun gram childs -> aux childs (gram::acc)) c
	in
	aux t []
;;

let iter_level n f t  =
	let rec aux t acc  n =
		if n = 0 then
			f (List.rev acc) t.freq
		else	
		match t.childs with
			| None -> ()
            | Some(c) -> M.iter (fun gram childs -> aux childs (gram::acc) (pred n)) c
	in
	aux t [] n
;;



(* vegig megyunk a fan ugy, hogy kozben az eggyel kisebb kontextus csomoponjait hor
	dozzuk *)

	


let calculate_lambdas nt dt n  =

	let lambdas = Array.create (n+1) 0 in
		
	let adjust_lambdas nnodes dnodes level =
		let rec searchmax max maxi i nominator denominator =
			match (nominator, denominator) with 
				(nh::nt), (dh::dt) ->  
					
				  	let ratio = if nh.freq == 1 || dh.freq == 1 then (-1.0) 
							    else float (nh.freq -1) /. float (dh.freq -1)  in
					let (max, maxi) = if ratio  > max then (ratio, i) else (max, maxi) in
						searchmax max maxi (pred i) nt dt
				| (_ , _ ) -> (max, maxi)
		 in

		 let (prob,maxi) =   searchmax (-1.0) 0 level  nnodes dnodes  in
		 let freq = (List.hd (nnodes)).freq in
		 lambdas.(maxi) <-lambdas.(maxi) + freq 
	in

	let for_word gram ctrie =
		begin
			(* itt a gram: t_0-k, ctrie: milyen contexttel fordult elo *)
			let rec step_down gram (nnodes) (dnodes) i =
				if i = n then adjust_lambdas (nnodes) (dnodes) i else
				begin
				let nnode = List.hd nnodes in
				let dnode = List.hd dnodes in
			
				match nnode.childs with
					None ->  adjust_lambdas (nnodes) (dnodes) i 
					| Some (x) ->
						M.iter (fun xgram xnode ->
									match dnode.childs with
										None -> failwith ("inconsistent tree")
									 | Some(y) ->
										 try 
											let ynode = M.find_save y xgram in
										 	step_down xgram (xnode::nnodes) 
												(ynode::dnodes) (succ i)
										with Not_found -> failwith ("inconsistent tree")
								) x ;
				end
			in
			step_down gram (ctrie::[]) (dt::[]) 1
		end
	in	
	let _ =	
	(* elso szintet kulon kell kezelni *)
	match nt.childs with
		Some (t0s) ->
			M.iter (for_word ) t0s
		| None -> ()
	in
	(* normalization *)
	lambdas.(0) <- 0;
	let sum = Array.fold_left (fun sum x -> sum+x) 0 lambdas in
	let lambdas = Array.map (fun x -> float x /. float sum) lambdas in
	lambdas
;;
	
	
let probtree nt dt lambdas =
	
	let empty () = {freq = 0.0; childs = None} in
	(* nt-t lemasoljuk es csinalunk egy float fat *)
	let rec clone (orig:int t) (cloned:float t) (denom:int t) lambdas p_to=
		match lambdas with
			[] -> () 
		| lambda :: tlambdas ->
		begin
		let p = lambda *. float orig.freq /. float denom.freq +. p_to  in
		cloned.freq <- log p;
			if List.length tlambdas = 0 then () else
			
		match orig.childs with
			Some (childs) ->
				begin
				
				let cloned_childs = M.empty () in
				cloned.childs <- Some(cloned_childs);
				M.iter (fun gram orig_child -> 
						
							let cloned_child = empty () in
                            M.add_or_replace cloned_childs gram cloned_child;
							match denom.childs with
								None -> failwith ("inconsistent tree")
							 | Some(y) -> 
									 let ynode = try M.find_save y gram 
									 with Not_found -> failwith ("inconsistent tree") in
								 	 clone orig_child cloned_child ynode ( tlambdas) p) childs
							
                            
				end
		   | None -> ()
		end
	in
	let (l0::lambdas) = (Array.to_list lambdas) in
	let cloned_childs = M.empty () in
	let cloned = {freq = l0; childs = Some(cloned_childs)} in
	let _ =
	match nt.childs with
		Some(childs) -> M.iter  
						(fun gram child -> 
							let cloned_child = empty () in
							M.add_or_replace cloned_childs gram cloned_child;
							clone child cloned_child dt  lambdas l0
							) childs
	 | None -> ()
	in
	cloned 
	

let rec seq_prob (trie:'a t) seq =
	match (seq, trie.childs)  with 
		| ((head :: tail), Some (childs)) -> begin
											try
												let down = M.find childs head in
												seq_prob down tail
										 		with Not_found -> (* ismeretlen context *)
										 						  trie.freq
											end
		| (_, None) -> trie.freq (* nincs nagyobb context *)
		| ([], _) -> (* kisebb context *) trie.freq
		
let wordprob  probtree context word =
	seq_prob probtree (word::context)

let move_to_child tree word =
	match tree.childs with
		Some (childs) ->  M.find childs word 
		| None -> (* empty tree *) raise Not_found
	
let edges tree =
	match tree.childs with
		Some (childs) ->  M.fold (fun k v l -> k :: l) [] childs
		| None -> (* empty tree *) []
		
let iter_childs  f tree=
	match tree.childs with
		Some (childs) -> M.iter f childs
		| None -> ()
	

end
	
module SNgramTree = Make(Mfhash.String)
(*
let _ = 
	let tt = SNgramTree.empty () in
	let ot = SNgramTree.empty () in
	let add_sentence (words, tags) =
	
		let rec aux words tags =		
			match words, tags with
			 |  (word::[], tag::[]) -> SNgramTree.add tt tags 2;
			 |	(word::word_tails),
			 	(tag::tag_tails)   -> SNgramTree.add tt tags 3;
									  SNgramTree.add ot (word::tags) 2;
						  			  aux word_tails tag_tails
			 | (_,_) -> ()
		in
		let tags = ("</s>"::tags) @ ("<s>"::[]) in
		let words  = ("</s>"::words) @ ("<S>"::[]) in
	    aux words tags;
	in
	Io.iter_sentence stdin add_sentence;
(*	SNgramTree.iter  (fun l freq -> Printf.printf "%s\t%d\n" (String.concat " " l) freq ) tt;
*)
	Printf.printf "iterated \n";
	let tlambdas = SNgramTree.calculate_lambdas tt tt 3;
	in Array.iter (Printf.printf "%f\n") tlambdas;
(*	let cloned = SNgramTree.probtree tt tt tlambdas in
	SNgramTree.iter  (fun l freq -> Printf.printf "%s\t%f\n" (String.concat " " l) freq ) cloned;
*)	
	let olambdas = SNgramTree.calculate_lambdas ot tt 2;
	in Array.iter (Printf.printf "%f\n") olambdas
	(*
	 SNgramTree.iter  (fun l freq -> Printf.printf "%s\t%d\n" (String.concat " " l) freq ) tt;
*)
*)