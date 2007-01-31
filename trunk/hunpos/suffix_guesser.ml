module C = Mfhash.Char
module T = Mfhash.String

let n = 10 ;;

type t = {mutable freq:  float;
		  tags : float T.t;
		  mutable childs       :   (t C.t) option}
	
let empty () = {freq = 0.0 ; tags = T.empty (); childs = None}
	
					
let add_word trie word tag freq =
	let freq = float freq in
	let incr_tag node tag = 
		(!node).freq <- (!node).freq +. freq;
	
        let _ = T.update (!node).tags tag (fun () -> (1.0)) ((+.) (1.0)) in ()
	in
	
	let step_down node c =
		let childs = match (!node).childs with
			None -> let n = C.empty() in
					(!node).childs <- Some(n) ;
					n
		   | Some(x) -> x in
		C.find_or_add childs c (empty )
	in
	
	
	let node = ref trie in
	(* jegyeznunk kell a 0 karakter levagast is *)
	incr_tag node tag ;
	
	
	(* hatulrol vegigmegyunk a szon, de max n karakterig *)
	let stop = (String.length word - 1) in
	let start = max 0 (stop - n) in
	for n = stop downto start do
		node := step_down node word.[n];
		incr_tag node tag
	done;
;;

let toprob trie theta =
	let do_tag tag ptag parent_node =
		let rec do_childs p_parent parent_node =
			(* minden gyereken vegigmegyunk es atadjuk neki
			   theta * P(tag| .)
			*)
			let p' = theta *. p_parent in
			let adjust_child child =
				(* van-e a gyereknek ilyen tagje? *)
				T.update child.tags tag (fun () -> raise Not_found) 
										(fun f -> (f /. child.freq +. p') /. (1. +. theta))
			in
			let scale child =
				(* van-e a gyereknek ilyen tagje? *)
				let _ = 
				T.update child.tags tag (fun () -> raise Not_found) 
										(fun f -> (log (f /. ptag) )) in () 
			in
			
			match parent_node.childs with
				| None -> () (* itt az alja *)
				| Some(c) -> 
					C.iter (fun c child -> try let p = adjust_child child in
										   do_childs p child;
										   (* scale with tag prob *)
										   scale child
										   with Not_found -> () ) c
		in
		do_childs ptag parent_node
	in				   
	match trie.childs with
		None -> ()
		| Some(childs) ->
            T.iter (fun tag freq -> C.iter (fun c node -> do_tag tag (freq /. trie.freq)  node) childs) trie.tags 

let probs trie word =
	let start = (String.length word - 1) in
	let stop = max (start - n) 0 in

	let rec aux ix node   =
		
		let c = word.[ix] in
		try
		(* ha van gyereke es ix >= stop, akkor ott probaljuk meg *)
		match node.childs with
	            | Some(childs) when ix >= stop -> aux (pred ix) (C.find childs c )
				| _ -> raise Not_found
		with Not_found -> T.fold (fun t p l -> (t,p)::l) [] node.tags
	in
	aux start trie


let theta trie =
	let pow i = i *. i in
		
	let s = ref 0 in
	T.iter (fun tag freq -> incr s) trie.tags;
	let s = float (!s) in
	let p_av = 1.0 /. s in   (*-- use uniform distribution over tag-probs *)
	let theta = ref 0.0 in
	T.iter (fun tag freq -> let tagp =  freq /. trie.freq in
			theta := !theta +. pow (tagp -. p_av)) trie.tags;
	!theta /. (s -. 1.)
(*	
let _ =

	let trie = empty () in
	
	let add_sentence (words, tags) =
		List.iter2 (fun w t -> add_word trie w t 1) words tags;
	in
	prerr_endline "abulinding suffix trie";
	Io.iter_sentence stdin add_sentence;
	prerr_endline "estimating theta";
	let t = theta trie in
	toprob trie t;
	prerr_endline "calculating probs";
	let probs = probs trie "prücsköltem" in
    List.iter (fun (tag, prob) -> Printf.printf "%s %f\n" tag prob) probs
*)
	