

type tag_type = Start | End | Mid | Lone

type state = None | Tag of (string * tag_type)
type observation = (string * (state * float) list)

let start_state = None

let to_str state = match state with
	None -> "0" 
  | Tag(name, typ) -> let typs =
							match typ with
								Start -> "START"
								| Mid -> "MID"
								| End -> "END"
								| Lone -> "LONE"
							in
						name ^ "_" ^ typs
;;
		
let next obs =
	
	let tags = snd obs in
		
	let cont state tag = match state, tag with
		| None, None -> true
		| None, Tag(_, Start) -> true
		| None, Tag(_, Lone) -> true
		| Tag(_, Lone), None -> true
		| Tag(tag1, Start), Tag(tag2, Mid) -> tag1 = tag2
		| Tag(tag1, Start), Tag(tag2, End) -> tag1 = tag2
		| Tag(tag1, Mid), Tag(tag2, Mid) -> tag1 = tag2
		| Tag(tag1, Mid), Tag(tag2, End)  -> tag1 = tag2
		| Tag(_, End), None -> true
		| Tag(_, End), Tag(_, Start) -> true
		| Tag(_, Lone), Tag(_, Start) -> true
		| Tag(_, End), Tag(_, Lone) -> true
		| _, _ -> false
		(*
		| (Tag (_, End), Tag (_, (Mid|End))) -> false
		| (Tag (_, Mid), None) -> false
		| (Tag (_, Mid), Tag (_, Start)) -> false
		| (Tag (_, Start), None) -> false
		| (Tag (_, Start), Tag (_, Start)) -> false
	*)
	in
		
		
	let transition from =
        (* List.filter (fun (tag, probs) -> cont from tag) tags *)
    	    let l = List.filter (fun (tag, probs) -> cont from tag) tags in
		l
	in
	
	let emission state = 0.0
		
	in
	
	(transition, emission)
;;



(* paros hosszu listabol par tuple listat csinal, azaz
	a,b,c,d,e,f -> (a*b), (c*d), (e*f) lista 
	
	paratlan elemnel az utolsot egyszeruen elhagyja, mert Daniel olyan test fajlt adott, ami pont tabra
	vegzodott
*)
let make_pairs l =
	let rec aux l res = 
		match l with
		| [] -> res
		| a::b::tail -> (a,b) :: (aux tail res)
		| _ -> res
	in
	aux l []
;;

let tag_of_string tag = match Parse.split '_' tag with
	| _ :: [] -> None
	| name :: typ :: [] -> begin
		 match typ with
		    "START" -> Tag(name, Start)
		   |"MID" 	-> Tag(name, Mid)
		   |"END"	-> Tag(name, End)
		   |"LONE"	-> Tag(name, Lone)
		   | _ -> failwith ("not valid tag position type: " ^ tag)
		end
	| _ -> failwith ("too much _ " ^ tag)
;;
 
(* egy sort szetvag: word, gold, (tag, prob)* formatumot var *)
let split_line line =
	let fields = Parse.split '\t' line in
	match fields with 
		| word:: probs -> 
			(* a string valoszinuseget float-ta alakitja *)
            let of_string (tag, prob) = (tag_of_string tag, (log(float_of_string prob))) in
            (word, List.map of_string (make_pairs probs)) 
		| _-> failwith("parsing error: " ^ line)
;;

let sent_to_obs lines =
	List.map split_line lines
;;

module SMap = Ocamap.Make(struct 
							type t = state 
							let compare s1 s2= match s1, s2 with
								| None, None -> 0
								| Tag(s1, t1), Tag(s2, t2) when s1 = s2 ->
									begin
										match t1, t2 with 
											  Start, Start -> 0
											| Mid, Mid -> 0
											| End, End -> 0
											| Start, End -> 1
											| Start, Mid -> 1
											| Mid, End -> 1
											| Lone, Lone -> 0
											| _, Lone -> -1
											| Lone, _ -> 1
											| _ -> -1
									end
								| Tag(s1, t1), Tag(s2, t2) -> compare s1 s2
								| Tag(_, _), None -> -1
								| _ -> 1
						 end)
module Viterbi = Viterbi.Make(SMap)
let _ =
	
	let sentence sent =
		let sent = List.rev sent in
		let obs = sent_to_obs sent in
		let tags = Viterbi.decode next start_state (obs) in
		List.iter2 (fun tag (word, tags) -> print_string word; print_char '\t'; print_endline (to_str tag)) tags obs;
		print_newline ();
	in
	Io.iter_sentence stdin (sentence);
	(*let sent = ref [] in
	try 
	while(true) do
		sent := (input_line stdin) :: !sent
	done 
	with End_of_file -> sentence (List.rev !sent)
*)