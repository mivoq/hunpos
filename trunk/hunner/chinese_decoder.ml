module State = struct
	type t = (Ngram.t * bool * string)
	let compare (ngram1, _, _) (ngram2, _, _)= Ngram.compare ngram1 ngram2
		
	let tag (ngram, _, _) = List.hd ngram
		
	let prev_tags (ngram, _, _) = ngram 
		
	let intag (_, b, _) = b
	
	let in_which_tag (_, _, t) = t
		
	let start n = (Ngram.empty n, false, "")
	
end ;;

module StateViterbi = Viterbi.Make(State)


(* Split a string into a list of substrings based on a delimiter character *)
let split c str = 
  let rec aux s acc = 
    try  let ind=String.index s c in
         aux (String.sub s (ind+1) ((String.length s) - ind -1 )) 
              ((String.sub s 0 ind)::acc)       
    with Not_found -> List.rev (s::acc) 
  in aux str []
;;


(* betolti az ngram modellt *)
let load_model ()=  ()


let chan = stdin

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

(* egy sort szetvag: word, gold, (tag, prob)* formatumot var *)
let split_line line =
	let fields = split '\t' line in
	match fields with 
		| word:: gold:: probs -> 
			(* a string valoszinuseget float-ta alakitja *)
            let of_string (tag, prob) = (tag, (log(float_of_string prob))) in
            (word, gold, List.map of_string (make_pairs probs)) 
		| _-> failwith("parsing error: " ^ line)
;;

(* ez kell a viterbinek: egy megfigyeleshez visszaad egy fuggvenyt, amit minden from_state-bol megmondja,
	hogy milyen allapotokba, milyen sullyal lehet eljutni.

	
 *)
let trans model (obs:(string*float) list) state_from =

(*	Printf.printf "from: ";
	Ngram.print (State.prev_tags state_from);

	Printf.printf "elotte: \n";
		List.iter (fun (tag, prob) -> Printf.printf " (%s %f)" tag prob) obs;
		Printf.printf "\n";
*)		 

	let intag = State.intag state_from  in
	(* maxent kimenetet leszuri az elozo tag szerint *)
	let obs = 
		if intag then begin
			(* amiben vagyunk csak annak a MID-je es END-je johet *)
			let intag = State.in_which_tag state_from in
			let midpat = intag ^ "_MID" in
			let endpat = intag ^ "_END" in
(*			Printf.printf "intag: %s\n" intag ;
*)			let obs = List.filter (fun (tag, prob) ->  (tag = midpat||tag = endpat)) obs in
(*				Printf.printf "utana: \n";
					List.iter (fun (tag, prob) -> Printf.printf " (%s %f)" tag prob) obs;
					Printf.printf "\n";
*)			obs
		end
		else
			obs
	in	




	(* milyen cimkei johetnek? benne van az observation-ban. Ha AB allapotban voltunk es
	   C tagje lehet, akkor BC egy koveto allapot 
	*)
		let next_state tag =
			let (intag, intag_tag) = 
			match tag with
				"MISC_START" | "LOC_START" | "ORG_START" | "PERS_START"
			|    "MISC_MID" | "LOC_MID" | "ORG_MID" | "PERS_MID" -> (true, List.hd (split '_' tag))
			| _ ->(false, "")
			in
			( Ngram.shift_right (State.prev_tags state_from) tag, intag, intag_tag)
		in
	
        List.map (fun (tag, maxentprob) -> ((next_state tag), maxentprob)) obs
		
;;

let tag model obs_vector =
    let state_seq = StateViterbi.decode (State.start 2)  obs_vector  (trans model) in
	List.map (fun state -> State.tag (state)) state_seq
;;
	
	
let tag_sentence model sentence =
(*
	let print (word, gold, probs) =
		Printf.printf "%s %s ->" word gold;
		List.iter (fun (tag, prob) -> Printf.printf " (%s %f)" tag prob) probs;
		Printf.printf "\n"
	in

	List.iter (fun l ->  print (split_line l)) ( sentence);
*)		
	let get_probs line = 
		match split_line line with
			word, gold, probs -> probs
			| _ -> failwith "furcsa, nagyon furcsa"
	in

	let obs_vector = List.map (get_probs) sentence in
    let tags = tag model obs_vector in
	(* ha mar itt vagyunk, kiszedjuk a maxent legjobb tagjet is *)
	let c (t1, p1) (t2, p2) =  if p1 < p2 then 1 
							  else if p1= p2 then 0 
							  else -1 in
	let winner probs = 
		let (mtag, mprob) = List.hd (List.sort c probs) in
		mtag
	in
	let mtags = List.map (winner) obs_vector in
	List.combine tags mtags

	
 	
	

	

let total = ref 0

let falses = ref 0
	
	
let do_sentence model sentence =
	let tags = (tag_sentence model sentence ) in
	let result = List.combine tags sentence in
	List.iter (fun ((vtag, mtag),b) -> Printf.printf "%s\t%s\t%s\n" vtag mtag b) result;
	Printf.printf "\n"
;;
	
let _ =
	let model = load_model () in
    Io.iter_sentence_no_split chan (do_sentence model);
