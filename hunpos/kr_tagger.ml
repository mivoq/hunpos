type heur = All | ShortestLemma | LongestLemma

type inlex = Known | Oov | Guessed

(*  ez ugyanaz, mint a Hmm_tagger.seen_type, csak hozzaadtuk Na,
	ha tolunk itt nem kertek taggelest
*)
type seen_type = Na | Seen | LowerCasedSeen | SpecialToken | UnSeen

let rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> []
  | (a1::l1, a2::l2, a3::l3) -> let r = f a1 a2 a3 in r :: map3 f l1 l2 l3
  | (_, _, _) -> invalid_arg "map3"


let filter heur anals = 
	
	(* rendezi az elemzeseket a lemmak hossza szerint
		Ha up == true akkor novekvo, egyebkent csokkeno sorrendbe
		a lemmak mar komponensek bontott string list-ek it *)
	let best_anal shortest anals =
		(* string list kiteritett karakterhosszat adja meg *)
		let component_len (lemma, _, _, _) = String.length lemma in
		let anal_len anal = List.fold_left (fun n comp -> n + component_len comp) 0 anal in
		let comp = if shortest then (<) else (>) in
		let min = if shortest then max_int else min_int in
		let (len, best_anal) = List.fold_left 
			(fun (max, best_anal) anal -> let l = anal_len anal in
				 						  if comp l   max then (l, anal) 
										  else (max, best_anal) 
			) (min , [])  anals in
		(best_anal)
	in

	match heur with
		| All -> anals
		| ShortestLemma -> (best_anal true anals) :: []
		| LongestLemma -> (best_anal false anals) :: []
			

;;

let serialize_component anal =
	let (lemma, deriv, pos, inf) = anal in
	lemma ^ "|" ^ deriv ^ "|" ^ inf
;;

let serialize_analysis anal  =
	String.concat "@" (List.map serialize_component anal)
	

let serialize_analyses annot =
	String.concat "\t" (List.map serialize_analysis annot)
	


(* Nahat ez itt a lenyeg. Kivalasztja a megfelelo elemzest *)
let disambig lowercase decompounding guessing_on oov_filter known_filter hunmorph word tag  =
	
	let (anals, guessed) =  hunmorph word in
	let normalized = if lowercase then (String.lowercase word) else word in
	if guessed && (not guessing_on) then
		(* ha guessed, de nem kertek guessinget, akkor a szo maga a lemma *)
		(Oov, ((normalized, "", "UNKNOWN", "UNKNOWN") ::[]) :: [])
	else
	(* kivalasztjuk a taggelesnek megfelelo elemzest *)
	let anals = match tag with
		None -> anals
		| Some(tag) -> (* csak azokat az elemzeseket hasznaljuk, 
			              amik illeszkednek a tagre *)
			let fanals = List.filter (fun anal -> let (_, _, _, inf) = List.hd (List.rev anal) in
												  inf = tag
									 ) anals in
			if List.length fanals = 0 then begin
			
				(* most mi legyen? a tagger olyan kimenetet adott, 
				   ami nincs benne a hunmorph kimeneteben *)
				anals
			end else
				fanals
	in
	
	let anals =  
		match guessed with
			true -> oov_filter anals
		  | false -> known_filter anals
	in
	
	(* meg tokolunk az osszetettszavakkal kicsit: ha nem kertek decompoundingot,
	   akkor osszerakjuk a lemmakat egybe es az utolso elemzest vesszuk *)
	let anals = 
		if not decompounding then begin
			let join_components anal =
				let rec aux acc components = match components with
					| [] -> failwith ("null analysis: can't join the components")
	                | (lemma, deriv, pos, inf) :: [] -> (* az utolso komponens elemzeseit hasznaljuk *)
	                                                    ((acc ^ lemma), deriv, pos, inf) ::[]
					| (lemma, deriv, pos, inf) :: t -> aux (acc ^ lemma ) t
				in
				aux "" anal
			in
			List.map join_components anals
		end else
			anals
	in
	(* az utolso funkcionk: kisbetusites *)
	let anals = 
		if lowercase then
			let component2lowercase (lemma, deriv, pos, inf) =
				(String.lowercase lemma, deriv, pos, inf) in
			let anal2lowercase anal = List.map component2lowercase anal in	
			List.map anal2lowercase anals
		else anals	
	in 
	let inlex = if guessed then Guessed else Known in
	(inlex, anals)
;;


let disambiguator tagger hunmorph lowercase decompounding guessing_on known_heur oov_heur  =
	(* nehany beagyazott fuggveny *)
	
	(* parcialis fuggveny word tag part kap *)
	let disambiguator = disambig lowercase decompounding guessing_on 
		                         (filter known_heur) (filter oov_heur)  hunmorph 
	in
	(* ezt hivjuk minden szora, amihez nem kertek taggelest *)
	let disambig_without_tagging word =
		let (inlex, anals) = disambiguator word None in
		(inlex, Na, anals)
	in
	
	
	
	(* ezt hivjuk minden mondatra(!), amihez kertek taggelest *)
	let disambig_with_tagging tagger sentence =
		let obs, tags = tagger sentence in
		(* kiszedi obs-bol a tipusat, hogy viszonyulunk a train korpuszhoz *)
		
		let type2type = function
			Hmm_tagger.Seen -> Seen
			| Hmm_tagger.LowerCasedSeen -> LowerCasedSeen
			| Hmm_tagger.SpecialToken -> SpecialToken
			| Hmm_tagger.UnSeen -> UnSeen
		in
		let disambig_tagged word obs tag =
			let (inlex, anals) = disambiguator word (Some tag) in
			(inlex, (type2type obs.Hmm_tagger.seen), anals)
		in
		map3 disambig_tagged sentence obs tags
	in
	(* es itt a fuggveny, amit visszaadunk; mostmar nincs sok dolga *)
	let worker sentence =
		(* kenyelmesebb ez igy *)
		match tagger with
			None         -> List.map disambig_without_tagging sentence
		  | Some(tagger) -> (* ha van tagger *)
	 						disambig_with_tagging tagger sentence
	in
	worker
;;
		

let inlex2str = function
	Known -> "K"
	| Oov -> "O"
	| Guessed -> "G"

let intrain2str = function
	Seen -> "S"
	| LowerCasedSeen -> "UL"
	| SpecialToken -> "UD"
	| UnSeen -> "U"
	| Na -> "0"
	
let process_sentence disambig sentence =
    let annotations = disambig sentence in
	let print_annotated word annotation =
		let (inlex, intrain, anals) = annotation in
		print_string word; 
		print_char '\t'; 
		print_string (serialize_analyses anals); 
		print_char '\t';
		print_string (inlex2str inlex);
		print_char '\t';
		print_string (intrain2str intrain);
		print_newline ()
	in
	List.iter2 print_annotated (List.rev sentence) (List.rev annotations);
	print_newline ()
;;




let usage () = 
	Printf.eprintf "usage : %s  modelfile kr-morphtable\n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in	exit 1 
else

let tagorder = 2 in
let emorder = 2 in
let	lowercase = false in
let decompounding = false in
let guessing_on = true in
let known_heur = ShortestLemma in
let oov_heur = LongestLemma in

	
let morphtable = Kr_morphtable.load Sys.argv.(2) in
let morphtable_tagger = Kr_morphtable.tags morphtable in
let morphtable_analyzer = Kr_morphtable.analyze morphtable in
	
let model = Hmm_tagger.load Sys.argv.(1) in
prerr_endline "model loadad";
let tagger = Hmm_tagger.compile_tagger  model morphtable_tagger tagorder emorder in
prerr_endline "tagger compiled";
let tagger = Some tagger in
let disambiguator =  disambiguator tagger morphtable_analyzer lowercase decompounding guessing_on known_heur oov_heur
in
let ic =  stdin in
Io.iter_sentence ic (process_sentence   disambiguator ) 


(* 
	let ends_with suf s =
		if String.length suf > String.length s then false
		else
		String.sub s (String.length s - String.length suf) (String.length suf)  = suf	
	
*)