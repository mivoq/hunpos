(* post-hoc filter for ocamorph
   compile: ocamlc -o ocafilter ocafilter.ml 
*)



(* Split a string into a list of substrings based on a delimiter character *)
let split c str = 
  let rec aux s acc = 
    try  let ind=String.index s c in
         aux (String.sub s (ind+1) ((String.length s) - ind -1 )) 
              ((String.sub s 0 ind)::acc)       
    with Not_found -> List.rev (s::acc) 
  in aux str []

;;


(* erre szuksegunk lesz: egy listabol kiszuri az ugyanolyan elemeket. Rendezi a listat, majd
    vegigmegy az elemeken es atmasolja egy uj listaba az unikusakat *)
let unique l =
    let rec aux last output input  = match input with
        [] -> output
      | head :: tail -> if last = head then (aux last output tail ) else (aux head (head::output) tail)
    in
    match (List.sort compare l) with
        [] -> l
    |   head::tail -> aux head (head::[]) tail 

;;
	



(* This is the post-hoc filter to emulate the missing compounds=Fallback function
   of ocamorph. It there is analysis without compounding and guessing return only those.
   The second rule may be redundant if you call ocamorph with Guessing=Fallback option:
   include guessed analysis only there is no other.

  Visszaadja, hogy ismeretlen volt-e a szo, azaz a guessing mukodott-e.
*)
let block_anals anals = 
	let isnotguessed  a = not (String.contains a '?') in
	let isbasic a = not (String.contains a '+' || String.contains a '?') in
	if List.exists isbasic anals  then (false, List.filter isbasic anals)  else
	if List.exists isnotguessed anals   then (false, List.filter isnotguessed anals)  else (true, anals)
	
;;	

(** Egy elemzesrol eltavolit minden derivaciot es inflexiot. Azaz az elso
	/ vagy ? jel utani reszt es szetszedi az osszetettszavaknal. 
	Feltetelezi, hogy a lemma utan ? vagy / van.
	*)
let parse_anal anal =

	let strip_annot a = 
		let ix = try String.index a '?' with  Not_found -> try String.index a '/' with Not_found -> -1 in
		if(ix > 0) then
			String.sub a 0 ix
		else
		a
	in
	(List.map strip_annot (split '+' anal))
;;



let stem (hanal, lowercase, decompounding, guessing_on, oov_filter, known_filter) word =
	let (_, anals) = hanal (String.copy word) in

        (* ez a segmentation miatt kell, de nem tudom, h micsoda *)
	let anals = List.rev_map (Tag.print) anals in		

	(* ocamorph neha duplumokat ad vissza*)
	let anals = unique anals in 
	
	let anals = if lowercase then (List.map (String.lowercase) anals) else anals in
	let (guessed, blocked_anals) = block_anals anals in

	let lemmas =
        if (List.length blocked_anals = 0) || guessed && (not guessing_on) then 
			(* ha guessed, de nem kertek guessinget, akkor a szo maga a lemma *)
			let normalized = if lowercase then (String.lowercase word) else word in
			(normalized::[]) :: []
		else let lemmas = List.rev_map (parse_anal) blocked_anals in
		
		 match guessed with
			true -> oov_filter lemmas
		  | false -> known_filter lemmas
	in
	if decompounding then
		(* az osszetettszo komponenseket kulon stemnek vesszuk *)
		unique (List.flatten  lemmas)
	else
		(* osszetettszavakat osszerakjuk *)
		unique (List.rev_map (String.concat "") lemmas)
;;
		

type heur = All | ShortestLemma | LongestLemma
	
let filter heur (lemmas: string list list) = 
	
	(* rendezi a lemmakat hosszuk szerint. Ha up == true akkor novekvo, egyebkent csokkeno sorrendbe
		a lemmak mar komponensek bontott string list-ek it *)
	let sort_lemmas up (lemmas: string list list) =
		(* string list kiteritett karakterhosszat adja meg *)
		let rec str_len n slits = match slits with
			[] -> n
			| h :: [] -> (n + String.length h)
			| h :: t -> str_len (n + String.length h) t
		in
		let comp l1 l2 =
		    let (len1, len2) = ((str_len 0 l1) , (str_len 0 l2) ) in
			if up then compare len1 len2 else compare len2 len1
			in
		List.sort comp lemmas
	in

	match heur with
		| All -> lemmas
		| ShortestLemma -> (List.hd (sort_lemmas true lemmas)) :: []
		| LongestLemma -> (List.hd (sort_lemmas false lemmas)) :: []
			

;;

(* no_caps 
	stop_at_first    0 1 for indexing
// blocking         0 1
// compounds        0 1
// guess            0 1 2 *)


let stemmer bin_accent bin_noaccent =
   (* ket ocamorphot epitunk, egyik az ekezetes, masik az ekezetmentes *)
   let hanal_accent =
          match (Analysis.make_marshal bin_accent false  false true true Analysis.NoGuess) with
			Analysis.Analyzer (f)-> f
  			|_ -> failwith ("Mi vaan?")	
   in
   (* ekezetmenteseket nem bontunk osszetettszora *)
   let hanal_noaccent =
		match Analysis.make_marshal bin_accent false false true false Analysis.NoGuess with
			Analysis.Analyzer (f)-> f
  			|_ -> failwith ("Mi vaan?")	
   in
   (* most osszerakunk egy olyan ocamorphot, ami egykent viselkedik *)
   let hanal w =
		let (i, anals) = hanal_accent w in
		if i > 0 then (i, anals) else
		(* ha ekezetesen nem ismertuk, akkor ekezetmentesen probaljuk *)
		hanal_noaccent w
	in
	(* (hanal, lowercase, decompounding, oov_filter, known_filter) *)
	let stem = stem (hanal, true, false, false, (filter LongestLemma), (filter LongestLemma)) in
	stem
	