type heur = All | ShortestLemma | LongestLemma

let ends_with suf s =
	if String.length suf > String.length s then false
	else
	String.sub s (String.length s - String.length suf) (String.length suf)  = suf	

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
let block_anals anals tag = 
	let anals = List.filter (ends_with tag) anals in
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
	(List.map strip_annot (Parse.split2 '+' anal))
;;



let stem (hanal, lowercase, decompounding, guessing_on, oov_filter, known_filter) word tag =
    let anals = Morphtable.analyze hanal word in
	if word = "Medgyessyt" then
		Printf.eprintf "anals: %s\n" (String.concat " " anals);
	(* ocamorph neha duplumokat ad vissza*)
	let anals = unique anals in 
	
	let tag = String.lowercase tag in
	let lowercased_anals = if lowercase then (List.map (String.lowercase) anals) else anals in
		
	let (guessed, blocked_anals) = block_anals  lowercased_anals tag in
		if word = "Medgyessyt" then begin
		Printf.eprintf "blocked anals: %s\n" (String.concat " " blocked_anals);
end;

	let (guessed, blocked_anals) = 
	if List.length blocked_anals = 0 then 
		block_anals lowercased_anals "" 
	else
		(guessed, blocked_anals)
	in	
	let lemmas =
        if (List.length blocked_anals = 0) || guessed && (not guessing_on) then 
			(* ha guessed, de nem kertek guessinget, akkor a szo maga a lemma *)
			let normalized = if lowercase then (String.lowercase word) else word in
			(normalized::[]) :: []
		else let lemmas = List.map (parse_anal) blocked_anals in
		
		 match guessed with
			true -> oov_filter lemmas
		  | false -> known_filter lemmas
	in
	if decompounding then
		(* az osszetettszo komponenseket kulon stemnek vesszuk *)
		unique (List.flatten  lemmas)
	else
		(* osszetettszavakat osszerakjuk *)
		unique (List.map (String.concat "") lemmas)
;;





module Hmm_tagger = Hmm_tagger


let tag_sentence tagger stemmer sentence =
    let obs, tags = tagger sentence in
	let print_tagged word tag =
		print_string word; print_char '\t'; print_string tag;
		print_char '\t'; print_endline (String.concat "" (stemmer word tag))
	in
	List.iter2 print_tagged (List.rev sentence) (List.rev tags);
	print_newline ()
;;

let total = ref 0

let falses = ref 0


let usage () = 
	Printf.eprintf "usage : %s  modelfile tagtable morphtable [tag-order [emission-order]] \n" Sys.argv.(0)
;;

let _ =	

if (Array.length Sys.argv) < 4 then 
	let _ = usage () in	exit 1 
else

let tagorder =
	if( Array.length Sys.argv) > 4 then (int_of_string Sys.argv.(4)) else 2
in

let emorder =
	if (Array.length Sys.argv) > 5 then (int_of_string Sys.argv.(5)) else 2
in	
let tagmorph = Morphtable.load Sys.argv.(2) in
let hunmorph = Morphtable.load Sys.argv.(3) in
let tagger = Hmm_tagger.load Sys.argv.(1)   tagmorph tagorder emorder in

let stemmer w = stem (hunmorph, true, false, true, filter LongestLemma, filter LongestLemma ) w in

let ic =  stdin in


Io.iter_sentence ic (tag_sentence  tagger stemmer)
