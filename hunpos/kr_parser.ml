(* Itt nehany olyan funkcio, amit az ocamorph nyelvfuggetlensege miatt nem tud.
   
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

let split_at s ix =
	let l = String.length s in
	if ix < 0 || ix >= l then raise (Invalid_argument "ix")
	else
	((String.sub s 0 ix) , String.sub s (ix+1) (l - ix - 1))
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
	let anals = unique anals in
	let isnotguessed  a = not (String.contains a '?') in
	let isbasic a = not (String.contains a '+' || String.contains a '?') in
	if List.exists isbasic anals  then (false, List.filter isbasic anals)  else
	if List.exists isnotguessed anals   then (false, List.filter isnotguessed anals)  else (true, anals)
	
;;	

(* igy fogunk vegigmenni:
   1. az osszetettszavas elemzeseket felbontjuk	
*)

let split_compounds a = split '+' a ;;
	
(*
  2. a lemmatol levalasztjuk az annotaciot. Vagy ? vagy / jelnel.
*)
let split_lemma_annot a = 
	let ix = try String.index a '?' with  Not_found -> try String.index a '/' with Not_found -> -1 in
	if(ix > 0) then
		((String.sub a 0 ix), String.sub a (ix + 1) ((String.length a) - ix -1))
	else
	(a, "")
;;	

(*
  3. derivacio, pos es inflexio harmasra bont
	
  hoppa! a / jel tobbszor is lehet
  kereszt/NOUN[ACT]/VERB[GERUND]/NOUN<POSS><CAS<ADE>>

*)



let split_deriv_pos_inf a =
	let (deriv ,inf) =
		try let ix = String.rindex a '/' in
			split_at a ix 
		with Not_found -> ("",a) 
	in
		
	let pos = 
		try let ix = String.index inf '<' in
			String.sub inf 0 ix
		with Not_found -> inf
	in
	(deriv,inf, pos)		
	
(* es most minden egyutt *)
let parse_anal a =
	let compounds = split_compounds a in
	List.map (fun component -> 
				let lemma, annot = split_lemma_annot component in
				let deriv, inf, pos = split_deriv_pos_inf annot in
				(lemma, deriv, pos, inf)	
			 ) compounds
			
			
(* (lemma, deriv, pos, inf) *)		
let parse anals =
	let (guessed, anals) = block_anals anals in
	(guessed, List.map (parse_anal) anals)
			
			
(* visszaadja a tageket: osszetettszonak csak az utolso tagja szamit *)
let inflections parsed_anals =
	let tag anal = 
		let (_,_,_, tag) = List.hd (List.rev anal) in
		tag
	in
	let tags = List.fold_left (fun acc anal -> (tag anal) :: acc) [] parsed_anals in
	unique (tags )
;;
(*
let _ =
 	let anals = "sváb/ADJ+hegy/NOUN<CAS<SUE>>" :: "sváb/NOUN+hegy/NOUN<CAS<SUE>>" :: [] in
	let (guessed, anals) = parse anals in
	let inflections = inflections anals in
	Printf.printf "%b %s\n" guessed (String.concat "\t" inflections);
	let f = List.hd anals in
	Printf.printf "length = %d\n" (List.length f);
*)