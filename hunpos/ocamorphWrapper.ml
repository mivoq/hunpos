(* wrapper az ocamorph kore: ahogy taggeleshez szeretnenk latni *)

module SMap = Map.Make(String)
module SSet = Set.Make(String)
(* elemzesek inf_tag=ekbol es a hozzajuk tartozo teljes elemzestbol allnak *)
type analysis = ( (SSet.t) SMap.t * bool *  bool)

(* ez nem kene, ha az ocamorph interfesze tartalmazna a sajat elemzo fuggvenyenek a tipusat*)
type analt = (string -> (int * string list))

(* tartunk egy hashtablat, amibe minden elemzest berakjuk, durva cache*)
type analyzer = ( analt *  (string, analysis) Hashtbl.t)

let save_cache  ((_,  cache):analyzer)  =
	let oc = open_out "ocamorph.cache" in
	Marshal.to_channel oc cache [];
	close_out oc

let load_cache () = try 
	let ic = open_in "ocamorph.cache" in
	let cache = Marshal.from_channel ic in
	close_in ic ;
	cache
	with Sys_error(_) -> 
	Hashtbl.create 1000 
	
	
let init () = 
	let bin_file = ref "/Users/hp/work/cvs/lexicons/morphdb.hu/out/morphdb_hu.bin" in 
  (* / stop_at_first    0 1 for indexing
    // blocking         0 1
    // compounds        0 1
    // guess            0 1 2 *)
    let a = Analysis.make_marshal !bin_file false true true Analysis.Fallback in
   	match a with
    	Analysis.Analyzer (f)-> 
			let cache = load_cache () in
							(f, cache)
		| _ -> failwith (" inkompatibilis ocamorph: stemmert adott vissza") 

(** egyszeru heurisztikaval szuri az elemzeseket, es melle teszi, hogy
	hogy osszetett szo-e es/vagy guessed-e, lehet a ketto egyszerre. *)
let filter_anals anals = 
	let annotate_anal anal = 
		let m = String.length anal - 1 in
		(* ha van benne plusz akkor compound, ha van benne ? akkor guessed *)
		let rec aux  i compound guessed =
			if i = m then (compound, guessed) else
			if compound && guessed then (true, true)  (* mar nem kell tovabb menni *) else
            let compound = if compound then true else  anal.[i] = '+' in
			let guessed  = if guessed  then true else  anal.[i] = '?' in
			aux (succ i) compound guessed
		in
		let (compound, guessed) = aux 0 false false in
		(anal, compound, guessed)
	in
	let annotated = List.map annotate_anal anals in
	match annotated with
	h :: [] -> annotated (* gyorsitas, ha csak egy elemzes van, akkor nem rugozunk *)
	| _ ->
		 let isnotguessed  (_, _, guessed) = not guessed in 
   		 let isbasic (_, compounds, guessed) = not guessed && not compounds in
         if List.exists isbasic annotated  then List.filter isbasic annotated  else
   		 if List.exists isnotguessed annotated   then List.filter isnotguessed annotated  else annotated

(* visszaadja az elemzes tag reszet *)
let get_tag anal =
	let rec get_pos_sep_index a i = 
	        if i < 0 then 0 else if (a.[i] = '/' || a.[i] = '?') then i else get_pos_sep_index a (i-1)
	in
	let l = String.length anal in
    let i = get_pos_sep_index anal (l - 1) in 
	let tag = String.sub anal (i+1) (l -i - 1) in

	tag
	


let analyze ((hanal,  cache):analyzer) word =

	(* ha a cache-ben benne, akkor visszaadjuk *)
	try Hashtbl.find cache word  with Not_found ->
	(*	print_string "not found " ; print_endline word; *)
	
	(* egyebkent, ugye elemzes *)
	let (_, anals) = hanal (String.copy word) in
	(* fallback szures es annotalas*)
    let filtered_anals = filter_anals anals in	
	(*
	let (ha, hc, hg)::t =  filtered_anals in
    let isbasic = not (List.fold_left (fun   b (_, c, g) ->( b || c || g)) (hc || hg) t) in
	let iscom =  (List.fold_left (fun   b (_, c, g) ->( b && (hc && (not hg)))) (hc && (not hg)) t) in
	let isgus =  (List.fold_left (fun   b (_, c, g) ->( b && hg)) (hg) t) in	
	Printf.printf "%s basic: %B com: %B gus: %B\n" word isbasic iscom isgus;
	*)		
	let add_anal  (anals, ocompounds, oguess) (anal, compound, guessed) =
		(* egy map, amiben
			 mindden taghez tartozo elemzesek Set-je van *)
		let tag = get_tag anal in
		let fulanals = try SMap.find  tag anals with Not_found -> SSet.empty in
		let fulanals = SSet.add  anal fulanals in
		(SMap.add  tag  fulanals anals, compound, guessed)
	in
    let anals = List.fold_left add_anal (SMap.empty, false, false)   filtered_anals in
    Hashtbl.replace cache word anals ;
	anals

let tags (tagmap, compunds, guess) =
	SMap.fold (fun k a tags -> k::tags ) tagmap []
	
let oov (_, _, guess) =guess

let compound (_, c, _) = c
	
(* adott taghez tartozo elemzesek *)
let full_analyses ((tagmap, _, _):analysis) tag =
	let faset = SMap.find  tag tagmap in
    SSet.fold (fun k l-> k::l)  faset [] 
(*	 
let _ =
let h = init () in
Io.iter_tokens (open_in "test.train") (fun (word, gold) -> 
	 let a = analyze h word in Printf.printf "%s\t%B %B\n" word (compound a) (oov a)) ;

save_cache h
*)	
	(* 759 basic: false com: false gus: true
	 904 basic: false com: true gus: false
	14947 basic: true com: false gus: false*)		