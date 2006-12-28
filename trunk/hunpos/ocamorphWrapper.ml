(* wrapper az ocamorph kore: ahogy taggeleshez szeretnenk latni *)

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type analysis = ( (string * string * string ) list * bool)

type analt = (string -> (bool * (string list)))

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
    
    
(* kiszedi a hunmorph bemenetebol azokat a karaktereket, amik miatt kesobb nem tudnank parszolni
    a KR kodot. 3+2 bemenet eseten a kimenet -> 3+2?NUM, ami majdnem osszetettszonak nez ki *)
let normalize word =
    let len = String.length word in
    let output = String.create len in
    for i = 0 to len - 1 do
        String.set output i(
        let c = String.get word i  in
        match c with
         | '?' | '/' | '<' | '>' | '+' -> '_'
         | _ -> c
        )
    done;
(*	print_endline output; *)
    output
    
(* egy sajat elemzo fuggvenyt allitunk elo, remelve, hogy az ocamorph csinal currying-et *)

let create_analyzer bin_file =
     let engine = Analysis.make_marshal bin_file  in
     (* harom fuggvenyt hivunk majd egymas utan. 
        az elso a sima szavak: nem osszetett ismert szo *)
        
     (* / stop_at_first    0 1 for indexing
        // blocking         0 1
        // compounds        0 1
        // guess            0 1 2 *)
    
    let basic_anal = match (engine false true false Analysis.NoGuess) with
        Analysis.Analyzer (f)-> f
        | _ -> failwith (" inkompatibilis ocamorph: stemmert adott vissza") 
    in  
    
    (* ha majd nem ad eredmenyt az elozo, akkor megengedjuk az osszetettszot *)
    
    let compound_anal = match (engine false true true Analysis.NoGuess) with
        Analysis.Analyzer (f)-> f
        | _ -> failwith (" inkompatibilis ocamorph: stemmert adott vissza") 
    in  
        
    (* guesser uzemmodban viszont nem erdekel most minket az osszetett szo:
       faxnigép?/NOUN
       faxni?NOUN+gép/NOUN
       csak az elso kell    
     *)
    
    let guesser = match (engine false true false Analysis.Global) with 
        Analysis.Analyzer (f)-> f
        | _ -> failwith (" inkompatibilis ocamorph: stemmert adott vissza") 
    in  
        
    (* es akkor a komponalt fuggvenyunk *)
    
    fun word -> (* eloszor is kiszedjuk a zavaro karaktereket *)
                let word = normalize word in
                let (_, res) = basic_anal word in
                if List.length res > 0 then (false, unique res) else
                let (_, res) = compound_anal word in
                if List.length res > 0 then (false, unique res) else
                let (_, res) = (guesser word) in (true, unique res)

		
let init () = 
	let bin_file = ref "/Users/hp/work/cvs/lexicons/morphdb.hu/out/morphdb_hu.bin" in 
  (* / stop_at_first    0 1 for indexing
    // blocking         0 1
    // compounds        0 1
    // guess            0 1 2 *)
    (create_analyzer !bin_file, load_cache () )


(* szetvagja az elemzest a lemmara es szufixxumokat helyettesito KR kodolt elemzesekre.
	majdnem jo, mert az osszetettszavaknal benne hagyja a + jelet es a nem utolso komponens
	POS kodjait.
*)
let split_anal anal =
(*	print_string "splitting: " ; print_endline anal;*)
	let l = String.length anal in
	(* az utolso osszetettszo hatar *)
	let p = try String.rindex  anal '+' with Not_found -> 0 in
	(* ezutani elso / v. ? jel *)
	let s = try String.index_from anal p  '/' with Not_found -> l in
	let q = try String.index_from anal p '?' with Not_found -> l in

	let i = min s q in
	let tag = String.sub anal (i+1) (l -i - 1) in
    let remained = String.sub anal 0 (i) in (* nem i+1, mert a / jel nem kell sehova sem*)
	(remained,tag)

let analyze ((hanal,  cache):analyzer) word =

	(* ha a cache-ben benne, akkor visszaadjuk *)
	try Hashtbl.find cache word  with Not_found ->
	(*	print_string "not found " ; print_endline word; *)

	(* egyebkent, ugye elemzes *)
	let (guessed,  anals) =  (hanal (String.copy word)) in
    let res = List.map (fun anal -> let s,p = split_anal anal in (s, p, anal)) anals in
	Hashtbl.add cache word (res,guessed) ;
	(res,guessed)

let tags (anals,_) =
    let set = List.fold_left (fun set (_,p, _)  -> SSet.add p set) (SSet.empty) anals  in
	SSet.fold (fun s l -> s :: l) set []


let oov (_,  guessed) =guessed

(* visszaadja a szo lemmait es a hozzatartozo lehetseges suffixomokat (elemzeseket) *)
let lemmas_with_suffixes (anals, _) =
    let map = List.fold_left (fun map (s, p,_)  -> 
									let l = try SMap.find s map with Not_found -> [] in 
									SMap.add  s (p::l) map) 
							 (SMap.empty) anals  
	in

	SMap.fold (fun lemma suffixes res -> (lemma, suffixes) :: res ) map []

(*
let _ =
let h = init () in
Io.iter_tokens (open_in "test.train") (fun (word, gold) -> 
let a = analyze h word in 
	(*
	print_string word ; print_char ' ' ; print_endline (String.concat "#" (tags a)) ;
*)
    List.iter (fun (l,sufs) -> print_string l ; print_char ' '; print_endline (String.concat "$" sufs)) (lemmas_with_suffixes a)
)
;		
	
*)	
