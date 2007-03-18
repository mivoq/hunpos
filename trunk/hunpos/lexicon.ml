(* ez a vacak modul csak arra kell, hogy megmondjuk, egy-egy szo
   milyen cimkeket kaphat. Olyan, mint a morphtable, csak on fly epitjuk *)
 
(* mivel egy-egy szonak csak nehany cimkeje szokott lenni, ezert egy
	egyszeru listaban taroljuk a szo lehetseges cimkeit
 *)
(*
module S = sig
	(* TODO *)
end
*)
module Make (M : Amap.S ) = struct
	
type 'a t = (('a * int) list) M.t

let empty () = M.empty ()
	
(* k elemet hozzaadja l listahoz, ha nincs benne;
   erdekessege, hogy k elemet a lista elejere mozgatja;
   Az esetek nagyon nagy szazalekaban igy az elso elem 
   lesz, amit keresunk.

   ez most itt inkabb ocaml programozasi ujjgyakorlat, masok
   biztos fat v. mittudomen mit hasznalnanak.

   Fogalmam sincs, hogy mennyit gyorsit, de jol jon a PHD-ben
   egy ilyen megoldas.
 *)
let add_to_list l k =
	(* ez itt csak gyorsitas: megnezzuk, az elso elem az-e, amit keresunk
	   ha igen, akkor nem kell modositani semmit.*)
	match l with
		(h, freq)::t when h = k -> (h, succ freq) :: t
		| _ ->
			
	let rec aux l = match l with
		[] -> (k, 0, [])
	   | (h, freq) :: t when h = k -> (h, freq, t)
	   | h::t -> (* itt a trukkozes, es lam nem tail recursive *)
	 			 let (k, freq, t) = aux t in
				 (k, freq, h::t)
	in
	let (k, freq, l) = aux l in
	(k, succ freq) :: l
		
let add_word_tag lex word tag =
	let _ = M.update lex word 
	(fun () -> (tag, 1) :: [])
	(fun tags -> add_to_list tags tag)
	in ()
	
let iter f lex =
	let aux word tags =
		(* itt kiszamoljuk, h a szo hanyszor fordult elo *)
		let wfreq = List.fold_left (fun  wfreq (tag, tfreq)  -> wfreq + tfreq) 0 tags in
		f word wfreq tags
	in
	M.iter aux lex
	
let find_nofreq lex w =
	let l = M.find lex w in
	List.map (fun (tag, freq) -> tag) l
	
end
