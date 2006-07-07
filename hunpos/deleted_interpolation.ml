(************************************************************************
*
* A lineáris interpoláció, hogy egy trigram valószinűségét
*
* P(t2 | t0 t1) = l3*P(t2| t0, t1) + l2*P(t2 | t1) + l1*P(t2)
*
* alakban becsüljük. A deleted interpoláció a paraméterek kiszámolásának
* egyik módja. Az algoritmus alapja, hogy a paramétereket úgy kell beállítani,
* hogy egy új trigrammot a lehető legjobban becsüljünk. A fenti képlet három 
* tagját képzeljük el három becslőnek. Annak nagyobb a súlya (lamda értéke)
* amelyik pontosabban becsül.
*
* A tanító korpuszt bontsuk két részre. Az első darab alapján állítsuk be a
* három becslőt, a többi alapján pedig a paramétereket: a második korpusz
* minden trigramjára nézzük meg melyik tag a maximális, és annak adjunk
* egy pontot. Az összes trigram után normalizáljuk a lamdákat 1 összegre.
*
* Szélsőséges eset, amikor a tanító korpuszt úgy bontjuk két részre, hogy
* a második korpuszban pontosan 1 trigram marad csak. Ez nem lesz jó 
* paraméterbeállításnak, de ezt megcsinálhatjuk minden trigramra is. Ez
* a deleted interpoláció: töröljük a korpusz egy trigramját, megnézzük azt
* mi becsüli a legjobban, kiosztjuk a pontot a becslőknek, majd új trigrammal
* folytatjuk.
*
* Összefoglalva az algoritmus (Brants 2000) alapján
*
* set l1 = l2 = l3 = 0
* foreach trigram t1, t2, t3 with f(t1, t2, t3) > 0
*   depending on the maximum of the following three values:
*           f(t1, t2, t3) - 1
*          -----------------     increment l3 by f(t1, t2, t3)    
*            f(t1, t2) -1
*
*           f(t2, t3) - 1
*          -----------------     increment l2 by f(t1, t2, t3)    
*            f(t2) -1
*
*           f(t3) - 1
*          -----------------     increment l1 by f(t1, t2, t3)    
*            N -1
*    end
* end
* normalize l1, l2, l3
*
************************************************************************)


let calculate_lamdas ntree dtree n =
	let incrementation lamdas deleted_ngram freq =
		let rec searchmax max maxi i nominator denominator =
			match (nominator, denominator) with 
				(nh::nt, dh::dt) ->  
                    let ratio = if nh == 1 || dh == 1 then (-1.0) else float (nh -1) /. float (dh -1)  in
					let (max, maxi) = if ratio  > max then (ratio, i) else (max, maxi) in
						searchmax max maxi (succ i) nt dt
				| (_ , _ ) -> (max, maxi)
		in
		let nom   = List.rev (Ngramtree.freq deleted_ngram  ntree ) in
		let denom = List.rev (Ngramtree.freq   (Ngram.chop_right deleted_ngram)  dtree) in
        let (_,maxi) =   searchmax (-1.0) (n) 0  nom denom  in
		lamdas.(maxi) <-lamdas.(maxi) + freq 
	
	in
	let lamdas = Array.create (n+1) 0 in
	Ngramtree.iter (incrementation lamdas) n ntree;

	(* normalization *)
    let sum = Array.fold_left (fun sum x -> sum+x) 0 lamdas in
	Array.map (fun x -> float x /. float sum) lamdas

(* l0 P(t3 | t1 t2) + l1 P(t3 | t2) + l2 P(t3) 
   P(t3 | t1 t2) = freq (t1 t2 t3) / freq (t1 t2)

*)


type ptree =  Node of float * float * ptree Ngramtree.Cmap.t
let empty = Node(0., 0. , Ngramtree.Cmap.empty)
	
(* Fog egy fat és felepit egy ugyanolyant, csak abban mar a valoszinusegek lesznek *)
let build ntree dtree lamdas = 

		
	let rec clone_node lamdas level p_to gram_to (Ngramtree.Node (freq, thilds)) =
	   let rec add_child lamdas level gram (Ngramtree.Node (freq, childs) as old_child) (Node(p, f2, childs) as new_parent)  =
	 	   let ngram = gram :: gram_to in
		   let new_child = clone_node (List.tl lamdas) (succ level) p ngram old_child in
		   Node(p, f2, Ngramtree.Cmap.add gram new_child childs)
		in 
	(* Ngram.print gram_to; *)

	let denom_gram = Ngram.chop_right gram_to in

	let denom = Ngramtree.ngram_freq (List.rev denom_gram) dtree in

	let lamda = List.hd lamdas in
	let p = lamda *. float freq /. float denom +. p_to  in
(*
	Printf.printf "cloning at level %d\n" level;
	Printf.printf "p + lamda * freq (%s) / freq(%s) = %f + %f * %d / %d = %f " ( String.concat " " gram_to)  (String.concat " " denom_gram) p_to lamda freq denom p ;
*)	
    Ngramtree.Cmap.fold (add_child lamdas level) thilds (Node(p, 0., Ngramtree.Cmap.empty))
	in
	(* Igy elol van lamda3, ami P(C) szamolasahoz kell *)
	let lamdas = List.rev (Array.to_list lamdas) in
    let root = clone_node lamdas 0 0. [] ntree in
	(* most megegyszer vegigrohanunk rajta es logot veszunk *)

		let  rec log_node  (Node(p, f2, childs)) =
			let log_childs = Ngramtree.Cmap.map (fun node -> log_node node) childs in
			(Node((log p), f2, log_childs))
		in
	(* log_node *) root

let rec lprob_aux p_to (Node(p, f2, childs)) ngram =
		match ngram with 
			| [] -> p
			| h::t -> try lprob_aux (p) (Ngramtree.Cmap.find h childs) t with Not_found -> p_to


let lprob_lf ptree ngram =
	lprob_aux 0. ptree ngram
	
let lprob ptree ngram =
	lprob_aux 0. ptree (List.rev ngram)
	 
let print t =
	let rec print_tree level str (Node (f1, f2, m)) =
		for i = 1 to level do
			print_char '\t'
		done;
 		Printf.printf "%s\t%f\t%f\n" str f1 f2;
    	Ngramtree.Cmap.iter (fun k v -> print_tree (succ level) k v) m
	in
	print_tree 0 "root" t
		
let prob ntree dtree lamdas ngram word =
(*
	print_string "prob calc for: " ; Ngram.print ngram;
*)
	let nom   = List.rev (Ngramtree.freq_word ngram  word ntree ) in
	let denom = List.rev (Ngramtree.freq  ngram dtree) in
(*
	List.iter (fun i -> print_int i ; print_char ' ';) nom;
	print_newline();
	List.iter (fun i -> print_int i ; print_char ' ';) denom;
	print_newline();
*)
	let rec sumup p nom denom i =
		match (nom, denom) with 
			(nh::nt, dh::dt) -> 

				Printf.printf "+ %f %d / %d" lamdas.(i) nh dh ;  

				let ratio = if nh == 0 || dh == 0 then 0.0 else float (nh)  /. float (dh) in
                let p = p +. (lamdas.(i) *. ratio)  in
				sumup p nt dt (succ i)
			| (_ , _ ) ->  Printf.printf " +%f\n" lamdas.(i);  p +. lamdas.(i)
	in		
    sumup (0.0) nom denom (0)