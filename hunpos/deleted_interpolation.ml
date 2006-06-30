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


let calculate_lamdas tree n =
	let incrementation lamdas deleted_ngram freq =
		let rec searchmax max maxi i nominator denominator =
			match (nominator, denominator) with 
				(nh::nt, dh::dt) ->  
                    let ratio = if nh < 2 || dh <2 then (0.0) else float (nh -1) /. float (dh -1)  in
					let (max, maxi) = if ratio  > max then (ratio, i) else (max, maxi) in
						searchmax max maxi (succ i) nt dt
				| (_ , _ ) -> (max, maxi)
		in
		let nom   = List.rev (Ngramtree.freq deleted_ngram  tree ) in
		let denom = List.rev (Ngramtree.freq  (List.rev ( List.tl (List.rev deleted_ngram) )) tree) in
        let (_,maxi) =   searchmax (-1.0) (-1) 0  nom denom  in
		if maxi > -1 then lamdas.(maxi) <-lamdas.(maxi) + freq 
	
	in
	let lamdas = Array.create n 0 in
	Ngramtree.iter (incrementation lamdas) n tree;

	(* normalization *)
    let sum = Array.fold_left (fun sum x -> sum+x) 0 lamdas in
	Array.map (fun x -> float x /. float sum) lamdas
		
