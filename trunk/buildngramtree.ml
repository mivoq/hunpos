


let add_sentence (otree, ttree) sentence = 
  let window = Ngram.empty 3 in
  let f (window, otree, ttree) (word, gold)  = 
	let window = Ngram.shift_right window gold in
    let ot = Ngramtree.add otree (word :: window) in
	let tt = Ngramtree.add ttree (window) in
	(window, ot, tt)
  in 
  let ttree = Ngramtree.add ttree (window) in
  let (w, otree, ttree) = List.fold_left (f)  (window, otree, ttree) sentence in
  let (w,  otree, ttree) = f  (w,otree, ttree) ("</S>", "</s>") in
	( otree, ttree)

let chan = open_in "szeged.ful.0.test" 
	(* test.train *) 
	(* szeged.ful.0.test *)



let _ = 
 
  let (otree, ttree) = Io.fold_sentence  add_sentence (Ngramtree.empty, Ngramtree.empty) chan in
(* Ngramtree.print otree; *)
(* Ngramtree.print ttree; *)

(* let olamdas = Deleted_interpolation.calculate_lamdas otree ttree 3 in *)
let tlamdas = Deleted_interpolation.calculate_lamdas ttree ttree 3 in
	Array.iteri (fun i v -> Printf.printf "%d = %f\n" i v) tlamdas;
let ptree = Deleted_interpolation.build ttree ttree tlamdas in


let check_ngram ngram freq =
	Ngram.print ngram;
(*	let prob1=Deleted_interpolation.lprob ptree ngram in *)
	let left = Ngram.chop_right ngram in
	let w = List.hd (List.rev ngram) in
	let prob2=Deleted_interpolation.prob ttree ttree tlamdas left w in

	Printf.printf "%f\n" prob2 
in
 
	
	
Ngramtree.iter check_ngram 3 ttree
(* 
Deleted_interpolation.print ptree;	
*)
	(*
Array.iteri (fun i v -> Printf.printf "%d = %f\n" i v) olamdas;


let words sentence =
	List.map (fun (word, gold) -> word) sentence
in

	
let chan = open_in "szeged.ful.0.test" in
let f = ref 0 in 
let total = ref 0 in
let tag_sentence sentence =
let tags = Tagger_hmm.tag otree olamdas ttree tlamdas (words sentence) in	

List.iter2 (fun (word, gold) tag -> Printf.printf "%s\t%s\t%s\n"  word gold tag  ) sentence tags 
in Io.iter_sentence chan tag_sentence 
*)
