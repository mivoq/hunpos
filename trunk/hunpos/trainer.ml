let add_sentence (otree, ttree) sentence = 
  let window = Ngram.empty 2 in
  let f (window, otree, ttree) (word, gold)  = 
	let new_window = Ngram.shift_right window gold in
    let ot = Ngramtree.add otree (word :: new_window) in
	let tt = Ngramtree.add ttree (gold :: window) in

	(new_window, ot, tt)
  in 
  let ttree = Ngramtree.add ttree (window) in
  let (w, otree, ttree) = List.fold_left (f)  (window, otree, ttree) sentence in
	  let (w,  otree, ttree) = f  (w,otree, ttree) ("</S>", "</s>") in
	( otree, ttree)

let chan = open_in "data/szeged.ful.newest.0.train" 
	(* test.train *) 
	(* szeged.ful.0.test *)



let _ = 
Printf.eprintf "building frequency trees\n";
let (otree, ttree) = Io.fold_sentence  add_sentence (Ngramtree.empty, Ngramtree.empty) chan in
(* Ngramtree.print otree; *)
(* Ngramtree.print ttree; *)

Printf.eprintf "calculating observation lamdas\n";
let olamdas = Deleted_interpolation.calculate_lamdas otree ttree 3 in

Printf.eprintf "calculating tag transition lamdas\n";
let tlamdas = Deleted_interpolation.calculate_lamdas ttree ttree 3 in
(*	Array.iteri (fun i v -> Printf.printf "%d = %f\n" i v) olamdas; *)


Printf.eprintf "deriving observation probabilities\n";
let potree = Deleted_interpolation.build otree ttree olamdas in

Printf.eprintf "deriving tag transition probabilities\n";
let pttree = Deleted_interpolation.build ttree ttree tlamdas in

Printf.eprintf "model is trained.\n";
Printf.eprintf "saving.\n";

let oc = open_out "data/trained_modell" in
Marshal.to_channel oc (potree, pttree) [];
close_out oc;
