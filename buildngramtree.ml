let add_sentence t sentence = 
  let window = Ngram.empty in
  let f (window, t) (word, gold)  = 
	let window = Ngram.shift_right window gold in
    let t = Ngramtree.add t (window) in
	(window, t)
  in
  let (w, t) = List.fold_left (f)  (window, t) sentence in
  let (w, t) = f  (w,t) ("</s>", "</s>") in
	t

let chan = open_in "szeged.ful.0.test" 
	(* test.train *) 
	(* szeged.ful.0.test *)

let rec build_tree t =
  try 
    let sentence = Io.read_sentence chan in
    build_tree (add_sentence t sentence)
  with End_of_file -> t

let _ = 
  print_string "hello\n";
  let tree = Ngramtree.empty in
  Printf.printf "building...\n";
  let tree = build_tree tree in

  let print_ngram tree ngram freq =
	print_int freq; print_char ' ';
	Ngram.print ngram ;
	let freqv = Ngramtree.freq ngram tree in
	  List.iter (fun f -> print_int f; print_char ' ';) freqv ;
	  print_newline ();
 in
Ngramtree.print tree;
(*	Ngramtree.iter (print_ngram tree) 3 tree; *)
	let lamdas = Deleted_interpolation.calculate_lamdas tree 3 in
		Array.iteri (fun i v -> Printf.printf "%d = %f\n" i v) lamdas
(*  Printf.printf "printing...\n";
  flush stdout;
  Ngramtree.print tree;
*)