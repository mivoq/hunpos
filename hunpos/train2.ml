module SNgramTree = Ngramtree2.Make(Mfhash.String)

let build_modell chan = 
	let tt = SNgramTree.empty () in
	let ot = SNgramTree.empty () in
	let add_sentence (words, tags) =
	
		let rec aux words tags =		
			match words, tags with
			 |  (word::[], tag::[]) -> SNgramTree.add tt tags 2;
			 |	(word::word_tails),
			 	(tag::tag_tails)   -> SNgramTree.add tt tags 3;
									  SNgramTree.add ot (word::tags) 2;
						  			  aux word_tails tag_tails
			 | (_,_) -> ()
		in
		let tags = ("</s>"::tags) @ ("<s>"::[]) in
		let words  = ("</s>"::words) @ ("<S>"::[]) in
	    aux words tags;
	in
	Io.iter_sentence chan add_sentence;
	(tt, ot)
	
	

let usage () = 
	Printf.eprintf "usage : %s corpusfile modelfile \n" Sys.argv.(0)
;;

let _ =	
if (Array.length Sys.argv) < 3 then 
	let _ = usage () in	exit 1 
else

let chan = open_in Sys.argv.(1) in
	(* test.train *) 
	(* szeged.ful.0.test *)



Printf.eprintf "building frequency trees\n";
let (ttree, otree) = build_modell chan in
(* Ngramtree.print otree; *)
(*Ngramtree.print ttree; 
*)
Printf.eprintf "calculating observation lamdas\n";
let olamdas = SNgramTree.calculate_lambdas otree ttree 2 in
Array.iteri (fun i v -> Printf.printf "%d = %f\n" i v) olamdas; 

Printf.eprintf "calculating tag transition lamdas\n";
let tlamdas = SNgramTree.calculate_lambdas ttree ttree 3 in
Array.iteri (fun i v -> Printf.printf "%d = %f\n" i v) tlamdas; 


Printf.eprintf "deriving observation probabilities\n";
let potree = SNgramTree.probtree otree ttree olamdas in

Printf.eprintf "deriving tag transition probabilities\n";
let pttree = SNgramTree.probtree ttree ttree tlamdas in

Printf.eprintf "model is trained.\n";
Printf.eprintf "saving.\n";

let oc = open_out Sys.argv.(2) in
Marshal.to_channel oc (potree, pttree) [];
close_out oc;
