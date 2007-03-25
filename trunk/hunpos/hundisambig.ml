open Kr_disambiguator

let inlex2str = function
	Kr_disambiguator.Known -> "K"
	| Oov -> "O"
	| Guessed -> "G"

let intrain2str = function
	Seen -> "S"
	| LowerCasedSeen -> "UL"
	| SpecialToken -> "UD"
	| UnSeen -> "U"
	| Na -> "0"
	
let process_sentence disambig sentence =
    let annotations = disambig sentence in
	let print_annotated word annotation =
		let (inlex, intrain, anals) = annotation in
		print_string word; 
		print_char '\t'; 
		print_string (serialize_analyses anals); 
		print_char '\t';
		print_string (inlex2str inlex);
		print_char '\t';
		print_string (intrain2str intrain);
		print_newline ()
	in
	List.iter2 print_annotated (List.rev sentence) (List.rev annotations);
	print_newline ()
;;

(***********************************
	main
	********************************)


let decompounding_ref = ref true 
let lowercase_ref = ref true 
let guessing_ref = ref true 
let heur_known = ref LongestLemma 
let heur_oov = ref ShortestLemma 
let hmm_model_ref = ref ""  
let morphtable_ref = ref ""  
let use_tagger_ref = ref true 

let usage  =
"Usage: hunmorph++  [options] \nReads words from the standard input and 
prints them to the stdout with their morphological analyses separated by TAB.\n"
;;
	
let others s = 
	 invalid_arg ("Oops, some error. Notice, options require a leading '--'. Watch '"  ^ s ^ "'\n");
;;
let set_binary_arg refered s = match s with
	"yes" -> refered := true
	| "no" -> refered := false
	| _ -> invalid_arg "use `yes' or `no' for boolean arguments";
;;

let set_oov_heur s = 
	heur_oov := match s with
	| "all"      ->   All
	| "shortest" ->  ShortestLemma
	| "longest"  -> (invalid_arg "hey, using --stem_oov longest means --guessing no!");
	| _	         -> (invalid_arg "Invalid --heur_oov option!");
;;

let set_known_heur s =
	heur_known := match s with
	| "all"      ->  All
	| "shortest" ->  ShortestLemma
	| "longest"  ->  LongestLemma
	| _	         -> (invalid_arg "Invalid --heur_known option!")
;;

let speclist = 
Arg.align [
	 "--morphtable",   Arg.Set_string morphtable_ref, " Lexicon processed by ocamorph";
	 "--tagger-model",   Arg.Set_string hmm_model_ref, " Trained HMM model if using use-tagger=yes";
	 "--use-tagger",  Arg.String (set_binary_arg use_tagger_ref) , " Use HMM tagger to narrow possible analyses `yes' | `no' (default = yes)" ;
     "--lowercase", Arg.String (set_binary_arg lowercase_ref) , " Convert all characters to lower case `yes' | `no' (default = yes)" ;
	 "--decompounding",   Arg.String (set_binary_arg decompounding_ref)   , " Enable decoumpounding `yes' | `no' (default = yes)" ;
     "--guessing",   Arg.String (set_binary_arg guessing_ref)   , " Enable guessing `yes' | `no' (default = yes)" ;
	 "--heur-known", Arg.String (set_known_heur), " Heuristic used to choose the best analysis of known words `shortest' | `longest' |  `all'  (default = longest)\n";	 
     "--heur-oov", Arg.String (set_oov_heur), " Heuristic used to choose the best analysis of OOV words `shortest'  |  `all'  (default = shortest)\n";	 
]


let _ =
try
	Arg.parse speclist others usage;
	if !morphtable_ref = "" then
		(invalid_arg "Without morphtable I can't work. I can't analyze.")
	;
	
	if !use_tagger_ref && !hmm_model_ref = "" then
		( invalid_arg "No HMM model, specify --tagger-model argument or use --use-tagger=no\n";) ;


	with Invalid_argument (s) ->
		( Printf.eprintf "%s\n" s ; Arg.usage speclist usage; exit 0 ) 
		



let _ =	
	







let tagorder = 2 in
let emorder = 2 in
let	lowercase = !lowercase_ref in
let decompounding = !decompounding_ref in
let guessing_on = !guessing_ref in
let known_heur = !heur_known in
let oov_heur = !heur_oov in

	
let morphtable = Kr_morphtable.load !morphtable_ref in
let tagger =
	if !use_tagger_ref then
		let morphtable_tagger = Kr_morphtable.tags morphtable in
		let model = Hmm_tagger.load !hmm_model_ref in
		let tagger = Hmm_tagger.compile_tagger  model morphtable_tagger tagorder emorder in
		Some tagger	
	else
		None
in
	

let morphtable_analyzer = Kr_morphtable.analyze morphtable in

let disambiguator =  disambiguator tagger morphtable_analyzer lowercase decompounding guessing_on known_heur oov_heur
in
let ic =  stdin in
Io.iter_sentence ic (process_sentence   disambiguator ) 


(* 
	let ends_with suf s =
		if String.length suf > String.length s then false
		else
		String.sub s (String.length s - String.length suf) (String.length suf)  = suf	
	
*)