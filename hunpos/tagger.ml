module Hmm_tagger = Hmm_tagger


open Getopt
open Printf

let proc_err s = 
  prerr_string s;
  exit 1
    
;;

let help () = 
printf "hunpos-tag: HunPos HMM based tagger\n\n"; 
printf "Usage: hunpos-train [options] model-file\n\n"; 
printf "Reads the untagged corpus from the stdin and writes the\n";
printf "tagged version to the stdout.\n\n";
printf "-m,  --morphtable=FILE        Morphological lexicon. (default: do not use)\n";
printf "\n";
printf "Performance tuning:\n";
printf "-g,  --max-guessed-tags=NUM   only the most probable NUM tags used (default = -g10)\n";
printf "-b,  --beam-theta=NUM              use NUM as theta in viterbi beam search (default = -t1000)\n";
printf "\n";
printf "Output options:\n";
printf "-t,  --tokens-types           print the types of each token (default = no)\n";
printf "\n";
  


printf "-h,  --help                   Print this help.\n";
    exit (-1);
;;


let typ2string = function
	Hmm_tagger.Seen -> "S"
	| Hmm_tagger.LowerCasedSeen -> "*L"
	| Hmm_tagger.SpecialToken -> "*D"
	| Hmm_tagger.UnSeen -> "*"

	
let tag_sentence marktokens tagger  sentence =
    let obs, tags = tagger sentence in
	let print_tagged obs tag =
		print_string obs.Hmm_tagger.word; print_char '\t'; print_string tag;
		print_char '\t'; 
		if marktokens then 
		    print_endline (typ2string obs.Hmm_tagger.seen)
		else
		    print_newline ();
	in
	List.iter2 print_tagged (List.rev obs) (List.rev tags);
	print_newline ()
;;

let total = ref 0
let morphtable = ref ""
let model_file = ref ""  
let max_guessed_tags = ref 10
let marktokens = ref false 
let theta = ref 1000
  
let string_arg var =  Some (fun x -> var := x ) 
;;

let int_arg varname var = Some (fun x -> 
    try 
        let i = int_of_string x in 
        var := i 
    with _ -> raise (Error ("not valid value for " ^ varname ))
)
      
  let float_arg varname var = Some (fun x -> 
  try 
    let i = float_of_string x in 
    var := i 
  with _ -> raise (Error ("not valid value for " ^ varname ))
)


let specs = 
[
  ( 'm', "morphtable", None, string_arg morphtable);
  ( 'g', "max-guessed-tags", None, int_arg "max-guessed-tags" max_guessed_tags);
  ( 't', "token-types", set marktokens true, None);
  ( 'b', "beam-theta", None, int_arg "beam-theta" theta);
  ( 'h', "help", Some help, None)

]

let main () =
    (
       try 
         parse_cmdline specs (fun x -> model_file := x );
         if !model_file = "" then raise (Error "No model-file given.");

       with Getopt.Error s -> proc_err (sprintf "%s\n\nPlease try: hunpos-tag -h\n" s)
     );
     
    let hunmorph = 
        if !morphtable = "" then 
            (* using no morphtable *)
            (fun (s:string) -> raise Not_found)
        else
            let m = Morphtable.load !morphtable in
            Morphtable.tags m
    in
    
    let model = Hmm_tagger.load !model_file in
    
    prerr_endline "model loaded";
    let tagger = Hmm_tagger.compile_tagger  model hunmorph  !max_guessed_tags (log (float_of_int !theta)) in
    prerr_endline "tagger compiled";

    let ic =  stdin in
    Io.iter_sentence ic (tag_sentence  !marktokens tagger )


let _ = main();
