
open Getopt
open Printf

let proc_err s = 
  prerr_string s;
  exit 1
    
;;

let help () = 
printf "hunpos-train: train hunpos tagger from corpus\n\n"; 
printf "Usage: hunpos-train [options] model-file\n\n"; 
printf "Reads the training set from the stdin and writes the\n";
printf "parameters of the tagger to the model-file.\n\n";
printf "HMM model:\n";   
printf "-t,  --tag-order=NUM          Order of tag transitions. NUM=2 means trigram tagging. (default: -t2)\n";
printf "-e,  --emission-order=NUM     NUM=1 -> P(w_i | t_i), NUM=2 ->P(w_i | t_{i-1} t_i). (default: -e2)\n";
printf "\n";
printf "Unknown words:\n";
printf "-s,  --suffix-length=LEN      use suffix trie with max. suffix length = LEN; (default = -s10)\n";
printf "-f,  --rare-frequency=NUM     add only words to the suffix trie with max. freq < NUM; (default = -f10)\n";
printf "\n";
printf "-h,  --help                   Print this help.\n";
    exit (-1);
;;
    



let emission_order = ref 2
let tag_order = ref 2 
let max_freq_for_guess = ref 10
let max_suffix_length = ref 10 
let model_file = ref "" 



let string_arg var =  Some (fun x -> var := x ) 
;;

let int_arg varname var = Some (fun x -> 
    try 
        let i = int_of_string x in 
        var := i 
    with _ -> raise (Error ("not valid value for " ^ varname ))
)


let specs = 
[
  ( 's', "suffix-length", None, int_arg "suffix-length" max_suffix_length);
  ( 'f', "rare-frequency", None, int_arg "rare-frequency" max_freq_for_guess);
  ( 'e', "emission-order", None, int_arg "emission-order" emission_order);
  ( 't', "tag-order", None, int_arg "tag-order" tag_order);
  ( 'h', "help", Some help, None)

] 

let main () =
  (
    try 
      parse_cmdline specs (fun x -> model_file := x );
      if !model_file = "" then raise (Error "No model-file given.");
       
    with Getopt.Error s -> proc_err (sprintf "%s\n\nPlease try: hunpos-train -h\n" s)
  );
 
let chan =  stdin in
	(* test.train *) 
	(* szeged.ful.0.test *)
let emission_order = !emission_order in
let tag_order = !tag_order in
let max_freq_for_guess = !max_freq_for_guess in
let max_suffix_length = !max_suffix_length in
let model_file = !model_file in
	
	
let model = Hmm_tagger.start_train tag_order emission_order in
prerr_endline "reading training corpus";
Io.iter_tagged_sentence chan (Hmm_tagger.add_sentence model);

prerr_endline "compiling probabilities";
Hmm_tagger.calculate_probs model;


prerr_endline "constructing suffix guesser";
Hmm_tagger.build_suffixtries model max_freq_for_guess max_suffix_length;
prerr_endline "saving the model";
Hmm_tagger.save model model_file;

Hmm_tagger.print_stat model;
;;

let _ = main ();
