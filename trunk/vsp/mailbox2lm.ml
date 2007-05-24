module H = Mfhash.String

(* ilyet keresunk
	#DATE: Thu, 10 May 2007 20:28:58 +0200 (CEST) *)
let is_date line =
	if String.length line < 10 then false else
	line.[0] = '#' && 
	line.[1] = 'D' &&
	line.[2] = 'A' &&
	line.[3] = 'T' &&
	line.[4] = 'E'
	
;;

let freq_table_from_mbox stemmer stop_filter ic =
  let ft = Unigram_lm.empty_freq_table () in
  prerr_endline "building freq table";
  begin
    try
      while(true) do
        let word = input_line ic in
	    if not (is_date word) && stop_filter word then
          Unigram_lm.add_word ft  word;
      done
    with End_of_file -> ();
  end;
  prerr_endline "stemming...";
  Unigram_lm.canonize ft stemmer;
;;

let usage () = 
        Printf.eprintf "usage : %s norm_bin noaccent_bin stopwords  \n" Sys.argv.(0)
;;


let _ = 

if (Array.length Sys.argv) < 4 then 
	let _ = usage () in     exit 1 
else
    let stemmer = Ocastem.stemmer Sys.argv.(1) Sys.argv.(2) in
    let stop_words = Token_filter.load_stop_words Sys.argv.(3) in
	let stop_filter = Token_filter.is_ok stop_words in

	let ft = freq_table_from_mbox stemmer stop_filter stdin in
	Unigram_lm.write_to (stdout) ft;

	
	
	