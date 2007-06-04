let usage () = 
        Printf.eprintf "usage : %s lm1 lm2  \n" Sys.argv.(0)
;;


let _ = 

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in     exit 1 
else
   	let ft1 = Unigram_lm.read_from (open_in Sys.argv.(1)) in
	let ft2 = Unigram_lm.read_from (open_in Sys.argv.(2)) in
	Unigram_lm.add_lm_to_lm ft2 ft1;
	Unigram_lm.write_to (stdout) ft1

	
	
