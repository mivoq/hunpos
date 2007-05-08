let usage () = 
        Printf.eprintf "usage : %s bin_accent bin_noaccent \n" Sys.argv.(0)
;;

let _ = 

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in     exit 1 
else
let stemmer = Ocastem.stemmer Sys.argv.(1) Sys.argv.(2) in
let rec loop () =
	let word =  (input_line stdin) in
	print_string word;
	List.iter (fun s -> print_char '\t'; print_string s) (stemmer word);
	print_newline ();
	loop()
in
try loop () with End_of_file -> ()

	