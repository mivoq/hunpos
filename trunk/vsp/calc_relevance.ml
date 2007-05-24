module Shash = Mfhash.String
let usage () = 
        Printf.eprintf "usage : %s background topic  \n" Sys.argv.(0)
;;

let first_n n l =
  let rec aux n l acc =
    if n < 0 then acc else
	match l with
	    h::t -> aux (n-1) t (h::acc)
	  | _    -> acc
  in
  aux n l []
	
let _ = 

if (Array.length Sys.argv) < 3 then 
	let _ = usage () in     exit 1 
else
   	
	let ft_lang = Unigram_lm.read_from (open_in Sys.argv.(1)) in
	let ft_topic = Unigram_lm.read_from (open_in Sys.argv.(2)) in
	let rels = Unigram_lm.calc_relevance ft_topic ft_lang in
	let rels = Shash.to_list rels in
	let rels = List.fast_sort (fun (key1,val1) (key2,val2) -> compare val2 val1) rels in
    let rels = first_n 2000 rels in
	let rels = List.map (fun (w, rel) -> (w,(float_of_int (Unigram_lm.freq ft_topic w)) *. rel)) rels in
	List.iter (fun (key, w) -> Printf.printf "%s\t%f\n" key w) rels