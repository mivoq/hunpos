module Shash = Mfhash.String

type freq_table = {mutable n : int; freq_table : int Shash.t}


let split c str =
  let rec aux acc idx =
    try 
      let idx' = String.index_from  str idx c in
      aux  ((String.sub str idx (idx' - idx))::acc) (succ idx')
    with Not_found -> List.rev ((String.sub str idx (String.length str - idx)) :: acc)
  in
  aux [] 0


let empty_freq_table () =
	{n = 0; freq_table = Shash.empty ()}
	
let add_word ft word =
	ft.n <- ft.n + 1;
	let _ = Shash.update (fun () -> 1) (succ) ft.freq_table word  in ()

(* hozzaadja lm2-t lm1-hez. Vigyazz! Lm1-t megvaltoztatja *)
let add_lm_to_lm lm2 lm1 =
  lm1.n <- lm1.n + lm2.n;
  let add_word_to_lm1 word freq =
	let _ =  Shash.update (fun () -> freq) (fun x -> x + freq) lm1.freq_table word  in
	()
  in
  Shash.iter add_word_to_lm1 lm2.freq_table
	
	
(* visszaad egy uj ft-t, amiben az eredeti ft szavai vannak, athajtva f fuggvenyen.
	stemmelon *)
let canonize ft f =
	let new_table = Shash.empty () in
	let add_word w freq = 
		let stem = f w in
		let _ = Shash.update (fun () -> freq) (fun x -> x + freq) new_table stem
		in ()
	in
	Shash.iter (add_word) ft.freq_table;
	{n = ft.n; freq_table = new_table}
	
	
(* a fajl bemenete egy szo egy sor *)
let freq_table_from_tokenized_file f file =
	let ic = open_in file in
	let ft = empty_freq_table () in
	let _ = try
		while(true) do
			let word = input_line ic in
			let word = f word in
			add_word ft word;
		done
	with End_of_file -> () in 
	ft
	
let write_to oc ft =
	Printf.fprintf oc "%d\n"  ft.n;
	Printf.fprintf oc "%d\n"  (Shash.size ft.freq_table);
	Shash.iter (fun word freq -> Printf.fprintf oc "%s\t%d\n" word freq) ft.freq_table
	
	
let read_from ic =
	let ft = Shash.empty () in
	let n = int_of_string (input_line ic) in
	let types = int_of_string (input_line ic) in
	begin
	try
	  while(true) do
	    let line = input_line ic in
		match split '\t' line with
			[word; freq] -> Shash.add_or_replace ft word (int_of_string freq);
		  |	_            -> failwith ("not valid line: " ^ line)
	  done
	with End_of_file -> ()
    end;
	if types != (Shash.size ft) then failwith ("not enough lines in the file");
	{n = n; freq_table = ft}
	
let freq  ft w =
	Shash.find ft.freq_table w;;
			
(* log g_t (w) - log g_l (w) -t szamol minden olyanra, ami benne van a topicban. 
   ft_language-ben minden szo benne van, ami ft_topic-ban, ilyen a hatternyelvmodell
 *)
let calc_relevance ft_topic ft_language  =
	let prob_table = Shash.empty () in
	(* vegig megyunk ft2-n *)
	let log_topic_n = log (float_of_int ft_topic.n) in
	let log_language_n = log (float_of_int ft_language.n) in
		
	let for_each_topic_word word freq =
		let g_t = log (float_of_int freq) -. log_topic_n in
		let f_l = try Shash.find ft_language.freq_table word with Not_found -> 0 in
		(* TODO: itt elszallhat, ha rosszul raktad ossze a hattermodellt !*)
		let g_l = log( (float_of_int f_l)) -. log_language_n in
		let rel =  (g_t -. g_l) in
		Shash.add_or_replace prob_table word rel;
	in
	Shash.iter (for_each_topic_word) ft_topic.freq_table;
	prob_table