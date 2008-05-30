(* this  function is called from the c++ wrapper; it simplifies the interface *)

let init model_file morph_table_file  max_guessed_tags theta = 

   
  let hunmorph = 
    if morph_table_file = "" then 
      (* using no morphtable *)
      (fun (s:string) -> raise Not_found)
    else
      let m = Morphtable.load morph_table_file in
      Morphtable.tags m
  in
  
  let model = Hmm_tagger.load model_file in
  let tagger = Hmm_tagger.compile_tagger  model hunmorph  max_guessed_tags (log (float_of_int theta)) in
  let tagger tokens = 
    (* the first tagger returns (observation, tag) list
       in this version we simplify everything and returning string list *)
    let (_, tags) = tagger tokens in
    
    tags 
  in
  tagger
;;

let _ = Callback.register "init_from_files" init


