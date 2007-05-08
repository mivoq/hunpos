exception Too_many_flags

type cap = Lowercase | Allcaps | Capitalized | Mixed of int list | NoCap

let pseudoroot_flag = ref 0 


let normalize_caps x reverse chars char_count = 
  let len = String.length x in
  let cap_f start step stop x = 
    let rec cap_f i = function (cap, count, list, key) as l ->
      if i = stop  
      then l
      else (
	let lc = Char.lowercase x.[i] in
	let code = Char.code lc in
	if chars.(code) = 0 then (
	  incr char_count;
	  chars.(code) <- !char_count;
	  chars.(Char.code x.[i]) <- !char_count;
	 );
	let l' = 
	  if x.[i] = lc then 
	    cap, count, list, lc :: key
	  else (
	    x.[i] <- lc; 
	    let cap = cap || i = 0 in
	    cap, count + 1, i :: list, lc :: key 
	   )
	in
	cap_f (i + step) l'
       )
    in
    cap_f start (false, 0, [], []) 
  in    
  let start, step, stop = 
    if reverse 
    then len - 1, -1, -1
    else 0, 1, len
  in
  let cap, count, list, key = cap_f start step stop x in
  let cap = match cap, count with 
  | true, 1 -> Capitalized (* a single capital letter word 'I' will be Capitalzed rather than Allcaps *)
  | _, 0 -> NoCap (* the empty string will be lowercase *)
  | _, x when x = len -> Allcaps 
  | _, _ -> Mixed list 
  in
  cap, key

let normalize_no_caps x reverse chars char_count = 
  let len = String.length x in
  let cap_f start step stop x = 
    let rec cap_f i = function key  ->
      if i = stop  
      then key
      else (
	  let lc = x.[i] in
	  let code = Char.code lc in
	    if chars.(code) = 0 then (
	  incr char_count;
	  chars.(code) <- !char_count;
	  chars.(Char.code x.[i]) <- !char_count;
	 );
	    cap_f (i + step) (lc :: key)
	)
    in
      cap_f start []
  in    
  let start, step, stop = 
    if reverse 
    then len - 1, -1, -1
    else 0, 1, len
  in
  let key = cap_f start step stop x in
    NoCap, key

let normalize = ref normalize_caps 
		
let get_flag_index flagcount maxflags flaghash x = 
  try 
    Hashtbl.find flaghash x
  with Not_found ->
    incr flagcount; (* flag zero is occupied for free lexemes *)
    if !flagcount >= maxflags then raise Too_many_flags;
    Hashtbl.add flaghash x !flagcount;
    !flagcount


let cap_and = function
  | x, y when x = y -> x
  | x, NoCap | NoCap, x -> x
  | Lowercase, m | m, Lowercase -> NoCap
  | Allcaps, _ | _, Allcaps -> Allcaps
  | Mixed l, Capitalized | Capitalized, Mixed l -> Mixed (1 :: l)
  | Mixed l1, Mixed l2 -> Mixed (l1 @ l2)
(* I include individual matches cause I hate warnings *)
  | Capitalized, Capitalized -> failwith "redundant"

let stringify_cap c = match c with 
    Lowercase -> "Lowercase"
  | NoCap -> "NoCap"
  | Capitalized -> "Capitalized"
  | Allcaps -> "Allcaps"
  | Mixed _ -> "Mixed"

let check_cap x y = 
  (* let xs = stringify_cap x in
  let ys = stringify_cap y in *)
  (cap_and (x, y)) = y (* checks y against constraint x *)
    (* this is not correct since "Mixed" lists are not expected to be equal *)
  || (x = Lowercase && y = NoCap)

let recapitalize string cap = 
  if string = "" then string else
  let _ = match cap with 
  | Capitalized -> string.[0] <- Char.uppercase string.[0]
  | Allcaps -> let len = String.length string in
    for i = 0 to len - 1; do 
      string.[i] <- Char.uppercase string.[i]
    done
  | Mixed l -> 
      let len = String.length string in
      List.iter (fun i -> if i < len then string.[i] <- Char.uppercase string.[i]) l
  | _ -> ()
  in
  string
	
	
 
