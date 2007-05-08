let debug_level = ref 0

let round f = 
  let fra,int = modf f in 
  let i = int_of_float int in
  i + (if fra > 0.5 then 1 else 0)

let dw x = assert ( x; true  )

let lazy_carp l a = 
  if !debug_level >= l then Lazy.force a else ()

let carp l a = 
  let helper f = 
    if !debug_level >= l then
      Printf.eprintf "%s" f
    else () 
  in
  Printf.kprintf helper a

let open_in_channel data_file_name =    
  try
    if data_file_name = "-" then stdin else open_in data_file_name
  with Sys_error s as e -> carp 0 "Sys error: %s\nUtils: unable to open file '%s' for reading\n" s data_file_name; 
    raise e
	    
let open_out_channel data_file_name =    
  try
    if data_file_name = "-" then stdout else open_out data_file_name
  with Sys_error s as e -> carp 0 "Sys error: %s\nUtils: unable to open file '%s' for writing\n" s data_file_name; 
    raise e
      
let rec seq i j = 
  if i = j then [j] else i :: (seq (succ i) j)

let rec cond_rev_append f l a = 
  match l with [] -> a
  | hd :: tl -> cond_rev_append f tl 
      (if f hd then hd :: a else a)
	  
let fold_left f init list = 
  let func x y = f y x in
  List.fold_left (func) init list

let fold_on_powerset apply init list = 

  let apply' x (res, subsetlist) subset = 
    let newsubset = x :: subset in
    let res = apply newsubset res in
    res, ( newsubset :: subsetlist)
  in
  let apply'' (res, subsetlist) x = 
    List.fold_left (apply' x) (res, subsetlist) subsetlist
  in
  (* emptyset will be in the powerset but the function is not applied to it *)
  let res, powerset = List.fold_left (apply'') (init, [[]]) list in
  res

  

