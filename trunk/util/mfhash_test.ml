module H = Mfhash.String
let usage_and_exit () =
	Printf.eprintf "%s move_to_front do_resizing init_size\n" Sys.argv.(0);
	exit(-1)
	
let _ =
	let start = Sys.time () in
	if Array.length Sys.argv < 4 then usage_and_exit () ;
	try
	  let move_to_front = match Sys.argv.(1) with "true" -> true | "false" -> false | _ -> usage_and_exit () in
	  let do_resizing =  match Sys.argv.(2) with "true" -> true | "false" -> false | _ -> usage_and_exit () in
	  let size = int_of_string Sys.argv.(3) in
	let lex = H.create ~move_to_front:move_to_front ~do_resizing:do_resizing (1 lsl size)  in
	let incr  = H.update (fun () ->1) (succ)  lex   in
	try
	while(true) do
	  let _ = incr (input_line stdin) in ()
	done
	with End_of_file ->
	let stop =Sys.time () in
	
	Printf.printf "%b %b %d %f %d %d\n" move_to_front do_resizing size (stop -. start) (H.size lex) (H.array_size lex)
	
	with _ -> usage_and_exit ()