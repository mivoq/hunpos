type maxent_model (* The type remains abstract *)

external create: unit -> maxent_model = "maxent_new"
external add_event: maxent_model-> string list -> string -> int -> unit = "maxent_add_event"
external train: maxent_model->unit = "maxent_train"
external save: maxent_model->string->unit = "maxent_save"
external load: string->maxent_model = "maxent_load"
external eval: maxent_model->string list -> string -> float = "maxent_eval"
external eval_all:maxent_model->string list->(string*float) list ="maxent_eval_all"

let _ =
	let m = create () in
	add_event m ("haps"::"hups"::[]) "NOUN" 2;
		add_event m ("haps"::"hips"::[]) "VERB" 2;
	let _ = train m in
	save m "vacak";
	let m = load "vacak" in
	let c = "hups"::"hips"::[] in
	print_float (eval m c "NOUN"); print_newline ();
	let fl = eval_all m c in
	List.iter (fun (s,p) -> Printf.printf "%s %f\n" s p ) fl;
	
