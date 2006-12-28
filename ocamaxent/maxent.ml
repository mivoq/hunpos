type maxent_model (* The type remains abstract *)

external create: unit -> maxent_model = "maxent_new"
external add_event: maxent_model-> string list -> string -> int -> unit = "maxent_add_event"
external train: maxent_model->unit = "maxent_train"
external save: maxent_model->string->unit = "maxent_save"
external load: string->maxent_model = "maxent_load"
external eval: maxent_model->string list -> string -> float = "maxent_eval"
external eval_all:maxent_model->string list->(string*float) list ="maxent_eval_all"

let sort_outcomes outcomes = 
	let comp (o1,( p1:float)) (o2, (p2:float)) = 
		if p1 < p2 then 1 
		else if p2 < p1 then -1 
		else 0
	in	
	List.sort comp outcomes
