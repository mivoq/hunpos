
module Make (M : Amap.S)  = struct
	(* : (Amap.S with type key = M.key list)  *)
type key = M.key list 
	
type 'a t = {mutable value:  'a option;
				childs       :   ('a t) M.t} 
				
let empty () = {value = None ; childs = M.empty ()}
	

let rec find_node t l  = match (l,t.value) with
  | [],  None   -> raise Not_found
  | [], (Some v) -> t
  | x::r, _    -> find_node  (M.find t.childs x ) r

let value t = match t.value with
	Some v -> v
	| None -> raise Not_found
	
	
	
	 	
let find t l = 
	value (find_node t l)
	
let find_save = find
(* ha l - t megtalálja, akkor update v egyebkent beteszi default ertekkel *)
let rec update t l  init updater =

	let rec ins t l = match (l, t.value) with
		| [], None -> let nv = init () in
					  t.value <- Some nv; 
					  nv
		| [], Some v -> let nv = updater v in
						t.value <- Some (nv) ; 
						nv 
		| x::r, _ ->
            let child = M.find_or_add t.childs x  (empty)  in
            ins child r
	in
	ins t l


let find_or_add t l def =
		update t l def (fun id -> id)
	
let iter f t =
 	let rec traverse revp t = match t.value with
     | None -> M.iter (fun x -> traverse (x::revp)) t.childs
     | Some v-> f (List.rev revp) v; 
			    M.iter (fun x t -> traverse (x::revp) t) t.childs
   in
   traverse [] t


end