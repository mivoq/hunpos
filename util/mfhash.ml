
(* mikozben iteralsz, ne nyulj hozza! *)
module type S =
  sig
    type key
    type 'a t
	val empty : unit -> 'a t
	val create_from : int -> ((key->'a->unit) -> unit) -> 'a t
  (*	val create : int -> 'a t
	val clear : 'a t -> unit
	val copy :  'a t -> 'a  t
*)	val iter : (key -> 'a -> unit) -> 'a  t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t  -> 'b
	val sorted_iter : (key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit 
    val print_bucket_stat : 'a t -> unit 
	val find : 'a t -> key -> 'a 
	val find_save : 'a t -> key -> 'a
	val find_or_add : 'a t-> key -> (unit -> 'a) -> 'a
	val add_or_replace : 'a t -> key -> 'a -> unit
	val update : 'a  t ->  key -> (unit -> 'a)   -> ('a -> 'a) -> 'a  
	val update_all: 'a t -> (key -> 'a -> 'a) -> unit
	val size : 'a t -> int
  end

module Make(H: Hashtbl.HashedType) : (S with type key = H.t) =
(*: (Amap.S with type key = H.t) = *)
  struct
    type key = H.t


(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type  ('b ) bucketnode =
  {mutable next: ( 'b) bucketlist;
   key : key ;
   mutable value : 'b }
and ( 'b) bucketlist =
    Empty
  | Cons of  ('b) bucketnode
;;

						
type ( 'b) t =
  { mutable size: int;                        (* size *)
    mutable hash_mask: int;				  (* hany bites a hash fgv. *)
    mutable data: ( 'b) bucketlist array } (* the buckets *)

let hash_fun w hash_mask =
(*	Printf.printf "hash %d mask %d res %d \n" (H.hash w) hash_mask (	(H.hash w) land hash_mask);
*)	(H.hash w) land hash_mask


	
let empty () =
	{size = 0;
	 hash_mask = 1;
	 data = Array.make 2 Empty}	

(*
let create size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }
*)

let size h = h.size
	
let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0



let resize tbl =

	
  let odata = tbl.data in
  let osize = Array.length odata in
	(* todo: ellenorizni, hogy van-e meg hely *)
  let nsize = osize lsl 1 in
  let nmask = (tbl.hash_mask lsl 1) + 1 in
  if nsize <> osize then 
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(onode) ->
         (* preserve ordering, insert first the first *)
	
          let nidx = (hash_fun onode.key nmask) in
          let _ = match ndata.(nidx) with
			(* this is empty bucket *)
	 		Empty -> ndata.(nidx) <- 	Cons( {next = Empty; 
										  key = onode.key; 
										  value = onode.value} ) ;
			| Cons(node) ->
				(* insert it to the tail *)
				let rec aux prev  =
					 match prev.next with
					Empty -> prev.next <- Cons ({next = Empty; key = onode.key; value = onode.value});
                    | Cons(nnode) -> aux nnode ;
				in
				aux node ;
		  in
		  insert_bucket onode.next
		  
	in			
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
	tbl.hash_mask <- nmask
else
	Printf.eprintf "not enough memory to resize the hash\n";
 
;;
let iter f h =

  let rec do_bucket = function
      Empty ->
        ()
    | Cons(node) ->
       f node.key node.value; do_bucket node.next in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let update_all h f =

  let rec do_bucket = function
      Empty ->
        ()
    | Cons(node) ->
       node.value <- f node.key node.value; 
	   do_bucket node.next 
  in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

	
let fold f accu h =
 	let accu = ref accu in
	iter (fun k v -> accu := f k v !accu) h;
	!accu
	
let sorted_iter compare f h =

   let add2list l b = 
		let rec aux node res = match node with
			Empty -> res
			| Cons(node) -> aux node.next ( (node.key, node.value) :: res)
		in
		aux b l
	in
	let datalist = Array.fold_left add2list [] h.data in
	 let sdatalist = List.fast_sort (fun (key1,_) (key2,_) -> compare key1 key2) datalist in
	 List.iter (fun (key, value) -> f key value) sdatalist   


let rec bucket2list l = function
		Empty -> l
		| Cons(node) -> bucket2list (node.key :: l) node.next
;;

let print_bucket_stat h =
	let hist = Hashtbl.create 20 in
	let d = h.data in
	begin
	 for i = 0 to Array.length d - 1 do
	     let len = List.length (bucket2list [] d.(i)) in
		 try
		 	incr (Hashtbl.find hist len) ;
		 with Not_found -> Hashtbl.add hist len (ref 1);
	   done ;
	   Hashtbl.iter  (fun k f -> Printf.eprintf "%d\t%d\n" k !f) hist ;
	end

let find h k   =
	  let i = hash_fun k (h.hash_mask) in
	  let l = h.data.(i) in
	  match l with
		| Empty ->  raise Not_found 
		| Cons(node1) -> 
						let rec find_rec nodex = match nodex.next with
	                        | Empty -> raise Not_found
							| Cons(nodexx) ->
								if H.equal k nodexx.key  then
									(
									(* move front the node *)
									nodex.next <- nodexx.next ;
									nodexx.next <- Cons(node1) ;
									h.data.(i) <- Cons( nodexx) ;
									
									nodexx.value
								)
								else
									find_rec nodexx ;
						in
						if  H.equal k node1.key then
							node1.value 
						else 
							find_rec node1
	


let find_save h k   =
	  let i = hash_fun k (h.hash_mask) in
	  let l = h.data.(i) in
	  match l with
		| Empty ->  raise Not_found 
		| Cons(node1) -> 
			let rec find_rec nodex = match nodex.next with
                      | Empty -> raise Not_found
				| Cons(nodexx) ->
					if H.equal k nodexx.key  then
					
						nodexx.value
					else
						find_rec nodexx ;
			in
			if  H.equal k node1.key then
				node1.value 
			else 
				find_rec node1

	
let grow_it h =
	 h.size <- succ h.size;
	 if h.size > Array.length h.data lsl 1 then resize h 
	
let update h   k  init updater  =
	 let i = hash_fun k (h.hash_mask)  in
	 let l = h.data.(i) in
	 match l with
	| Empty ->  let nv = init () in
				h.data.(i) <- Cons( {next = Empty; key = k; value =  nv} ) ;
				grow_it h;
				nv
	
	| Cons(node1) -> 
		let rec update_rec nodex = 
			match nodex.next with
            	| Empty -> let nv = init () in
						   nodex.next <- Cons( {next = Empty;  key = k; value =  nv} ) ;
					   	   grow_it h;
						   nv
			| Cons(nodexx) ->
				if H.equal k nodexx.key  then
					begin
					(* update info *)
					let nv = updater nodexx.value in
					nodexx.value <- (nv) ;
				    (* move front the node *)
					nodex.next <- nodexx.next ;
					nodexx.next <- Cons(node1) ;
					h.data.(i) <- Cons( nodexx) ;
					nv
				end
				else
					update_rec nodexx ;
		in
		if  H.equal k node1.key then
			begin
				(* data is at front *)
				let nv = updater node1.value in
				node1.value <- (nv) ;
				nv
			end
		else 
			update_rec node1
;;

let add_or_replace h k v =
	let _ = update h k (fun () -> v) (fun x -> v) in ()
	
let find_or_add h k init   =
	 let i = hash_fun k (h.hash_mask)  in
	 let l = h.data.(i) in
	 match l with
		| Empty ->  let nv = init () in
					h.data.(i) <- Cons( {next = Empty;  key = k; value = nv} ) ;
					grow_it h ;
					nv
	
	| Cons(node1) -> 
		let rec update_rec nodex = 
			match nodex.next with
                     | Empty -> let nv = init () in
								nodex.next <- Cons( {next = Empty;  key = k; value = nv} ) ;
					   			grow_it h ;
					   			nv
			| Cons(nodexx) ->
				if H.equal k nodexx.key  then
					begin
					let oldvalue =  nodexx.value in
				    (* move front the node *)
					nodex.next <- nodexx.next ;
					nodexx.next <- Cons(node1) ;
					h.data.(i) <- Cons( nodexx) ;
					oldvalue
				end
				else
					update_rec nodexx ;
		in
		if  H.equal k node1.key then
			begin
			(* data is at front *)
			node1.value ;
			end
		else 
			update_rec node1



let create min_size =
	let nsize = ref 2 in
	let nmask = ref 1 in
	while (!nsize < min_size) do
		nsize := !nsize lsl 1;
  		nmask := (!nmask lsl 1) + 1 
	done ;
	{size = 0;
	 data = Array.make  !nsize Empty;
	 hash_mask = !nmask}
;;

let create_from s iter = 
	let h = create s in
	iter (fun k v-> let _ = update h k (fun () -> v) (fun id -> id) in ()) ;
	h
end


module HashedString = struct
	external get_byte: string -> int -> int = "%string_unsafe_get"
	external length : string -> int = "%string_length"
	
	type t = String.t
	let hash w = (* Hashtbl.hash w
		*)
				let h = ref 0 in
				for i=0 to length w -1 do h:= !h + (!h lsl 3) + (get_byte w i) done; 
				!h
		
	let equal (s1:string) (s2:string) = (s1 = s2) 
	let compare (s1:string) (s2:string) = String.compare s1 s2
end ;;

(*module SHash = Make(struct  type t = String.t 
	let hash = Hashtbl.hash
	let equal s1 s2 = (compare s1 s2) = 0 end )
*)
module String = Make(HashedString)

module Char = Make(
		struct 
			type t = char
			let hash = Hashtbl.hash
			let equal (c1:char) (c2:char) = (c1 = c2)
		end
)
	
module Int = Make (struct  type t = int 
	let hash = Hashtbl.hash
	let equal (i1:int) (i2:int) = (i1 = i2) 
	end)
	
	
let _ =
	let lex = String.empty () in
	let incr word = String.update lex word (fun () ->1) (succ)  in
	try
	while(true) do
	  incr (input_line stdin)
	done
	with End_of_file ->
	()