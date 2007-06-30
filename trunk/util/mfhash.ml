

module type S =
  sig
    type key
    type 'a t
	val empty : unit -> 'a t
(*	val create_from : int -> ((key->'a->unit) -> unit) -> 'a t
  *)	 val create : ?move_to_front:bool -> ?do_resizing:bool -> int -> 'a t
(*	val clear : 'a t -> unit
	val copy :  'a t -> 'a  t
*)	val iter : (key -> 'a -> unit) -> 'a  t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t  -> 'b
    val to_list : 'a t -> (key * 'a) list
	val sorted_iter : (key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit 
    val print_bucket_stat : 'a t -> unit 
	val find : 'a t -> key -> 'a 
	val find_or_add : 'a t-> key ->  'a -> 'a
	val add_or_replace : 'a t -> key -> 'a -> unit
	val update : (unit -> 'a)   -> ('a -> 'a) -> 'a  t ->  key ->  'a  
	val update_all: 'a t -> (key -> 'a -> 'a) -> unit
	val size : 'a t -> int
	val array_size : 'a t -> int
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
  { mutable size: int;                     (* size *)
    mutable hash_mask: int;				   (* hany bites a hash fgv. *)
    mutable data: ( 'b) bucketlist array;   (* the buckets *)
    mutable move_to_front : bool;
	mutable do_resizing   : bool;
  }
let hash_fun w hash_mask =
(*	Printf.printf "hash %d mask %d res %d \n" (H.hash w) hash_mask (	(H.hash w) land hash_mask);
*)	(H.hash w) land hash_mask


	
let empty () =
	{size = 0;
	 hash_mask = 1;
	 data = Array.make 2 Empty;
	 move_to_front = false;
	 do_resizing = true;
	}	

let create  ?(move_to_front = false) ?(do_resizing = true) min_size =
  let nsize = ref 2 in
  let nmask = ref 1 in
  while (!nsize < min_size) do
	nsize := !nsize lsl 1;
 	nmask := (!nmask lsl 1) + 1 
  done ;
 {
  size = 0;
  data = Array.make  !nsize Empty;
  hash_mask = !nmask;
  move_to_front = move_to_front;
  do_resizing = do_resizing; 
 }
;;



(*
let create size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }
*)

let size h = h.size
	
let array_size h = Array.length h.data 
	
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
	
let to_list h =
  let add2list l b = 
   let rec aux node res = match node with
      Empty -> res
	| Cons(node) -> aux node.next ( (node.key, node.value) :: res)
	  in
	  aux b l
   in
   Array.fold_left add2list [] h.data 
;;
	
let sorted_iter compare f h =
   let datalist = to_list h in
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

let update init updater h k =


  let grow_it h =
	 h.size <- succ h.size;
	 if  h.do_resizing && h.size > Array.length h.data lsl 1 then resize h 
  in

  (* lookup the bucket list *)
  let i = hash_fun k (h.hash_mask)  in
  let l = h.data.(i) in
  match l with
   | Empty ->  (* the bucket is empty just add the new element *)
	           let nv = init () in
               h.data.(i) <- Cons( {next = Empty; key = k; value =  nv} ) ;
               grow_it h;
               nv
   | Cons(first) -> 
	   if  H.equal k first.key then
         (* data is the first element of the bucket list. Just update it. *)
         let nv = updater first.value in
         first.value <- (nv) ;
         nv
       else 
       let rec update_bucket_list prev = 
         match prev.next with
       | Empty -> let nv = init () in
                  prev.next <- Cons( {next = Empty;  key = k; value =  nv} ) ;
                  grow_it h;
                  nv
       | Cons(cur) ->
                  if H.equal k cur.key  then begin
                    (* update info *)
                    let nv = updater cur.value in
                    cur.value <- (nv) ;
                    (* move front the node *)
					if h.move_to_front then begin
                      prev.next <- cur.next ;
                      cur.next <- Cons(first) ;
                      h.data.(i) <- Cons( cur) ;
                    end;
					nv
                  end else
                    update_bucket_list cur ;
       in
       update_bucket_list first

	
;;
(*
let create_from s iter = 
  let h = create s in
  iter (fun k v-> let _ = update  (fun () -> v) (fun id -> id)  h k in ()) ;
  h
*)
(* ha benne, akkor modositas nelkul visszaadja, ha nincs Not_found *)
let find h k =
	 update (fun () -> raise Not_found) (fun x -> x) h k
	
let add_or_replace h k v =
	let _ = update (fun () -> v) (fun x -> v) h k in ()
	
let find_or_add h k v   =
	update (fun () -> v) (fun x -> x) h k




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
	
	
