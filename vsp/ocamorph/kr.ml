
type snode = {
    mutable store : ( ( string * ( int * snode ) ) list ) ;
    mutable cnt : int;
    mutable name : string;
  }
      
type signature = {
  mutable signature : ( (string * snode) list );
}

type bv = int

type t = Node of bv * snode * (int * t) list | Empty | Lemma of string * t

exception Buttom

exception Impossible

let snode_make () = 
  { 
    store = [];
    cnt = 0 ;
    name = "";
  }  

let snode_add snode string = 
  try List.assoc string snode.store with Not_found -> 
    let new_snode = snode_make () in
    new_snode.name <- string;
    let cnt = snode.cnt in
    snode.store <- ( string, (cnt, new_snode)) :: snode.store;
    snode.cnt <- succ cnt; 
    cnt, new_snode

let snode_get snode string = 
  try List.assoc string snode.store with Not_found -> 
    raise Buttom

let sig_make () = { signature = []; }

let sig_add s key = 
  try List.assoc key s.signature with Not_found -> 
    let snode = snode_make () in
    s.signature <- (key, snode) :: s.signature;
    snode

exception Undefined_signature

let sig_get s key = (
  if key = "" 
  then match s.signature with (_, default) :: _ -> default 
  | [] -> raise Undefined_signature 
  else try 
    List.assoc  key s.signature 
  with Not_found -> 
    raise Undefined_signature
 )
    

let bv_and = (land)
let bv_or  = (lor)

let bv_all_one = max_int
let bv_all_zero = 0

let bv_get bv i = 
  let s = ( (1 lsl i) land bv <> 0 ) in 
  (* Printf.eprintf "bv: %d, i:%d bool: %b\n" bv i s; *)
  s
let bv_set bv i = (1 lsl i) lor bv 

let top snode = Node ( bv_all_one, snode, [] )

let name = function 
  | Node(_, snode, _) -> snode.name | Empty -> "bottom" | _ -> "noname"

let unify_par use_underspec a b = 
  let bv_comb = (if use_underspec then bv_and else bv_or) in
  let nonnull bv i = 
    if use_underspec 
    then (if bv_get bv i then true else raise Buttom)
    else true
  in
  let rec unify_rec = function
    | Node(bv_a, snode_a, subtree_a), Node(bv_b, snode_b, subtree_b) ->
	if snode_a <> snode_b then raise Buttom
	else (
	  let bv = bv_comb bv_a bv_b in 
	  let rec merge = function
	    | [], [] -> []
		  
	    | (i,_) as hd_a :: tail_a, [] when nonnull bv i
	      -> hd_a :: merge ( tail_a, [] ) (* a specifies a node, b underspecifies *)		
			  
	    | (i,_) as hd_a :: tail_a, ( (j, _) :: _ as l_b) when i < j && nonnull bv i
	      -> (* prerr_string "a specifies a node, b underspecifies";*)
		hd_a :: merge ( tail_a, l_b ) (* a specifies a node, b underspecifies *)		
			  
			  
	    | [],           ( (i,_) as hd_b ) :: tail_b when nonnull bv i
	      -> hd_b :: merge ( [], tail_b ) (* b specifies a node, a underspecifies *)
			  
	    | (j, _) :: _ as l_a , ( (i,_) as hd_b ) :: tail_b when i < j && nonnull bv i
	      -> 
		(* prerr_string "b specifies a node, a underspecifies\n";*)
		hd_b :: merge ( l_a, tail_b ) (* b specifies a node, a underspecifies *)
			 
	    | (i, node_a) :: tail_a, (j, node_b) :: tail_b (* when i = j *)
	      ->
		(* both a and b specifies the node: recursion on unify *)
		(* assume that it is well-formed so skip checking ith bit *)
		let node = unify_rec ( node_a,  node_b ) in
		(i, node) :: merge ( tail_a, tail_b )
	    | _, _ -> raise Impossible
	  in
	  let subtree = merge ( subtree_a, subtree_b ) in
	  Node( bv, snode_a, subtree )
	 )
    | _, _ -> raise Impossible (* this shouldn't happen *) 
  in
  try unify_rec ( a, b ) with e -> raise e
   (* with Buttom -> Empty *)

let unify = unify_par true
let join  = unify_par false
      
let rec tabs i = 
  if i = 0 then "" else "\t" ^ tabs (pred i)

let pretty_print = function 
  | Node(_,_,_) as n ->   
      let rec pretty_print_rec depth = function 
	| Node(_, snode, subtree) ->   
	    let apply sofar (ind, node)  = 
	      (tabs depth) ^ (name node) ^ "\n" 
	      ^ (pretty_print_rec (succ depth) node) 
	      ^ sofar
	    in
	    List.fold_left (apply) "" subtree
	| _ -> raise Impossible
      in
      pretty_print_rec 0 n
  | _ -> "bottom"
	
let print = function 
  | Node(_,_,_) as n ->
      let rec print_rec = function 
	| Node(_, snode, subtree) ->
	    let apply sofar (int, node) = 
	      "<" ^ (name node) ^ (print_rec node) ^ ">" ^ sofar
	    in
	    List.fold_left (apply) "" subtree
	| _ -> raise Impossible
      in
      print_rec n
  | _ -> "-"

