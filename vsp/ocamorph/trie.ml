module type TagType = 
  sig
    type 'a t
    val add : 'a -> 'a t -> 'a t
    val empty : 'a t
  end
      
module type ArcsType = 
  sig 
    type key
    type 'a t
    type 'a zip
    val build : key -> 'a -> 'a t
    val find : key -> 'a t -> 'a
    val zip_add : 'a -> 'a zip -> 'a zip
    val zip_up : 'a zip -> 'a t
    val zip_down : key -> 'a t -> 'a option * 'a zip
    val empty : 'a t
  end
      
module Make 
  (T: TagType)  
    (A: ArcsType) = 
  struct
    
    type key = A.key list
    type 'a tag = 'a T.t
    type 'a t = Node of 'a tag * 'a arcs 
    and 'a arcs = ('a t) A.t
    and 'a zipper = Top | Zip of 'a tag * ('a t) A.zip * 'a zipper
    and 'a zip = Location of 'a zipper * 'a t | Virtual of key * 'a zipper
	
    let arcs_find = A.find
    let arcs_zip_add = A.zip_add
    let arcs_zip_up = A.zip_up
    let arcs_zip_down = A.zip_down
    let arcs_build = A.build
    let empty_arcs = A.empty

    let tag_add = T.add
    let empty_tag = T.empty
	
    let empty = Node(empty_tag, empty_arcs)

    let rec build key tag = match key with 
    | [] -> Node(tag_add tag empty_tag,empty_arcs)
    | head :: tail -> 
	let trie = build tail tag in
	  let arcs = arcs_build head trie in
	  Node(empty_tag, arcs)
	    
    let rec zip_up = function 
      | Location ( Top, trie ) -> trie
      | Location ( Zip (tag, zipped_arcs, parent), trie ) ->
	  zip_up ( Location ( parent, Node (tag, arcs_zip_up (arcs_zip_add trie zipped_arcs)) ))
      | Virtual _ -> failwith "you can't zip up a virtual node"
	  
    let rec zip_down_rec key = function 
      | Location (zip, (Node(tag,arcs) as trie)) -> (
	  match key with
	  | [] -> Some tag, Location (zip, trie)
	  | head :: tail -> 
	      let found, zipped_arcs = arcs_zip_down head arcs in
	      match found with 
	      | Some trie -> 
		  zip_down_rec tail 
		    (Location ( (Zip ( tag, zipped_arcs, zip) ), trie ))
	      | None -> 
		  None, ( Virtual ( tail, ( Zip ( tag, zipped_arcs, zip ) )))
	 )
      | Virtual ( l, zip ) -> None, ( Virtual ( l @ key , zip ) )
	      
    let zip_down key trie = zip_down_rec key (Location (Top, trie))

    let zip_add tag = function 
      | Location (zip, Node( tags, arcs )) -> 
	  Location (zip, Node( tag_add tag tags, arcs) ) 
      | Virtual (key, zip ) -> 
	  Location (zip, build key tag)

    let add key tag trie = 
      let _, zip = zip_down key trie in
      let zip = zip_add tag zip in
      zip_up zip
    
    let rec node_find list = function Node(_, map) as trie ->
      match list with
      | [] -> trie
      | hd :: tl -> 
	  let trie = arcs_find hd map in
	  node_find tl trie

    let rec find list = function Node(l, map) (* as trie *) ->
      match list with
      | [] -> l
      | hd :: tl -> 
	  let trie = arcs_find hd map in
	  find tl trie
	    
    let morphs node = match node with Node (l,_) -> l
	
    let compaction _ = []

    let mfind key = function Node(_,arcs) -> arcs_find key arcs
  end
    
