
type chunk = string
type analysis = chunk list * chunk list * int list * int list

let empty_clip = ("","")
let clip_right (l,_) = function None -> (l,"") | Some t -> (l,t)
let clip_left (_,r) = function None -> ("",r) | Some t -> (t,r)
let clip_print (l,r) = l ^ r
let empty = ([],[],[],[])

let top = ""

let print_tag delim = 
  fun (left, right, _, _)  ->
  String.concat delim (List.rev_append left right)

let print_tag_dot = print_tag "."

let print_pos delim = 
  fun (_, _, leftpos, rightpos) ->
  String.concat delim 
    (List.rev_map 
	(string_of_int)
	(List.rev_append rightpos leftpos)
    )

let (print_pos_dot:string list * string list * int list * int list -> string ) = print_pos "."

let print_debug t = 
  (print_tag_dot t)
  ^ "\t" 
  ^ (print_pos_dot t)

let (print:string list * string list * int list * int list -> string ) = print_tag ""



let join_segmentation seg_delim seg_delim_left seg_delim_right =
  let join_segmentation string (left, right, leftpos, rightpos) =
    
    let intervals (prev, tags, bag) pos tag =
      if pos > prev  then 
	let segment = String.sub string prev (pos - prev) in
	let segment = seg_delim_right :: segment :: seg_delim_left :: [seg_delim] in
	  pos
	    , (if bag = [] then segment :: tags else bag :: segment :: tags)
		, tag :: [seg_delim]
      else
	if pos = -1 then 
	prev, tags, (tag :: bag)
      else
	if tag = "+" then
	prev, (bag :: tags), (tag :: [seg_delim])
      else prev, tags, (tag :: seg_delim :: bag)
    in
      (*  (4,2,0...) *)
    let foldrightintervals = 
      let f a b c = intervals c a b in
	f
    in
    let prev, tags, bag = 
      List.fold_right2 (foldrightintervals) leftpos left (0, [],[]) 
    in 
    let a = if tags = [] then 
	(prev, tags, bag) else
	intervals (prev, tags, bag) prev "+"  
    in
    (*  (1,1,6,8,11...) *)
    let a = 
      List.fold_left2 (intervals) a rightpos right
    in
    let len = String.length string in 
    let _, tags, _ = intervals a len "+" in

    let f l1 l2 = List.rev_append l2 l1 in
    let a = String.concat "" (List.fold_left (f) [] tags ) in
      a
  in
    join_segmentation
    

let print_segmentation = join_segmentation
let bottom = "unknown"



let add_tag_right tag (left, right, leftpos, rightpos) = (left, tag :: right, leftpos, rightpos)
let add_tag_left tag (left, right, leftpos, rightpos) = (tag :: left, right, leftpos, rightpos)
let add_pos_right pos (left, right, leftpos, rightpos) = (left, right, leftpos, (pos + 1) :: rightpos)
let add_pos_left pos (left, right, leftpos, rightpos) = (left, right, pos :: leftpos, rightpos)

let add_right tag rtag rpos = 
  add_pos_right rpos (add_tag_right rtag tag)
let add_left tag ltag lpos = 
  add_pos_left lpos (add_tag_left ltag tag)

let add_clip (l,r) tag = 
  let tag = match l with
      "" -> tag 
    | _ -> add_left tag l (-2)
  in
  let tag = match r with
      "" -> tag 
    | _ -> add_right tag r (-2)
  in
    tag

let add_field = (^)
  
