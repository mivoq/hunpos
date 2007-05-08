%{
 
module G = Constraint

module MorphHash = Hashtbl.Make(struct type t = Parser_common.cap * Tag.chunk * Constraint.flags let equal (c,t,f) (c', t',f') = c = c' && t == t' && f == f' let hash = Hashtbl.hash end)

let morph_hash = ref None

let get_morph_hash () =
  match !morph_hash with Some(x) -> x 
  | None -> let hash = MorphHash.create 100001 in
    morph_hash := Some(hash); 
    hash
       
let reset_morph_hash () = 
  match !morph_hash with None -> () | Some(hash) ->
    Utils.carp 1 "%d morphs\n" (MorphHash.length hash);
    morph_hash := None
	  
let count = ref 0

let insert morphs = function Constraint.Morph( cap, tag, node, clip, flag, flags, char_constraints ) as morph ->
  let morph_hash = get_morph_hash () in
  let key = cap, tag, flags in
  let morph = 
    try 
      incr count;
      MorphHash.find morph_hash key 
    with Not_found ->
      MorphHash.add morph_hash key morph;
      Utils.carp 2 "%d\n" !count;
      morph
  in
  morphs := morph :: !morphs
	
let flag_f = 
  let flag0 = ref None in
  fun () ->
    match !flag0 with Some(x) -> x
    | None ->
	let f = Constraint.Alt [!Parser_common.pseudoroot_flag] in
	flag0 := Some f;
	f

let global_empty_tag = ""
let global_empty_set = []

%}
%token EOL EOF
%token <Constraint.morph StringTrie.S.t ref * StringTrie.S.key * StringTrie.S.key * Parser_common.cap * Constraint.flags> NODEINFO
%token <Tag.chunk> TAG
%start main
%type <unit> main
%%
  main:
| EOF { reset_morph_hash () }
| rule EOL main {}
  ;
  rule:
| NODEINFO tag { 
  let (trie, key, skip, cap, flags) = $1 in
  let tag = $2 in
  let tag = if tag = "" then global_empty_tag else tag in
  let clip = None in
  let flag = 0 in (* lexes belong to flag 0 group by default *)
  let char_constraints = global_empty_set in
  let morph = Constraint.Morph( cap, tag, skip, clip, flag, flags, char_constraints ) in
  trie := StringTrie.S.add key morph !trie
}
| NODEINFO { 
  let (trie, key, skip, cap, flags) = $1 in
  let tag = global_empty_tag in
  let clip = None in
  let flag = 0 in (* lexes belong to flag 0 group by default *)
  let char_constraints = global_empty_set in
  let morph = Constraint.Morph( cap, tag, skip, clip, flag, flags, char_constraints ) in
  trie := StringTrie.S.add key morph !trie
}
    ;
tag:
| tag TAG { $1 ^ $2 }
| TAG { $1 }
;






