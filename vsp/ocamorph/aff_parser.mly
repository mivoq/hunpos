%{
 
module G = Constraint
(*
let flag_f = 
  let flag_f_ref = ref None in
  fun () ->
    match !flag_f_ref with 
    | None ->
	let f1 = !Parser_common.pseudoroot_flag in
	let f2 = !Parser_common.lemma_present in
	let f = Constraint.Alt [f1;f2] in
	flag_f_ref := Some f;
	f
    | Some f -> f
	 *)
%}

%token <string> AFFIX BOOL 
%token <int> FLAG CLIP COUNT
%token EOL EOF
%token <Constraint.morph StringTrie.S.t ref * StringTrie.S.key * StringTrie.S.key * string option * (Constraint.chars option list) * Parser_common.cap> COND
%token <string*Constraint.flags> NODEINFO
%token <Tag.chunk> TAG
%start main
%type <unit> main
%%
  main:
| EOF {  }
| rule EOL main {}
  ;
  rule:
| AFFIX FLAG CLIP NODEINFO COND  { 
  let flag = $2 in
  let _,flags = $4 in 
  (* this should contain capitalization info which should be counted with *)
  let trie, key, skip, clip, char_constraints, cap = $5 in
  let tag = Tag.top in (* the most uninformative tag e.g., "" or <> *)
  let morph = Constraint.Morph( cap, tag, skip, clip, flag, flags, char_constraints ) in
  trie := StringTrie.S.add key morph !trie
}
| AFFIX FLAG CLIP NODEINFO COND tag { 
  let flag = $2 in
  let _, flags = $4 in 
  let trie, key, skip, clip, char_constraints, cap = $5 in
  let tag = $6 in
  let morph = Constraint.Morph( cap, tag, skip, clip, flag, flags, char_constraints ) in
  trie := StringTrie.S.add key morph !trie
}
    ;
tag:
| tag TAG { Tag.add_field $1 $2 }
| TAG { $1 }
;


