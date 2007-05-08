(* module StringTrie = Trie_in_situ.Make( Map.Make( Char ) ) *)
(* module StringTrie = Trie_in_situ.Make( Charmap ) *)

(*s a morph rule is a tuple of 
   tag of type [Tag.analysis]
   a trie node of type [morph StringTrie.t]
   the hidden string that is clipped [string]
   category flag [int]
   continuation flags (a bitvector) checker [int -> bool]
   a character check for match [(char -> bool) list]
 *)

type flags = Bv of bool array | Alt of int list

type chars = bool * string

type morph = 
    Morph of Parser_common.cap * Tag.chunk * StringTrie.S.key * string option * int * flags * (chars option list) 
(*  | FMorph of Parser_common.cap * Tag.analysis * (morph StringTrie.Final.t) * string option * int * flags * (chars option list) *)



