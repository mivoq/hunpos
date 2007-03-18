type t 
val create : unit -> t
val toindex : t -> string -> int
val toword : t -> int -> string
val ngram_toindex : t-> string list -> int list
val max : t -> int

