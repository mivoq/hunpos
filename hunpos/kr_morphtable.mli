type t 
type parsed_anal = (string * string * string * string) list list
val load : string -> t
val tags : t -> string -> string list
val analyze : t -> string -> parsed_anal * bool