module type S =
  sig
    type key 
    type 'a t
    val empty: unit -> 'a t
	val find : 'a t -> key -> 'a 
	val find_or_add : 'a t-> key ->  'a -> 'a
	val add_or_replace : 'a t -> key -> 'a -> unit
	val update : (unit -> 'a)   -> ('a -> 'a) -> 'a  t ->  key ->  'a
	val iter :  (key -> 'a -> unit) -> 'a t ->  unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
	val update_all: 'a t -> (key -> 'a -> 'a) -> unit
	val size : 'a t -> int
  end