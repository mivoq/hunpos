module type S =
  sig
    type key 
    type 'a t
    val empty: unit -> 'a t
	val update : 'a t -> key -> (unit -> 'a) -> ('a -> 'a) -> 'a
	val find_or_add : 'a t -> key -> (unit -> 'a) -> 'a
	val find : 'a t -> key -> 'a
	val find_save : 'a t -> key -> 'a
	val add_or_replace : 'a t -> key -> 'a -> unit
	val iter :  (key -> 'a -> unit) -> 'a t ->  unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
	val update_all: 'a t -> (key -> 'a -> 'a) -> unit
  end