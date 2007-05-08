module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make (O: OrderedType) = struct
  
  type key = O.t
  type 'a t = (key * 'a) list
  type 'a zip = Zip of (key * 'a) list * key * ( (key * 'a) list )

  let build key value = (key, value) :: []

  let find = List.assoc 

  let zip_down key list = 
    let rec zip_down_rec left = function 
      | [] -> None, Zip (left, key, [])
      | (key', value) as pair :: tail as right -> 
	  let diff = O.compare key key' in
	  if diff < 0 
	  then zip_down_rec (pair :: left) tail
	  else if diff = 0 then Some value, Zip (left, key, tail)
	  else None, Zip (left, key, right)
    in
    zip_down_rec [] list

  let zip_up = function 
    | Zip (left, _, right) -> 
	let rec zip_up_rec right = function 
	  | [] -> right
	  | head :: tail -> zip_up_rec (head :: right) tail
	in
	zip_up_rec right left

  let zip_add value = function 
    | Zip (left, key, right) -> Zip (left, key, (key, value) :: right)

  let add key value l =
    let _, zip = zip_down key l in
    let zip = zip_add value zip in
    zip_up zip

  let empty = []

end
