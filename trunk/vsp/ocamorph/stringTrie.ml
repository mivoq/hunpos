module CharMap = Zip_list.Make( Char )
module Tag = struct 
  type 'a t = 'a list
  let add i j = i :: j
  let empty = []
end

module S = Trie.Make (Tag) (CharMap)
    
