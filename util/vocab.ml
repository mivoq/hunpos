module SHash = Mfhash.String
module IHash = Mfhash.Int

type t = (int SHash.t * string IHash.t * int ref )

let create () = (SHash.empty (), IHash.empty (),  ref 0)
	
let toindex (word2id, id2word, max) w =
	let id = SHash.find_or_add word2id w  !max in
	if id = !max then begin
		let _ = IHash.find_or_add id2word id w in
		incr max ;
	
		end; 
	id
	
let toword (word2id, id2word, max) idx =
	IHash.find id2word idx
	
let ngram_toindex vocab ngram =
	List.map (toindex vocab) ngram
	
let max (_, _, max) = !max
	
