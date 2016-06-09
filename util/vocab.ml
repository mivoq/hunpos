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

let fix_old_vocab (_, old_id2word, max) =
  let (word2id, id2word, _) = create () in
  let add (id, word) =
    IHash.add_or_replace id2word id word;
    SHash.add_or_replace word2id word id;
  in
   List.map add (IHash.to_list old_id2word);
   (word2id, id2word, max)
