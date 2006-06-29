
let rec add_to_trie t sentence = 
	let add_pair_to_trie t (word, gold) =
		Trie.add t word
	in
	match sentence with
        | (h::tail) -> add_pair_to_trie (add_to_trie t tail) h
		| [] -> t
in	
let chan = open_in "szeged.ful.0.test" in
	let rec build_trie t =
		try 
		let sentence = Io.read_sentence chan in
		build_trie (add_to_trie t sentence)
		with End_of_file -> t
	in
		let trie = build_trie Trie.empty in

			Trie.print_all trie;
			
			