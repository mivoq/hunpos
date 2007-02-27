module SHash = Mfhash.String

let tokens = ref 0 
let sentences = ref 0 
let lex = SHash.empty() 
let clength = ref 0 
let syllables = ref 0 
let hard_words = ref 0
let hard_word_limit = 3 
let verbs = ref 0 
let past_verbs = ref 0
let nouns = ref 0 
let rx_verb = Str.regexp "VERB" 
let rx_pers1 = Str.regexp "<PERS<1>>"
let rx_pers2 = Str.regexp "<PERS<2>>"
	
let rx_plur = Str.regexp "<PLUR>"
let rx_past = Str.regexp "PAST"
	
let rx_noun = Str.regexp "NOUN"

let verb_szamszem = Array.make_matrix 2 3 0
	
let is_vowel c = match Char.lowercase c with
    'a' | 'e' | 'i' | 'o' | 'u' | 'á' | 'é' | 'û' | 'ú' | 'õ' | 'ó' | 'ü' | 'ö' | 'í' -> true
|	_ -> false
;;

let matching r s  =
	try
	let ix = Str.search_forward r s 0 in
	true
	with Not_found -> false
;;

let syllable_count s =
	let c = ref 0 in
	for i = 0 to String.length s do
		if is_vowel s.[i] then incr c
	done;
	!c
	
let calc_sentence sent =
	tokens := !tokens + (List.length sent);
(*	if (List.length sent) > 77 then
	begin
		Printf.printf "%d\n" (List.length sent);
		List.iter (fun (word::t) -> print_string word; print_char ' ') sent;
		Printf.printf "\n";
	end;
*)	let update_word line = 
		let (word::tag::stem::t) = line in
		clength := !clength + (String.length stem);
		let sc = syllable_count stem in
		if sc >= hard_word_limit then
			incr hard_words
		;
        if (matching rx_verb tag) then 
		begin
			incr verbs;
			let szem =
				if matching rx_pers1 tag then 1 
				else if matching rx_pers2 tag then 2
				else 3
			in
			let szam = 
				if matching rx_plur tag then 2 else 1
			in
			verb_szamszem.(szam - 1).(szem - 1) <- verb_szamszem.(szam -1 ).(szem-1) + 1;
			
			if (matching rx_past tag ) then 
				incr past_verbs;
			
		end; (* verbs *)
			
		if (matching rx_noun tag) then
		begin
			incr nouns;
		end;
		syllables := !syllables + sc; 
		let _ = SHash.update lex stem (fun () -> 1) (succ) in () 
	in
	List.iter update_word sent;
	incr sentences;
;;


Io.iter_fielded_sentence stdin (calc_sentence) ;

let percent x y =
	(float x) /. (float y) *. 100.
in
	
let types = SHash.size lex in
Printf.printf "tokens: %d\n" !tokens;
Printf.printf "types: %d\n" types;
Printf.printf "sentences: %d\n" !sentences;
Printf.printf "avg sentence length: %8.2f\n" ((float !tokens) /. (float !sentences));
Printf.printf "types/tokens: %f\n" ((float types) /. (float !tokens));
Printf.printf "avarage word length: %f\n" ((float !clength) /. (float !tokens)) ;
Printf.printf "hard words: %f\n" ((float !hard_words) /. (float !tokens));
let gunning = (((float !hard_words) /. (float !tokens)) +. ((float !tokens) /. (float !sentences))) *. 0.4 in
Printf.printf "gunning fog index: %f\n" gunning;
Printf.printf "\n";
Printf.printf "verbs: %5.2f\n" (percent !verbs !tokens);
Printf.printf "past verbs: %5.2f\n" (percent !past_verbs !verbs);
Printf.printf "nouns: %5.2f\n" (percent !nouns !tokens);

Printf.printf "nouns/verbs: %5.2f\n"( (percent !nouns !verbs) /. 100.);

for szam = 0 to 1 do 
	for szem = 0 to 2 do
		Printf.printf "%d szam %d szem %5.2f\n" (szam+1) (szem+1) (percent verb_szamszem.(szam).(szem) !verbs);
	done
done