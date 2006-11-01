(** Wrapper az ocamorph/morphdb.hu kore, hogy ugy lassuk, ahogy taggeleshez kenyelmes.
	Itt kituntetett szerepet kapnak a POS<KR_KOD> alaku inflexios cimkek, amikkel a tagger
	dolgozik.
	
	A wrapper kozvetlenul egy bin erofossas fajl alapjan hivja az ocamorph Analysis moduljat.
	
	Minden szo elemzeset beteszi egy cache-be, amit nem urit. Az elemzes nagyon lassu dolog,
	de a legnagyobb egyben taggelt korpusz lexikonja is befer memoriaba. A cache tartalmat
	el is tudja menteni (save_cache) az {[ocamorph.cache]} fajlba. Kovetkezo init re meg megprobalja
	beolvasni.
	
	
	*)

(** A wrapper altal egy szora visszaadott eredmeny tipusa, kozvetlenul nem 
	olvashato belole ki semmi *)
type analysis 

(** Az init altal visszaadott elemzo tipusa
	*)
type analyzer 

(** A cache tartalmát elmenti az {[ocamorph.cache]} fájlba a kurrens könyvtárba. 
	*)
val save_cache : analyzer -> unit


(** Betölti az elemzőt a megadott erőforrás fájllal. Megpróbálja feltölteni a cache
	tartálmat a {[ocamorph.cache]} fájlból. Ha nem talál ilyen fájlt, vagy bármi hiba
	van, akkor üres cache-el indul.
	
	*)
val init : unit -> analyzer

(** Elemzi a szót. A visszaadott elemzést közvetlenül semmire nem lehet használni. Lásd a tags, oov stb.
	függvényeket.
	*)
val analyze : analyzer -> string -> analysis

(** Visszaadja az elemzett szó lehetséges cimkéit.
	*)
val tags : analysis -> string list

(** Ha az elemző nem ismerte a szót, azaz csak guessing volt.
	*)
val oov : analysis -> bool

(** 
	@raise Not_found Ha az adott taghez nincs is elemzes.
	*)
val full_analyses : analysis -> string -> string list