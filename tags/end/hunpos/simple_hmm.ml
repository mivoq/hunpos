(* most teszteleshez egy egyszeru kis hmm-ecske. *)
let o_a = 0;;
let o_i = 1;;
let o_fly = 2;;
let o_plane = 3;;

let s_n = 0;;
let s_v = 1;;
let s_d = 2;;
	
let trans_matrix = Array.make_matrix 3 3 0.0 ;;
trans_matrix.(0).(0) <-  0.3;;
trans_matrix.(0).(1) <-  0.7;;
trans_matrix.(0).(2) <-  0.0;;
trans_matrix.(1).(0) <-  0.25;;
trans_matrix.(1).(1) <-  0.0;;
trans_matrix.(1).(2) <-  0.75;;
trans_matrix.(2).(0) <-  1.0;;
trans_matrix.(2).(1) <-  0.0;;
trans_matrix.(2).(2) <-  0.0;;

let observation_matrix = Array.make_matrix 3 4 0.0;;
observation_matrix.(s_n).(o_a) <-  0.05;;
observation_matrix.(s_v).(o_a) <-  0.0;;
observation_matrix.(s_d).(o_a) <-  1.0;;
observation_matrix.(s_n).(o_fly) <-  0.3;;
observation_matrix.(s_v).(o_fly) <-  0.75;;
observation_matrix.(s_d).(o_fly) <-  0.0;;
observation_matrix.(s_n).(o_i) <-  0.35;;
observation_matrix.(s_v).(o_i) <-  0.0;;
observation_matrix.(s_d).(o_i) <-  0.0;;
observation_matrix.(s_n).(o_plane) <-  0.3;;
observation_matrix.(s_v).(o_plane) <-  0.25;;
observation_matrix.(s_d).(o_plane) <-  0.0;;

let init_probs = Array.make 3 0.0;;
init_probs.(s_n) <-  0.4;;
init_probs.(s_v) <-  0.1;;
init_probs.(s_d) <-  0.5;;
let start = (-1) ;;

	 
(* most itt foloslegesnek tunhet, hogy egyben adjuk meg egyszerre
	tobb allapotra, hogy O eseten merre lehet tovabbmenni. De taggelesnel
	draga mulatsag minden egyes lehetseges elozo allapotra meghivni mondjuk
	a guessert, ami ugyis fuggetlen az elozo allapottol. *)
	

let transition_from_one_state state_from o =
	let transition =  
		match state_from with
		 	(-1) -> init_probs
			| _ ->	trans_matrix.(state_from)
	in
	Array.to_list (Array.mapi (fun s w -> (s ,	observation_matrix.(s).(o) *. w )) transition)
;;

let rec transition states_from o = 
	match states_from with
		f :: t -> (f, transition_from_one_state f o) :: transition t o
	| [] -> []
	;;

let transition_probs o = 
		( fun state_from -> transition_from_one_state state_from o) ;;
(* 
let t = transition_probs 1 ;;		
let g = t (0 :: 1 :: []) ;; *)