
let obs = Simple_hmm.o_i ::  Simple_hmm.o_fly:: Simple_hmm.o_a :: Simple_hmm.o_plane :: [] in
let seq = Viterbi.decode obs Simple_hmm.transition_probs in 
List.iter (fun s -> print_int s; print_char '\t') seq ;