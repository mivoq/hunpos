open Ocamlbuild_plugin;; 
open Command;; 
dispatch begin function 
| After_rules -> 
    flag ["use_cc" ] 
     (S[A"-output-obj"; A"-cc"; A"g++";   A"-custom"; ]); 
     
    
| _ -> ()
end;;

(*  A"-output-obj"; A"-ccopt"; *)