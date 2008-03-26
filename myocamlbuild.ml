open Ocamlbuild_plugin;; 
open Command;; 

let libasmrun = !*Ocamlbuild_pack.Ocaml_utils.stdlib_dir/"libasmrun.a";;
Printf.printf "%s\n" libasmrun;;

let ar = A"ar";;

let headers = ["hunpos.h"]
;;

dispatch begin function 
| After_rules -> 
  
   rule "output C obj"
      ~dep:"%.ml"
      ~prod:"%caml.o"
      begin fun env _ ->
        let caml_o = env "%caml.o" and ml = env "%.ml" in
        Cmd(S[!Options.ocamlopt; A"-output-obj"; P ml; A"-o"; Px caml_o])
      end;
    rule "build C lib"
      ~deps:["%wrap.o"; "%caml.o"]
      ~prod:"lib%.a"
      begin fun env _ ->
        let wrap_o = env "%wrap.o" and caml_o = env "%caml.o"
        and lib_a = env "lib%.a" in
        Seq[cp libasmrun lib_a;
            Cmd(S[ar; A"r"; Px lib_a; P caml_o; P wrap_o])]
      end;

    flag ["use_cc" ] 
     (S[A"-output-obj"; A"-cc"; A"g++"; A"-cclib" ; A"-custom"; ]); 
          
     (* As an approximation all our C files use the headers.
        Note: This will import headers in the build directory. *)
     dep  ["compile"; "c"] headers;
   ()  
| _ -> ()
end;;

(*  A"-output-obj"; A"-ccopt"; *)
