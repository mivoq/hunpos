open Unix
open Message_node
open File_node
open Io
open Files
open Message_graph
open Observables_graph

let dot_dir = "./dots";;

let main () =
  if Array.length(Sys.argv) < 2
    then
      begin
        print_string "usage: ";
        print_string Sys.argv.(0);
        print_string " <dir>\n";
        print_string "all files will be written to the directory \"";
        print_string dot_dir;
        print_string "\" in graphvis style.";
        let t = Unix.time() in 
        let s = gmtime t in 
        let file_name = "loc-" ^ (string_of_int (1900 + s.tm_year)) ^ "-" ^ (pre (s.tm_mon + 1) "0") ^ "-" ^ (pre s.tm_mday "0") ^ ".txt" in 
        print_string file_name;
        print_newline();
        exit 0;
      end
    else
      let dirname = Sys.argv.(1) in
        let cpp_regexp = Str.regexp ".*\\(h\\|cpp\\)$" in 
          begin
            print_string("parsing files and subdirectories of \"");
            print_string(dirname);
            print_string("\":");
            print_newline();
            clean_directory dot_dir;
            Unix.mkdir dot_dir 0o755;
            let filelist         = get_all_files dirname cpp_regexp in
            let filelist_cpp     = strip_file_list_by_name filelist (Str.regexp ".*\\.cpp$") in
            let filelist_h       = strip_file_list_by_name filelist (Str.regexp ".*\\.h$") in
            let mappings         = get_mapping filelist_h in
            let classes_list     = strip_file_list_by_content filelist_cpp (Str.regexp ".*\\(_m_\\|_M_\\).*") in
            let connecting_files = strip_file_list_by_content filelist_cpp (Str.regexp ".*addObserver.*") in
            let graph            = get_message_graph classes_list in
            let message_graph    = get_observable_graph connecting_files graph in
            let l                = strip_messages message_graph in
            let m                = map_mappings l mappings in
            let n                = get_message_relations m in
              (** let o = get_peer_path n in *)
              begin
                (** print_message_nodes n; *)
                let s = message_graph_to_dot_string n in
                  write_files dot_dir s ".dot";
              end;
          end;
        exit 0;;

(** start **)
main();;
