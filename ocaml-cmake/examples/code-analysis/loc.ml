open Unix
open Files
open File_statistics
open Io
open Arg

let main () =
  if Array.length(Sys.argv) < 2
    then
      begin
        print_string "usage: ";
        print_string Sys.argv.(0);
        print_string " <dir>\n";
        print_string "output written to console and to date dependent file ";
        let t = Unix.time() in 
        let s = gmtime t in 
        let file_name = "loc-<dir>" ^ (string_of_int (1900 + s.tm_year)) ^ "-" ^ (pre (s.tm_mon + 1) "0") ^ "-" ^ (pre s.tm_mday "0") ^ ".txt" in 
        print_string file_name;
        print_newline();
        exit 0;
      end
    else
      begin
        let dirname = Sys.argv.(1) in
          let regexp = Str.regexp ".*" in 
            begin
              print_string("parsing files and subdirectories of \"");
              print_string(dirname);
              print_string("\":");
              print_newline();
              let filelist = get_locs_data (get_all_files dirname regexp) in 
                begin
                  print_loc_stats filelist;
                  print_newline();
                    write_loc_stats filelist dirname;
                end;
            end;
          exit 0;
      end;
;;
(** start **)
main();;
