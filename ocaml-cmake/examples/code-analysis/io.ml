open Files
open File_node
open Message_node
open Unix
open Unix.LargeFile
open Str

(** sum __locs data *)
let __locs_total       f = f#get_loc;;
let __locs_comments    f = f#get_loc_comments;;
let __locs_non_trivial f = f#get_loc_nontrivial;;
let __locs_trivial     f = f#get_loc_trivial;;

(** sum loc data *)
let rec __locs_sum lst f =
  match lst with
    [] -> 0
  | head :: tail -> (f head) + (__locs_sum tail f);
;;


(** private functions *)
let rec __set_length size str =
  if size > String.length str then (__set_length (size - 1) str) ^ " "
                              else str;
;;
  
(** print node list *)
let rec __print_node_list (prefix : string) (lst : message_node list) =
  match lst with
    [] -> print_newline()
  | head :: tail -> begin
                      print_string (prefix ^ head#get_class_name ^ "\n");
                      __print_node_list prefix tail;
                    end;
;;


(*********************************************************)
(** public functions                                     *)
(*********************************************************)

(** print string list *)
let rec print_string_list (prefix : string) (lst : string list) =
  match lst with
    [] -> print_newline();
  | head :: tail -> begin
                      print_string (prefix ^ head ^ "\n");
                      print_string_list prefix tail;
                    end;
;;

(** print file node list *)
let rec print_file_node_list lst =
  match lst with
    [] -> ();
  | head :: tail -> 
      print_string ("file: " ^ (__set_length 30 head#get_name) ^ " \tlocated at " ^ head#get_path ^ "/" ^ head#get_name ^ "\n");
      print_file_node_list tail;
;;


(** print message node list *)
let rec print_message_nodes (lst : message_node list) =
  match lst with
     [] -> print_newline(); 
   | head :: tail -> begin
      print_string ("node: " ^ head#get_class_name ^ "\n");
      if ( List.length (head#get_messages_in) > 0 ) then
                                                 begin
                                                   print_string ("  has in-messages: \n");
                                                   print_string_list "    " head#get_messages_in;
                                                 end
                                               else
                                                 print_string("");
      if ( List.length (head#get_messages_out) > 0 ) then
                                                 begin
                                                   print_string ("  has out-messages: \n");
                                                   print_string_list "    " head#get_messages_out;
                                                 end
                                               else
                                                 print_string("");
      if ( List.length (head#get_sending_to) > 0 ) then
                                                 begin
                                                   print_string ("  has out-peers: \n");
                                                   __print_node_list "    " head#get_sending_to;
                                                 end
                                               else
                                                 print_string("");
      if ( List.length (head#get_receiving_from) > 0 ) then
                                                 begin
                                                   print_string ("  has in-peers: \n");
                                                   __print_node_list "    " head#get_receiving_from;
                                                 end
                                               else
                                                 print_string("");
      if ( List.length (head#get_observers) > 0 ) then
                                                 begin
                                                   print_string ("  has observers: \n");
                                                   __print_node_list "    " head#get_observers;
                                                 end
                                               else
                                                 print_string("");
      if ( List.length (head#get_observables) > 0 ) then
                                                 begin
                                                   print_string ("  has observables: \n");
                                                   __print_node_list "    " head#get_observables;
                                                 end
                                               else
                                                 print_string("");
      print_message_nodes tail;
      end;
;;


(** short version print_message_nodes function. only prints the node names *)
let rec print_message_nodes_short (lst : message_node list) =
  match lst with
     [] -> (); 
   | head :: tail -> begin
                       print_string ("node: " ^ head#get_class_name ^ "\n");
                       print_message_nodes_short tail;
                     end;
;;




(**
    prints a pair of string such as ("first", "second"):
    first:
    second
*)
let rec print_string_pair_list (prefix : string) (lst : (string * string) list) =
  match lst with
    [] -> print_newline();
  | head :: tail -> begin
                      print_string (prefix ^ (fst head) ^ ":\n");
                      print_string (prefix ^ (snd head) ^ "\n");
                      print_string_pair_list prefix tail;
                    end;
;;



(**
  * prints a pair of string and string list in the form of
  * head:
  *   entry
  *   entry
  *   ...
  *
  * with a prefix
*)
let rec print_string_string_pair_list (prefix : string) (lst : (string * (string list)) list) =
  match lst with
    [] -> print_newline();
  | head :: tail -> begin
                      print_string (prefix ^ (fst head) ^ ":\n");
                      print_string_list prefix (snd head);
                      print_string_string_pair_list prefix tail;
                    end;
;;



let rec write_files dir lst ext =
  match lst with
    [] -> ();
  | head :: tail -> begin
                      let out_channel = (open_out (dir ^ "/" ^ (fst head) ^ ext)) in 
                        output_string out_channel (snd head);
                      close_out out_channel;
                      write_files dir tail ext;
                    end;
;;

let rec remove_files (lst : file_node list) =
  match lst with
    [] -> ()
  | head :: tail -> begin
                      (** print_string ("will remove " ^ head#get_full_name ^ "\n"); *)
                      Unix.unlink head#get_full_name; 
                      remove_files tail;
                    end;
;;

let clean_directory dot_dir = 
  try 
    let filelist = get_all_files dot_dir (Str.regexp ".*") in 
      begin
        remove_files filelist;
        Unix.rmdir dot_dir;
        ();
      end;
  with 
    | Unix_error (ENOENT, err, parm) -> ();
;;


let get_total_loc_str str lst = 
  let locs_sum             = __locs_sum lst __locs_total       in 
  let locs_comment_sum     = __locs_sum lst __locs_comments    in 
  let locs_non_trivial_sum = __locs_sum lst __locs_non_trivial in 
  let locs_trivial_sum     = __locs_sum lst __locs_trivial     in 
      str
      ^ "\n"
      ^ "     Number of files : "
      ^ (string_of_int (List.length(lst)))
      ^ "\n"
      ^ "     LOC             : "
      ^ (string_of_int locs_sum)
      ^ "\n"
      ^ "     LOC Comments    : "
      ^ (string_of_int locs_comment_sum)
      ^ "\n"
      ^ "     LOC Non-Trivial : "
      ^ (string_of_int locs_non_trivial_sum)
      ^ "\n"
      ^ "     LOC Trivial     : "
      ^ (string_of_int locs_trivial_sum)
      ^ "\n";
;;

let rec get_files_loc_string lst = 
  match lst with
    [] -> "\n";
  | head :: tail -> head#get_full_name ^ " has: \n"
                    ^ " File type       : "
                    ^ head#get_file_type#get_short
                    ^ " ("
                    ^ head#get_file_type#get_long
                    ^ ")"
                    ^ "\n"
                    ^ " LOC total       : "
                    ^ (string_of_int (head#get_loc))
                    ^ "\n"
                    ^ " LOC comments    : "
                    ^ (string_of_int (head#get_loc_comments))
                    ^ "\n"
                    ^ " LOC non-trivial : "
                    ^ (string_of_int (head#get_loc_nontrivial))
                    ^ "\n"
                    ^ " LOC trivial     : "
                    ^ (string_of_int (head#get_loc_trivial))
                    ^ "\n"
                    ^ get_files_loc_string tail;
;;


let pre number prefix =
  if number < 10 then prefix ^ (string_of_int number)
                 else       (string_of_int number);
;;


let __output n s =
  if n > 0 then s
           else ""
;;

let get_loc_stats_number_of_files filelist =
  let filelist_cpp   = strip_file_list_by_name filelist (Str.regexp ".*\\.cpp$") in
  let filelist_h     = strip_file_list_by_name filelist (Str.regexp ".*\\.h$") in
  let filelist_txt   = strip_file_list_by_name filelist (Str.regexp ".*\\.txt$") in
  let filelist_cmake = strip_file_list_by_name filelist (Str.regexp ".*\\.cmake$") in
  let filelist_xml   = strip_file_list_by_name filelist (Str.regexp ".*\\.xml$") in
  let filelist_xsd   = strip_file_list_by_name filelist (Str.regexp ".*\\.xsd$") in
  let filelist_ml    = strip_file_list_by_name filelist (Str.regexp ".*\\.ml$") in
  let filelist_mli   = strip_file_list_by_name filelist (Str.regexp ".*\\.mli$") in
  let filelist_tex   = strip_file_list_by_name filelist (Str.regexp ".*\\.tex$") in
  let filelist_bib   = strip_file_list_by_name filelist (Str.regexp ".*\\.bib$") in
  let filelist_pl    = strip_file_list_by_name filelist (Str.regexp ".*\\.pl$") in
  let filelist_java  = strip_file_list_by_name filelist (Str.regexp ".*\\.java$") in
      "Number of files (total)  : " ^ (string_of_int (List.length(filelist))) ^ "\n"
    ^ (__output (List.length(filelist_cpp))
                ("Number of cpp files    : " ^ (string_of_int (List.length(filelist_cpp))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of h files      : " ^ (string_of_int (List.length(filelist_h))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of cmake files  : " ^ (string_of_int (List.length(filelist_cmake))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of txt files    : " ^ (string_of_int (List.length(filelist_txt))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of xml files    : " ^ (string_of_int (List.length(filelist_xml))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of xsd files    : " ^ (string_of_int (List.length(filelist_xsd))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of ml files     : " ^ (string_of_int (List.length(filelist_ml))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of mli files    : " ^ (string_of_int (List.length(filelist_mli))) ^ "\n"))
    ^ (__output (List.length(filelist_tex))
                ("Number of tex files    : " ^ (string_of_int (List.length(filelist_tex))) ^ "\n"))
    ^ (__output (List.length(filelist_bib))
                ("Number of bibtex files : " ^ (string_of_int (List.length(filelist_bib))) ^ "\n"))
    ^ (__output (List.length(filelist_pl))
                ("Number of perl files   : " ^ (string_of_int (List.length(filelist_pl))) ^ "\n"))
    ^ (__output (List.length(filelist_cpp))
                ("Number of java files   : " ^ (string_of_int (List.length(filelist_java))) ^ "\n"));
;;

let get_loc_stats_file_group filelist =
  let filelist_cpp   = strip_file_list_by_name filelist (Str.regexp ".*\\.cpp$") in
  let filelist_h     = strip_file_list_by_name filelist (Str.regexp ".*\\.h$") in
  let filelist_txt   = strip_file_list_by_name filelist (Str.regexp ".*\\.txt$") in
  let filelist_cmake = strip_file_list_by_name filelist (Str.regexp ".*\\.cmake$") in
  let filelist_xml   = strip_file_list_by_name filelist (Str.regexp ".*\\.xml$") in
  let filelist_xsd   = strip_file_list_by_name filelist (Str.regexp ".*\\.xsd$") in
  let filelist_ml    = strip_file_list_by_name filelist (Str.regexp ".*\\.ml$") in
  let filelist_mli   = strip_file_list_by_name filelist (Str.regexp ".*\\.mli$") in
  let filelist_tex   = strip_file_list_by_name filelist (Str.regexp ".*\\.tex$") in
  let filelist_bib   = strip_file_list_by_name filelist (Str.regexp ".*\\.bib$") in
  let filelist_pl   = strip_file_list_by_name filelist (Str.regexp ".*\\.pl$") in
  let filelist_java  = strip_file_list_by_name filelist (Str.regexp ".*\\.java$") in
      (__output (List.length(filelist_cpp))
                (get_total_loc_str ("Cpp files")    filelist_cpp))
    ^ (__output (List.length(filelist_h))
                (get_total_loc_str ("Cpp Header files")    filelist_h))
    ^ (__output (List.length(filelist_txt))
                (get_total_loc_str ("Txt files")    filelist_txt))
    ^ (__output (List.length(filelist_cmake))
                (get_total_loc_str ("Cmake files")    filelist_cmake))
    ^ (__output (List.length(filelist_xml))
                (get_total_loc_str ("Xml files")    filelist_xml))
    ^ (__output (List.length(filelist_xsd))
                (get_total_loc_str ("Xsd files")    filelist_xsd))
    ^ (__output (List.length(filelist_ml))
                (get_total_loc_str ("Ml files")    filelist_ml))
    ^ (__output (List.length(filelist_mli))
                (get_total_loc_str ("Mli files")    filelist_mli))
    ^ (__output (List.length(filelist_tex))
                (get_total_loc_str ("TeX files")    filelist_tex))
    ^ (__output (List.length(filelist_bib))
                (get_total_loc_str ("Bibtex files")    filelist_bib))
    ^ (__output (List.length(filelist_pl))
                (get_total_loc_str ("Perl files")    filelist_pl))
    ^ (__output (List.length(filelist_java))
                (get_total_loc_str ("Java files")    filelist_java));
;;

let print_loc_stats filelist =
    print_string ((get_files_loc_string filelist) 
                 ^ get_loc_stats_number_of_files filelist 
                 ^ "\n"
                 ^ get_loc_stats_file_group filelist 
                 ^ "\n"
                 ^ (get_total_loc_str "All Files" filelist));
;;

let write_loc_stats filelist dirname =
      let t = Unix.time() in 
      let s = gmtime t in 
      let name_prefix = global_replace (Str.regexp ".*/\\(.*\\)$") "\\1" dirname in
      let file_name = "loc-" ^ name_prefix ^ "-"
                             ^ (string_of_int (1900 + s.tm_year))
                             ^ "-" ^ (pre (s.tm_mon + 1) "0")
                             ^ "-" ^ (pre s.tm_mday "0") ^ ".txt" in 
      let out_channel = (open_out file_name) in 
      let s = (get_total_loc_str "All Files" filelist)
            ^ "\n"
            ^ get_loc_stats_file_group filelist 
            ^ "\n"
            ^ get_loc_stats_number_of_files filelist 
            ^ "\n"
            ^ (get_files_loc_string filelist) in
      begin
          output_string out_channel s;
          close_out out_channel;
          print_string "Data written to file ";
          print_string file_name;
          print_newline();
      end;
;;
