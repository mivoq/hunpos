open File_node
open Std
open Str
open String
open Unix
open Unix.LargeFile


let rec quicksort (list : file_node list) =
  match list with
  | [] -> []
  | [x] -> [x]
  | pivot::rest ->
    let rec partition left right list =
      match list with
      | [] -> quicksort left @ (pivot :: quicksort right)
      | head::tail -> if String.compare head#get_full_name pivot#get_full_name <= 0
                          then partition (head::left) right tail
                          else partition left (head::right) tail
    in partition [] [] rest;;

let rec contains elem lst =
  match lst with
    [] -> false
  | head :: tail -> if String.compare elem#get_full_name head#get_full_name == 0
                        then true
                        else contains elem tail;
;;

let rec unify (lst : file_node list) =
  match lst with
    [] -> []
  | head :: tail -> if contains head tail
                      then unify tail
                      else head :: (unify tail)
;;
  

let sort_and_unify (lst : file_node list) =
  unify (quicksort lst);
;;

let rec parse (file_list : file_node list) = 
  match file_list with
     [] -> []
   | head :: tail ->
      let in_chan = (open_in head#get_full_name) in
        let lines = Std.input_list in_chan in
          begin 
            close_in in_chan;
            ((head, lines) :: parse tail);
          end;
;;

let rec find_regexp_in_string_list (regexp : Str.regexp) (lst : string list) =
  match lst with
    [] -> false
   | head :: tail -> if string_match regexp head 0 then true
                                                   else find_regexp_in_string_list regexp tail;
;;

let rec strip_file_list_by_content (file_list : file_node list) (regexp : Str.regexp) =
  match file_list with
     [] -> []
   | head :: tail ->
      let in_chan = (open_in head#get_full_name) in
      let lines = Std.input_list in_chan in
          begin 
            close_in in_chan;
            if find_regexp_in_string_list regexp lines
                then head :: (strip_file_list_by_content tail regexp)
                else          strip_file_list_by_content tail regexp;
          end;
;;

let rec strip_file_list_by_name (file_list : file_node list) (regexp : Str.regexp) =
  match file_list with
    [] -> []
   | head :: tail -> if string_match regexp (head#get_name) 0 
                        then head :: strip_file_list_by_name tail regexp
                        else         strip_file_list_by_name tail regexp
;;

let rec get_messages strings result =
  match strings with
    [] -> result 
  | head :: tail -> if    string_match (Str.regexp ".*_m_.*") head 0 
                       || string_match (Str.regexp ".*_M_.*") head 0
                            then (get_messages tail (head :: result))
                            else (get_messages tail result);
;;


let rec strip_file_list (lst : file_node list) =
  match lst with
    [] -> []
   | head :: tail -> 
    if head#get_name <> "." && head#get_name <> ".svn" then head :: strip_file_list tail
                                                       else strip_file_list tail;
;;

let rec read_files path fd lst name_regexp = 
  try
    let file = Unix.readdir fd in
      if file <> ".."  && file <> "." && file <> ".svn" (** ... check for reg-exp*)
        then 
          let full_path = path ^ "/" ^ file in 
            (** print_string ("processing file " ^ full_path ^ "\n"); *)
            let s = stat(full_path) in
              if s.st_kind = S_DIR 
                then
                  let sub_fd = Unix.opendir full_path in
                    (** print_string ("reading sub-dir " ^ full_path ^ "\n"); *)
                    let a = (read_files path fd lst name_regexp) in
                      let b = (read_files full_path sub_fd lst name_regexp) in
                        a @ b;
                else 
                  if (Str.string_match name_regexp file 0) then
                    begin
                      let n = make_fn() in 
                      n#set_name file;
                      n#set_path path;
                      n#set_full_name (path ^ "/" ^ file);
                      let l = n :: lst in 
                        read_files path fd l name_regexp;
                    end
                  else read_files path fd lst name_regexp;
          else
            read_files path fd lst name_regexp;
  with 
    | End_of_file -> lst
    | Unix_error (EACCES, err, parm) ->
          begin
            print_string "cannot open ";
            print_string parm;
            print_string " for reading\n";
            lst
          end
;;


let rec get_all_files path name_regexp =
    let fd = Unix.opendir path in
      sort_and_unify (read_files path fd [] name_regexp); 
;;



