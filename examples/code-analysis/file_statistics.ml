open File_node
open Std
open Str
open String
open Unix
open Unix.LargeFile



let file_type file = 
  if String.compare (Str.last_chars file 3) "xsd" == 0 
    then file_type_xml
    else
  if String.compare (Str.last_chars file 3) "xml" == 0 
    then file_type_xml
    else 
  if String.compare (Str.last_chars file 14) "CMakeLists.txt" == 0 
    then file_type_cmake
    else 
  if String.compare (Str.last_chars file 5) "cmake" == 0 
    then file_type_cmake
    else 
  if String.compare (Str.last_chars file 3) "cpp" == 0 
    then file_type_cpp
    else 
  if String.compare (Str.last_chars file 1) "h" == 0 
    then file_type_cpp
    else 
  if String.compare (Str.last_chars file 4) "java" == 0 
    then file_type_java
    else 
  if String.compare (Str.last_chars file 2) "tex" == 0 
    then file_type_tex
    else 
  if String.compare (Str.last_chars file 2) "bib" == 0 
    then file_type_bib
    else 
  if String.compare (Str.last_chars file 2) "pl" == 0 
    then file_type_pl
    else 
  if String.compare (Str.last_chars file 2) "ml" == 0 
    then file_type_ml
    else 
  if String.compare (Str.last_chars file 3) "mli" == 0 
    then file_type_ml
    else 
      file_type_ascii;
;;

let file_loc file =
    let in_chan = (open_in file#get_full_name) in
      let lines = Std.input_list in_chan in
        begin 
          close_in in_chan;
          (** print_string("File: " ^ file ^ " has "); 
          print_int(List.length(lines));
          print_newline(); *)
          List.length(lines);
        end;
;;

let rec trim str = Str.global_replace (Str.regexp " ") "" str;;

let rec count_non_trivial_lines lines count comment starts ends intermediate =
  match lines with
    [] -> count
  | head :: tail -> let l = trim head in 
                        if String.length l < 2 
                          then count_non_trivial_lines tail count comment starts ends intermediate
                          else 
                            if comment == 1 
                              then 
                                if ends l 
                                  then count_non_trivial_lines tail count 0 starts ends intermediate
                                  else count_non_trivial_lines tail count 1 starts ends intermediate
                              else (** not in comment block *)
                                  if starts l
                                    then if ends l
                                           then count_non_trivial_lines tail count 0 starts ends intermediate
                                           else count_non_trivial_lines tail count 1 starts ends intermediate
                                    else 
                                    if intermediate l
                                      then count_non_trivial_lines tail count 0 starts ends intermediate
                                      else count_non_trivial_lines tail (count + 1) 0 starts ends intermediate
;;


let rec count_comment_lines lines count comment starts ends intermediate =
  match lines with
    [] -> count
  | head :: tail -> let l = trim head in 
                        if String.length l < 2 
                          then count_comment_lines tail (count + comment) comment starts ends intermediate
                          else 
                            if comment == 1 
                              then 
                                if ends l 
                                  then count_comment_lines tail (count + 1) 0 starts ends intermediate
                                  else count_comment_lines tail (count + 1) 1 starts ends intermediate
                              else
                                  if starts l
                                    then if ends l
                                           then count_comment_lines tail (count + 1) 0 starts ends intermediate
                                           else count_comment_lines tail (count + 1) 1 starts ends intermediate
                                    else 
                                    if intermediate l
                                      then count_comment_lines tail (count + 1) 0 starts ends intermediate
                                      else count_comment_lines tail count 0 starts ends intermediate
;;

let cpp_starts          l = String.length l >= 2 && String.compare (Str.first_chars l 2) "/*" == 0;;
let cpp_ends            l = String.length l >= 2 && String.compare (Str.last_chars  l 2) "*/" == 0;;
let cpp_intermediate    l = String.length l >= 2 && String.compare (Str.first_chars l 2) "//" == 0;;

let xml_starts          l = String.length l >= 4 && String.compare (Str.first_chars l 4) "<!--" == 0;;
let xml_ends            l = String.length l >= 3 && String.compare (Str.last_chars l 3)  "-->"  == 0;;
let xml_intermediate    l = false;;

let ml_starts           l = String.length l >= 3 && String.compare (Str.first_chars l 3) "(**" == 0;;
let ml_ends             l = String.length l >= 2 && String.compare (Str.last_chars l 2)  "*)"  == 0;;
let ml_intermediate     l = false;;

let cmake_starts        l = false;;
let cmake_ends          l = false;;
let cmake_intermediate  l = String.length l >= 1 && String.compare (Str.first_chars l 1) "#" == 0;;

let pl_starts        l = false;;
let pl_ends          l = false;;
let pl_intermediate  l = String.length l >= 1 && String.compare (Str.first_chars l 1) "#" == 0;;

let tex_starts        l = false;;
let tex_ends          l = false;;
let tex_intermediate  l = String.length l >= 1 && String.compare (Str.first_chars l 1) "%" == 0;;

let bib_starts        l = false;;
let bib_ends          l = false;;
let bib_intermediate  l = String.length l >= 1 && String.compare (Str.first_chars l 1) "%" == 0;;

let ascii_starts        l = false;;
let ascii_ends          l = false;;
let ascii_intermediate  l = false;;

let file_loc_special count_function file =
    let in_chan = (open_in file#get_full_name) in
      let lines = Std.input_list in_chan in
        begin
          close_in in_chan;
          if file#get_file_type == file_type_xml
            then count_function lines 0 0 xml_starts xml_ends xml_intermediate
            else
          if file#get_file_type == file_type_cmake
            then count_function lines 0 0 cmake_starts cmake_ends cmake_intermediate
            else 
          if file#get_file_type == file_type_cpp
            then count_function lines 0 0 cpp_starts cpp_ends cpp_intermediate
            else 
          if file#get_file_type == file_type_java
            then count_function lines 0 0 cpp_starts cpp_ends cpp_intermediate
            else 
          if file#get_file_type == file_type_ml
            then count_function lines 0 0 ml_starts ml_ends ml_intermediate
            else 
          if file#get_file_type == file_type_tex
            then count_function lines 0 0 tex_starts tex_ends tex_intermediate
            else 
          if file#get_file_type == file_type_bib
            then count_function lines 0 0 bib_starts bib_ends bib_intermediate
            else 
          if file#get_file_type == file_type_pl
            then count_function lines 0 0 pl_starts pl_ends pl_intermediate
            else 
          if file#get_file_type == file_type_ascii
            then count_function lines 0 0 ascii_starts ascii_ends ascii_intermediate
            else 0 (** undefined *)
        end;
;;


let rec get_locs_data lst =
  match lst with
    [] -> []
  | head :: tail -> begin
                      head#set_file_type      (file_type        head#get_full_name);
                      head#set_loc            (file_loc         head);
                      head#set_loc_comments   (file_loc_special count_comment_lines     head);
                      head#set_loc_nontrivial (file_loc_special count_non_trivial_lines head);
                      head#set_loc_trivial    (head#get_loc - head#get_loc_nontrivial);
                      head :: (get_locs_data tail);
                    end;
;;


