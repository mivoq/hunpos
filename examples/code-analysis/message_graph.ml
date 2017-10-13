open File_node 
open Files
open Message_node 
open Std
open Str
open String
open Unix
open Io


let rec get_messages_from_lines (lst : string list) (regexp : Str.regexp) (lines : string list) =
  match lines with
     [] -> lst
   | head :: tail -> if string_match regexp head 0 
                          then 
                            let l = (head :: lst) in 
                              get_messages_from_lines l regexp tail;
                          else
                              get_messages_from_lines lst regexp tail;
;;
  
let strip_name (str : string) =
  global_replace (Str.regexp "\\.cpp$") "" (global_replace (Str.regexp "\\.h$") "" str);
;;



let rec get_message_graph (file_list : file_node list) =
  match file_list with
     [] -> []
   | head :: tail -> 
      let in_chan = (open_in head#get_full_name) in
        let lines = Std.input_list in_chan in
          begin 
            close_in in_chan;
            let node = make_mn() in begin
                                      node#set_class_name (strip_name head#get_name);
                                      node#set_messages_out  (get_messages_from_lines [] (Str.regexp ".*notifyObservers.*_m_.*") lines);
                                      node#set_messages_in   (get_messages_from_lines [] (Str.regexp ".*_M_.*") lines);
                                      node :: get_message_graph tail;
                                    end;
          end;
;;

let strip_message_string (str : string) = 
   global_replace 
     (Str.regexp ".*\\(_m\\|__M\\)_\\(\\([a-z]\\|[A-Z]\\|_\\)+\\).*")
     ("\\1_\\2")
     str
;;

let rec strip_message_node_string_list (lst : string list) =
  match lst with
    [] -> []
  | head :: tail -> (strip_message_string head) :: (strip_message_node_string_list tail);
;;

let rec strip_message_node (node : message_node) =
  begin
    node#set_messages_out (strip_message_node_string_list node#get_messages_out);
    node#set_messages_in (strip_message_node_string_list node#get_messages_in);
    node;
  end;
;;

let rec strip_messages (lst : message_node list) =
  match lst with
    [] -> []
  | head :: tail -> (strip_message_node head) :: (strip_messages tail);
;;



let rec get_pairings_snd (lst : string list) =
  match lst with
    [] -> ""
  | head :: tail ->  if string_match (Str.regexp ".*_M_.*") head 0
                       then head
                       else get_pairings_snd tail;
;;

let rec get_pairings_from_lines (lst : string list) =
  match lst with
    [] -> []
  | head :: tail -> if string_match (Str.regexp ".*_m_.*") head 0
                      then
                        let t = (head, (get_pairings_snd tail)) in
                            t :: (get_pairings_from_lines tail);
                      else
                        get_pairings_from_lines tail;
;;

let rec get_pairings file =
  let in_chan = (open_in file) in
    let lines = Std.input_list in_chan in
      print_string ("found: " ^ file ^ "\n");
      get_pairings_from_lines lines;
;;
   
let rec strip_pairings (lst : (string * string) list) =
  match lst with
    [] -> []
  | head :: tail -> ( (strip_message_string (fst head)), 
                      (strip_message_string (snd head))) :: strip_pairings tail;
;;

let rec get_mapping (lst : file_node list) =
  match lst with
    [] -> []
  | head :: tail -> if string_match (Str.regexp ".*ObservableMessage.*") head#get_name 0
                      then strip_pairings (get_pairings head#get_full_name)
                      else get_mapping tail
;;


let rec get_map (s : string) (mapping : (string * string)  list) =
  match mapping with
    [] -> ""
  | head :: tail -> if compare s (fst head) == 0
                      then (snd head)
                      else get_map s tail;
;;

let rec map_list (lst : string list) (mapping : (string * string)  list) =
  match lst with
    [] -> []
  | head :: tail -> (get_map head mapping) :: (map_list tail mapping);
;;
  



let map (node : message_node) (mapping : (string * string)  list) =
  begin
    node#set_messages_out (map_list node#get_messages_out mapping);
    node;
  end;
;;

  

let rec map_mappings (lst : message_node list) (mapping : (string * string) list) =
  match lst with
    [] -> []
  | head :: tail -> (map head mapping) :: (map_mappings tail mapping);
;;


 

let get_dot_header =
  "digraph abstract {\n" ^
	"   size=\"6,6\";\n";
;;

let get_dot_footer =
  "}\n";
;;


let left_to_right a b = "  \"" ^ a ^ "\" -> \"" ^ b ^ "\";\n";;
let right_to_left a b = "  \"" ^ b ^ "\" -> \"" ^ a ^ "\";\n";;

let rec convert_node_list_to_dot_string s nodes func =
  match nodes with
    [] -> ""
  | head :: tail -> (func s head#get_class_name) ^ (convert_node_list_to_dot_string s tail func);
;;


let rec convert_node_to_string node =
  (convert_node_list_to_dot_string node#get_class_name node#get_sending_to     left_to_right) ^
  (convert_node_list_to_dot_string node#get_class_name node#get_receiving_from right_to_left);
;;


let rec check_peer_entry s lst =
  match lst with
    [] -> false
  | head :: tail -> begin
                      if compare s head == 0 then true
                                             else check_peer_entry s tail;
                    end;
;;

let rec check_peer (lst1 : string list) (lst2 : string list) =
  match lst1 with
    [] -> false
  | head :: tail -> begin
                      if check_peer_entry head lst2 
                        then true
                        else check_peer tail lst2;
                    end;
;;
  
  


let rec get_node_relations (node : message_node) (peers : message_node list) =
  match peers with
    [] -> node
  | head :: tail -> begin
                      if check_peer node#get_messages_in head#get_messages_out then
                        node#set_receiving_from (head :: node#get_receiving_from);

                      if check_peer node#get_messages_out head#get_messages_in then
                        node#set_sending_to (head :: node#get_sending_to);

                      get_node_relations node tail;
                    end;
;;
                        


let rec __get_message_relations (a : message_node list) (b: message_node list) =
  match a with
    [] -> []
  | head :: tail -> (get_node_relations head b) :: (__get_message_relations tail b);
;;

let get_message_relations (nodes : message_node list) =
  __get_message_relations nodes nodes;
;;

let rec convert_nodes_to_string (nodes : message_node list) =
  match nodes with
    [] -> ""
  | head :: tail -> (convert_node_to_string head) ^ (convert_nodes_to_string tail);
;; 
  
let graph_to_dot_string (nodes : message_node list) =
  ("all", get_dot_header ^ (convert_nodes_to_string nodes) ^ get_dot_footer);
;; 

let rec nodes_to_dot_strings (nodes : message_node list) =
  match nodes with
    [] -> []
  | head :: tail -> (head#get_class_name, (get_dot_header ^ (convert_node_to_string head) ^ get_dot_footer)) :: nodes_to_dot_strings tail;
;;
  

let message_graph_to_dot_string (nodes : message_node list) =
  (graph_to_dot_string nodes) :: (nodes_to_dot_strings nodes);
;;




 


let rec parse_observables_lines lines =
  match lines with
    [] -> []
  | head :: tail -> if string_match (Str.regexp ".*\\(Model\\|Control\\).*") head 0 
                      then head :: parse_observables_lines tail
                      else parse_observables_lines tail;
;;

let get_observables_lines node =
  let in_chan = (open_in node#get_full_name) in
      let lines = Std.input_list in_chan in
          begin 
            close_in in_chan;
            (node#get_name, parse_observables_lines lines);
          end;
;;

let rec get_observable_data observable_data = 
  match observable_data with
    [] -> []
  | head :: tail -> (get_observables_lines head) :: (get_observable_data tail);
;;

let parse_observable_string s =
  global_replace (Str.regexp ".*\\*\\(\\w+\\) *= * \\(\\w+\\)(.*$") "\\1 -- \\2" s;
;;

let rec strip_observable_lines lines =
  match lines with
    [] -> []
  | head :: tail -> if string_match (Str.regexp ".*new.*") head 0
                      then (parse_observable_string head) :: strip_observable_lines tail
                      else strip_observable_lines tail;
;;

let rec strip_observable_data observable_data =
  match observable_data with
    [] -> []
  | head :: tail -> let lines = strip_observable_lines (snd head) in
                      if List.length lines > 0
                        then (fst head, lines) :: strip_observable_data tail
                        else strip_observable_data tail;
;;



let list_function_out (node : message_node) = node#get_observers;;
let list_function_in  (node : message_node) = node#get_observables;;


let rec __get_all_path (source : message_node)
                       (destination : message_node)
                       (next_function : message_node -> message_node list) =
  begin
    print_string "will get all path from ";
    print_string source#get_class_name;
    print_string " to ";
    print_string destination#get_class_name;
    print_string "\n";
    let next_list = next_function source in
      begin
        print_string "next list: \n";
        print_message_nodes_short next_list;
      end
  end;
;;

let rec __get_path (source : message_node)
                   (destinations : message_node list) 
                   (next_function : message_node -> message_node list) =
  match destinations with
    [] -> ()
  | head :: tail -> 
    begin
      __get_all_path source head next_function;
      __get_path source tail next_function;
    end
;;
  
let __get_in_paths (node : message_node) =
  begin
    print_string "getting in-paths to node ";
    print_string node#get_class_name;
    print_string "\n";
    __get_path node node#get_receiving_from list_function_in;
  end
;;
  
let __get_peer_path (node : message_node) =
  begin
    __get_in_paths node;
    node;
  end
;;

let rec get_peer_path (lst : message_node list) =
  match lst with
    [] -> []
  | head :: tail -> (__get_peer_path head) :: get_peer_path tail;
;;
