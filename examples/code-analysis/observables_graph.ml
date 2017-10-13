open File_node
open Message_node
open Io

class o_o_data =
  object
    val mutable _name           = ""
    val mutable _observers      = ([] : string list)
    val mutable _observables    = ([] : string list)
    val mutable _lines          = ([] : string list)
    method get_name             = _name
    method set_name n           = _name <- n
    method get_observers        = _observers
    method set_observers l      = _observers <- l
    method get_observables      = _observables
    method set_observables l    = _observables <- l
    method set_lines l          = _lines <- l
    method get_lines            = _lines
  end;;

let rec __parse_file (name  : string)
                     (lines : string list) =
  match lines with
    [] -> []
  | head :: tail -> begin
                      print_string "name: ";
                      print_string name;
                      print_string " line: ";
                      print_string head;
                      print_newline();
                      __parse_file name tail;
                    end
;;

let rec __get_pairings  (files       : file_node list) =
  match files with
    [] -> ()
  | head :: tail ->
    begin
      print_string "now working on ";
      print_string head#get_name;
      print_newline();
      let in_chan = (open_in head#get_full_name) in
        let lines = Std.input_list in_chan in
          begin 
            close_in in_chan;
            __parse_file head#get_name lines;
            (); (** to make it a unit *)
          end;
      __get_pairings tail;
    end;
;;


let get_observable_graph (files       : file_node list)
                         (nodes       : message_node list) =
    begin
      print_string ("Files:\n");
      print_file_node_list files; 
      __get_pairings files;
      nodes;
    end;
;;

