open File_node
open Message_node

(** print a list of string list with prefix and carriage return *) 
val print_string_list             : string -> string list -> unit

(** print a list file file_node entries in the form of:
    <file name>   located at <path>
    ...
*) 
val print_file_node_list          : File_node.file_node list -> unit

(** print a list file message_nodes with all information *)
val print_message_nodes           : Message_node.message_node list -> unit

(** print a list file message_nodes, but only the node names *)
val print_message_nodes_short     : Message_node.message_node list -> unit

(**
    prints a pair of string such as ("first", "second") with an optional prefix:
    first:
    second
*)
val print_string_pair_list        : string -> (string * string) list -> unit

(**
  * prints a pair of string and string list in the form of
  * head:
  *   entry
  *   entry
  *   ...
  *
  * with a prefix
*)
val print_string_string_pair_list : string -> (string * (string list)) list -> unit
val print_loc_stats               : (File_node.file_node list) -> unit
val write_loc_stats               : (File_node.file_node list) -> string -> unit
val write_files                   : string -> (string * string) list -> string -> unit
val clean_directory               : string -> unit
val remove_files                  : File_node.file_node list -> unit
val pre                           : int -> string -> string
