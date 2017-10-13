open File_node
open Message_node

val get_message_graph           : file_node list    -> message_node list
val strip_messages              : message_node list -> message_node list
val get_mapping                 : file_node list    -> (string * string) list
val map_mappings                : message_node list -> (string * string) list -> (message_node list)
val get_message_relations       : message_node list -> message_node list
val message_graph_to_dot_string : message_node list -> (string * string) list
val get_peer_path               : message_node list -> message_node list
