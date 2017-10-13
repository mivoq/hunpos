open File_node
open Message_node

val get_observable_graph : File_node.file_node list ->
                           Message_node.message_node list ->
                           Message_node.message_node list
