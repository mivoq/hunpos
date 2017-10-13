class type message_node =
  object
    method set_class_name       : string -> unit
    method get_class_name       : string

    method set_messages_in      : (string list) -> unit
    method get_messages_in      : (string list)

    method set_messages_out     : (string list) -> unit
    method get_messages_out     : (string list)

    method set_sending_to       : (message_node list) -> unit
    method get_sending_to       : (message_node list)

    method set_receiving_from   : (message_node list) -> unit
    method get_receiving_from   : (message_node list)

    method set_path_length_to   : (message_node list) -> unit
    method get_path_length_to   : (message_node list)

    method set_path_length_from : (message_node list) -> unit
    method get_path_length_from : (message_node list)

    method set_observers        : (message_node list) -> unit
    method get_observers        : (message_node list)

    method set_observables      : (message_node list) -> unit
    method get_observables      : (message_node list)

  end;;

val make_mn : unit -> message_node
