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

class message_node_implementation =
  object(self)
    val mutable _class_name         = ""
    val mutable _messages_in        = ([] : string list)
    val mutable _messages_out       = ([] : string list)
    val mutable _sending_to         = ([] : message_node list)
    val mutable _receiving_from     = ([] : message_node list)
    val mutable _path_length_to     = ([] : message_node list)
    val mutable _path_length_from   = ([] : message_node list)
    val mutable _observers          = ([] : message_node list)
    val mutable _observables        = ([] : message_node list)

    method set_class_name n         = _class_name <- n
    method get_class_name           = _class_name

    method set_messages_in lst      = _messages_in <- lst
    method get_messages_in          = _messages_in

    method set_messages_out lst     = _messages_out <- lst
    method get_messages_out         = _messages_out

    method set_sending_to lst       = _sending_to <- lst
    method get_sending_to           = _sending_to

    method set_receiving_from lst   = _receiving_from <- lst
    method get_receiving_from       = _receiving_from

    method set_path_length_to lst   = _path_length_to <- lst
    method get_path_length_to       = _path_length_to

    method set_path_length_from lst = _path_length_from <- lst
    method get_path_length_from     = _path_length_from

    method set_observers lst        = _observers <- lst
    method get_observers            = _observers

    method set_observables lst      = _observables <- lst
    method get_observables          = _observables

  end;;

  
let make_mn () : message_node = ( new message_node_implementation :> message_node)

