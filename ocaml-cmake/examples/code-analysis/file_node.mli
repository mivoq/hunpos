class type file_type =
  object
    method get_short : string
    method set_short : string -> unit
    method get_long  : string
    method set_long  : string -> unit
  end;;

class type file_node =
  object
    method set_loc : int -> unit
    method get_loc : int

    method set_loc_comments : int -> unit
    method get_loc_comments : int

    method set_loc_nontrivial : int -> unit
    method get_loc_nontrivial : int

    method set_loc_trivial : int -> unit
    method get_loc_trivial : int

    method set_name : string -> unit
    method get_name : string

    method set_path : string -> unit
    method get_path : string

    method set_full_name : string -> unit
    method get_full_name : string

    method set_file_type : file_type -> unit
    method get_file_type : file_type

  end;;

val make_fn : unit -> file_node
val make_ft : unit -> file_type

val file_type_ascii : file_type
val file_type_xml   : file_type
val file_type_cmake : file_type
val file_type_cpp   : file_type
val file_type_java  : file_type
val file_type_ml    : file_type
val file_type_tex   : file_type
val file_type_bib   : file_type
val file_type_pl    : file_type
val file_type_none  : file_type

