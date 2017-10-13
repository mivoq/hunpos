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

    method set_full_name : string -> unit
    method get_full_name : string

    method set_file_type : file_type -> unit
    method get_file_type : file_type
  end;;


class file_type_implementation =
  object
    val mutable _short = ""
    val mutable _long  = ""
    method get_short   = _short
    method set_short n = _short <- n
    method get_long    = _long
    method set_long n  = _long <- n
  end;;

let make_ft () : file_type = ( new file_type_implementation :> file_type_implementation)

let make_new_file_type short long = 
let f = make_ft() in
  begin
    f#set_short short;
    f#set_long  long;
    f;
  end;
;;

let file_type_ascii = make_new_file_type "ascii" "An Ascii file.";;
let file_type_xml   = make_new_file_type "xml"   "XML or XSD file.";;
let file_type_cmake = make_new_file_type "cmake" "CMake module or CMakeLists.txt.";;
let file_type_cpp   = make_new_file_type "cpp"   "Cpp source file or header.";;
let file_type_java  = make_new_file_type "java"  "A Java Source file.";;
let file_type_tex   = make_new_file_type "tex"   "TeX file.";;
let file_type_bib   = make_new_file_type "bib"   "Bibtex file.";;
let file_type_pl    = make_new_file_type "pl"    "Perl file.";;
let file_type_ml    = make_new_file_type "ocaml" "OCaml source or header.";;
let file_type_none  = make_new_file_type "none"  "UNDEFINED.";;


class file_node_implementation =
  object
    val mutable _loc            = -1
    val mutable _loc_comments   = -1
    val mutable _loc_nontrivial = -1
    val mutable _loc_trivial    = -1
    val mutable _full_name      = ""
    val mutable _name           = ""
    val mutable _path           = ""
    val mutable _full_path      = ""
    val mutable _file_type      = file_type_ascii
    method get_loc              = _loc
    method set_loc l            = _loc            <- l
    method get_loc_comments     = _loc_comments
    method set_loc_comments l   = _loc_comments   <- l
    method get_loc_nontrivial   = _loc_nontrivial 
    method set_loc_nontrivial l = _loc_nontrivial <- l
    method get_loc_trivial      = _loc_trivial 
    method set_loc_trivial l    = _loc_trivial <- l
    method get_path             = _path
    method set_path p           = _path <- p
    method get_name             = _name
    method set_name n           = _name <- n
    method get_full_name        = _full_name
    method set_full_name n      = _full_name <- n
    method set_file_type f      = _file_type <- f
    method get_file_type        = _file_type
  end;;

let make_fn () : file_node = ( new file_node_implementation :> file_node)

