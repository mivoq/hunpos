open Unix.LargeFile
open Unix
open Str
open File_node

val strip_file_list            : File_node.file_node list -> File_node.file_node list
val read_files                 : string -> Unix.dir_handle -> file_node list -> Str.regexp -> file_node list 
val get_all_files              : string -> Str.regexp -> file_node list

val parse                      : File_node.file_node list -> (File_node.file_node * string list) list
val get_messages               : string list -> string list -> string list
val strip_file_list_by_content : File_node.file_node list -> Str.regexp -> File_node.file_node list
val strip_file_list_by_name    : File_node.file_node list -> Str.regexp -> File_node.file_node list

