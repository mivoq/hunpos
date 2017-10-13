cmake_minimum_required(VERSION 2.6)

project(code-analysis NONE)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/../../cmake")

find_package(OCaml REQUIRED)

include(UseOCaml)

find_ocaml_package(extlib)

add_ocaml_library(common
  SOURCES message_node file_node files io
  PACKAGES unix str extlib
  )

add_ocaml_executable(loc
  SOURCES file_statistics loc
  LIBRARIES common
  )

add_ocaml_executable(messages
  SOURCES observables_graph message_graph messages
  LIBRARIES common
  )

add_ocaml_executable(loc2 NATIVE
  SOURCES message_node file_node files io file_statistics loc
  PACKAGES unix str extlib
  )

add_ocaml_executable(messages2 BYTECODE
  SOURCES
    file_node.ml
    files.ml
    message_node.ml
    io.ml
    observables_graph.ml
    message_graph.ml
    file_node.mli
    files.mli
    message_node.mli
    io.mli
    observables_graph.mli
    message_graph.mli
    messages.ml
  PACKAGES unix str extlib
  )

install_ocaml_targets(common loc messages loc2 messages2 DESTINATION lib/ocaml/site-lib/code-analysis)

install_ocaml_interfaces(loc2 message_node file_node DESTINATION lib/ocaml/site-lib/code-analysis)

install_ocaml_exports(common loc messages loc2 messages2 DESTINATION lib/ocaml/site-lib/code-analysis FILE code-analysis.cmake)
