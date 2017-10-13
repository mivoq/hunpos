cmake_minimum_required(VERSION 2.6)

project(hello-world NONE)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/../../cmake")

find_package(OCaml REQUIRED)

include(UseOCaml)

add_ocaml_executable(main SOURCES main)

add_ocaml_executable(native NATIVE SOURCES main)
