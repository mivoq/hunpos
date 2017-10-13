cmake_minimum_required(VERSION 2.6)

project(hello-world C)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/../../cmake")

find_package(OCaml REQUIRED)

include(UseOCaml)

add_ocaml_executable(main-bytecode BYTECODE SOURCES hello.ml main_ocaml.ml LINK_FLAGS -custom)

add_ocaml_executable(main-native NATIVE SOURCES hello.ml main_ocaml.ml)

add_ocaml_library(hello_world NATIVE SOURCES hello.ml)

add_ocaml_library(hello_world-bytecode BYTECODE SOURCES hello.ml)

add_ocaml_executable(main2-native NATIVE SOURCES main_ocaml.ml PACKAGES hello_world)

add_ocaml_executable(main2-bytecode BYTECODE SOURCES main_ocaml.ml PACKAGES hello_world-bytecode LINK_FLAGS -custom)

add_ocaml_library(hello_world_wrap NATIVE SOURCES hellowrap_ocaml.ml PACKAGES hello_world)
add_ocaml_c_object(libhelloworld NATIVE SOURCES hellowrap-c.c hello.h PACKAGES hello_world hello_world_wrap C_FLAGS -DHELLO_LIBRARIES_EXPORTS)

add_executable(main-libhelloworld main.c)
target_compile_definitions(main-libhelloworld PRIVATE HELLO_LIBRARIES_EXPORTS)
target_link_libraries(main-libhelloworld libhelloworld)

add_ocaml_library(hello_world_wrap-bytecode BYTECODE SOURCES hellowrap_ocaml.ml PACKAGES hello_world-bytecode)
add_ocaml_c_object(libhelloworld-bytecode BYTECODE SOURCES hellowrap-c.c hello.h PACKAGES hello_world-bytecode hello_world_wrap-bytecode C_FLAGS -DHELLO_LIBRARIES_EXPORTS)

add_executable(main-libhelloworld-bytecode main.c)
target_compile_definitions(main-libhelloworld-bytecode PRIVATE HELLO_LIBRARIES_EXPORTS)
target_link_libraries(main-libhelloworld-bytecode libhelloworld-bytecode)


enable_testing()
add_test(main2-bytecode ${TARGET_SYSTEM_EMULATOR} main2-bytecode${CMAKE_EXECUTABLE_SUFFIX})
set_tests_properties(main2-bytecode PROPERTIES PASS_REGULAR_EXPRESSION "^Hello world\n$")
add_test(main2-native ${TARGET_SYSTEM_EMULATOR} main2-native${CMAKE_EXECUTABLE_SUFFIX})
set_tests_properties(main2-native PROPERTIES PASS_REGULAR_EXPRESSION "^Hello world\n$")
add_test(main-bytecode ${TARGET_SYSTEM_EMULATOR} main-bytecode${CMAKE_EXECUTABLE_SUFFIX})
set_tests_properties(main-bytecode PROPERTIES PASS_REGULAR_EXPRESSION "^Hello world\n$")
add_test(main-libhelloworld-bytecode ${TARGET_SYSTEM_EMULATOR} main-libhelloworld-bytecode${CMAKE_EXECUTABLE_SUFFIX})
set_tests_properties(main-libhelloworld-bytecode PROPERTIES PASS_REGULAR_EXPRESSION "^Hello world\n$")
add_test(main-libhelloworld ${TARGET_SYSTEM_EMULATOR} main-libhelloworld${CMAKE_EXECUTABLE_SUFFIX})
set_tests_properties(main-libhelloworld PROPERTIES PASS_REGULAR_EXPRESSION "^Hello world\n$")
add_test(main-native ${TARGET_SYSTEM_EMULATOR} main-native${CMAKE_EXECUTABLE_SUFFIX})
set_tests_properties(main-native PROPERTIES PASS_REGULAR_EXPRESSION "^Hello world\n$")
