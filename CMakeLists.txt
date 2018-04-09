cmake_minimum_required(VERSION 3.0)
cmake_policy(SET CMP0048 NEW)

set(HUNPOS_MAJOR_VERSION 1)
set(HUNPOS_MINOR_VERSION 0)
set(HUNPOS_PATCH_VERSION 0)
set(HUNPOS_VERSION ${HUNPOS_MAJOR_VERSION}.${HUNPOS_MINOR_VERSION}.${HUNPOS_PATCH_VERSION})
project(hunpos VERSION ${HUNPOS_VERSION} LANGUAGES C)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/ocaml-cmake/cmake")

find_package(OCaml REQUIRED)

include(UseOCaml)

find_ocaml_package(str)

if(NOT CMAKE_INSTALL_BINDIR)
  set(CMAKE_INSTALL_BINDIR "bin")
endif()
if(NOT CMAKE_INSTALL_INCLUDEDIR)
  set(CMAKE_INSTALL_INCLUDEDIR "include")
endif()

if(${CMAKE_OCaml_VERSION} LESS "4.02")
  set(CMAKE_OCaml_FLAGS -pp ${PROJECT_SOURCE_DIR}/ocaml-preprocess)
endif()

add_ocaml_library(hunpos NATIVE SOURCES
util/amap.ml
util/parse.ml
util/mfhash.ml
util/vocab.mli
util/vocab.ml

hunpos/io.ml
hunpos/lexicon.ml
hunpos/linear_interpolated_lm.ml
hunpos/morphtable.ml
hunpos/ngram.ml
hunpos/special_tokens.ml
hunpos/suffix_guesser.ml
hunpos/viterbi.ml
hunpos/hmm_tagger.ml
PACKAGES str
)
install_ocaml_targets (hunpos DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/ocaml/hunpos)

add_ocaml_library(hunpos-byte BYTECODE SOURCES
util/amap.ml
util/parse.ml
util/mfhash.ml
util/vocab.mli
util/vocab.ml

hunpos/io.ml
hunpos/lexicon.ml
hunpos/linear_interpolated_lm.ml
hunpos/morphtable.ml
hunpos/ngram.ml
hunpos/special_tokens.ml
hunpos/suffix_guesser.ml
hunpos/viterbi.ml
hunpos/hmm_tagger.ml
PACKAGES str
)
install_ocaml_targets (hunpos-byte DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/ocaml/hunpos)
install_ocaml_interfaces (hunpos-byte hmm_tagger morphtable DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/ocaml/hunpos)
#install_ocaml_interfaces (hunpos-byte hmm_tagger morphtable viterbi suffix_guesser ngram vocab mfhash amap parse special_tokens io lexicon linear_interpolated_lm DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/ocaml/hunpos)

add_ocaml_library(hunposwrap NATIVE SOURCES
clib/hunposcaml.ml
PACKAGES str hunpos
)
add_ocaml_c_object(libhunpos NATIVE SOURCES
  clib/hunposwrap.c
  clib/hunpos.h
  #clib/hunposcaml.ml
  PACKAGES str hunpos hunposwrap
  C_FLAGS -DHUNPOS_LIBRARIES_EXPORTS
)
install_ocaml_targets (libhunpos DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install (FILES clib/hunpos.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

add_executable(test-libhunpos clib/test.c)
if (NOT CMAKE_VERSION VERSION_LESS 2.8.12)
  target_include_directories(test-libhunpos PRIVATE clib)
else()
  include_directories(clib)
endif()
target_compile_definitions(test-libhunpos PRIVATE HUNPOS_LIBRARIES_EXPORTS)
target_link_libraries(test-libhunpos libhunpos)

add_ocaml_executable(hunpos-train NATIVE SOURCES
hunpos/lib/getopt.mli
hunpos/lib/getopt.ml
hunpos/trainer.ml
PACKAGES str hunpos
)
install_ocaml_targets (hunpos-train DESTINATION ${CMAKE_INSTALL_BINDIR})

add_ocaml_executable(hunpos-tag NATIVE SOURCES
hunpos/lib/getopt.mli
hunpos/lib/getopt.ml
hunpos/tagger.ml
PACKAGES str hunpos
)
install_ocaml_targets (hunpos-tag DESTINATION ${CMAKE_INSTALL_BINDIR})

# Create the Config.cmake and ConfigVersion.cmake files
get_filename_component(CMAKE_INSTALL_INCLUDEDIR_ABS ${CMAKE_INSTALL_INCLUDEDIR} ABSOLUTE BASE_DIR ${CMAKE_INSTALL_PREFIX})
get_property(CONF_LIBRARIES TARGET libhunpos PROPERTY IMPORTED_LOCATION)
get_filename_component(CONF_LIBRARY_SO_FILE ${CONF_LIBRARIES} NAME)
set(CONF_LIBRARIES libhunpos)
set(CONF_INCLUDE_DIRS ${CMAKE_INSTALL_INCLUDEDIR_ABS})
configure_file(hunposConfig.cmake.in "cmake/hunposConfig.cmake" @ONLY)
configure_file(hunposConfigVersion.cmake.in "cmake/hunposConfigVersion.cmake" @ONLY)
configure_file(libhunpos-targets.cmake-install.in "cmake/libhunpos-targets.cmake" @ONLY)
configure_file(libhunpos-targets-noconfig.cmake-install.in "cmake/libhunpos-targets-noconfig.cmake" @ONLY)
install(FILES "${PROJECT_BINARY_DIR}/cmake/hunposConfig.cmake" "${PROJECT_BINARY_DIR}/cmake/hunposConfigVersion.cmake" "${PROJECT_BINARY_DIR}/cmake/libhunpos-targets.cmake" "${PROJECT_BINARY_DIR}/cmake/libhunpos-targets-noconfig.cmake" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/hunpos" COMPONENT dev)

get_property(CONF_LIBRARIES TARGET libhunpos PROPERTY IMPORTED_LOCATION)
set(CONF_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/clib "${PROJECT_BINARY_DIR}")
configure_file(hunposConfig.cmake.in "hunposConfig.cmake" @ONLY)
configure_file(hunposConfigVersion.cmake.in "${PROJECT_BINARY_DIR}/hunposConfigVersion.cmake" @ONLY)
configure_file(libhunpos-targets.cmake.in "${PROJECT_BINARY_DIR}/libhunpos-targets.cmake" @ONLY)
export(PACKAGE hunpos)

add_ocaml_executable(hunpos-tag-byte BYTECODE SOURCES
hunpos/lib/getopt.mli
hunpos/lib/getopt.ml
hunpos/tagger.ml
PACKAGES str hunpos-byte
LINK_FLAGS -custom
)
add_ocaml_executable(hunpos-train-byte BYTECODE SOURCES
hunpos/lib/getopt.mli
hunpos/lib/getopt.ml
hunpos/tagger.ml
PACKAGES str hunpos-byte
LINK_FLAGS -custom
)
