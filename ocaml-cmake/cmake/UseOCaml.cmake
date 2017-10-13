# - Use OCaml
# Provide useful macros for OCaml.
#
# The following cache variables are defined:
#   CMAKE_OCaml_FLAGS
#   CMAKE_OCaml_FLAGS_DEBUG
#   CMAKE_OCaml_FLAGS_MINSIZEREL
#   CMAKE_OCaml_FLAGS_RELEASE
#   CMAKE_OCaml_FLAGS_RELWITHDEBINFO
#   CMAKE_OCaml_LINKER_FLAGS
#   CMAKE_OCaml_LINKER_FLAGS_DEBUG
#   CMAKE_OCaml_LINKER_FLAGS_MINSIZEREL
#   CMAKE_OCaml_LINKER_FLAGS_RELEASE
#   CMAKE_OCaml_LINKER_FLAGS_RELWITHDEBINFO
#   CMAKE_OCaml_NATIVE, specify default compiler-mode: native or bytecode.
#
# The following macros are defined:
#
# find_ocaml_package (<name>)
#   Use ocamlfind to find an OCaml package.
#   Variables ${name}_INCLUDE_DIRS and ${name}_LIBRARIES are set.
#   Cache variables ${name}_INCLUDE_DIR and ${name}_LIBRARY are also set.
#
# add_ocaml_executable (<name> [NATIVE | BYTECODE] source1 source2 ... sourceN)
#   sourcefiles should be mli or ml files.
#   To generate the executable, you have to call target_link_ocaml_libraries.
#   Sets the OCAML_${name}_NATIVE variable.
#   To specify the include directories, use the standard macro include_directories.
#
# add_ocaml_library (<name> [NATIVE | BYTECODE] source1 source2 ... sourceN)
#   sourcefiles should be mli or ml files.
#   To generate the library, you have to call target_link_ocaml_libraries.
#   Sets the OCAML_${name}_NATIVE variable.
#   To specify the include directories, use the standard macro include_directories.
#
# target_link_ocaml_libraries (<name> lib1 lib2 ... libN)
#   There are four ways to add a library :
#   - If it is another library of the current project, just specify its name.
#   - If it is an exported library, include the export file and specify its name.
#   - If it is a standard library, just specify its name.
#   - For other libraries, give an absolute path to the library.
#   Library dependencies are transitive.
#   Also set properties on target ocaml.${name}. Properties are
#   - KIND: a constant string which is equal to "EXECUTABLE" or "LIBRARY".
#   - LOCATION: indicates where the target is located.
#   - LINK_INTERFACE_LIBRARIES: indicates with which libraries the current library must be linked. Empty for an executable.
#   - OUTPUT_NAME: real name of the target.
#
# add_ocaml_target (<name>
#                   MAIN      <main>
#                   SOURCES   source1 source2 ... sourceN
#                   HEADERS   header1 header2 ... headerN
#                   LIBRARIES lib1 lib2 ... libN
#                   INCLUDES  include1 include2 ... includeN
#                  )
#   A shortcut macro for add_ocaml_executable, add_ocaml_library and target_link_ocaml_libraries.
#   If MAIN is specified, the target is considered as an executable.
#
# install_ocaml_targets (target1 target2 ... targetN DESTINATION <dir>)
#   Generates installation rules for OCaml targets.
#   Set the INSTALL_LOCATION property.
#
# install_ocaml_interfaces (<target> interfacename1 interfacename2 ... interfacenameN DESTINATION <dir>)
#   Installs CMI or CMX files according to the variable OCAML_${target}_NATIVE.
#
# install_ocaml_exports (target1 target2 ... targetN DESTINATION <dir> FILE <export-file>)
#   Generates and installs a CMake file containing code to import OCaml targets from the installation tree.
#
# gen_ocaml_lexers (outfilesname lexsources)
#   For each lex source, generates OCaml code by calling ocamllex.
#   The name of the result sources are put into the variable ${outfilesname}.
#   Because of dependency reasons, the OCaml code is also generated at the
# first configuration time.
#
# gen_ocaml_parsers (outfilesname yaccsources)
#   For each yacc source, generates OCaml code by calling ocamlyacc.
#   The name of the result sources are put into the variable ${outfilesname}.
#   Because of dependency reasons, the OCaml code is also generated at the
# first configuration time.
#
# TODO : see if it is possible to call the dependency generator at compile time
# before compiling source files but after generating some source files.
#
# Copyright (c) 2010, Judicaël Bedouet, j dot bedouet at infonie dot fr.
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

if(NOT OCAML_FOUND)
  message(WARNING "Please, find OCaml before including UseOCaml")
  return()
endif()

get_filename_component (CMAKE_USE_OCAML_DIR  "${CMAKE_CURRENT_LIST_FILE}" PATH)
set (CMAKE_OCAML_DEP_FILE "${CMAKE_USE_OCAML_DIR}/OCamlDep.cmake")

if (NOT DEFINED CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE "" CACHE STRING "CMake build type: none, debug, minsizerel, release or relwithdebinfo")
endif (NOT DEFINED CMAKE_BUILD_TYPE)

if (CMAKE_BUILD_TYPE)
  string (TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_UPPER)
endif (CMAKE_BUILD_TYPE)

option (CMAKE_OCaml_NATIVE "Compile OCaml targets with native compiler")
option (CMAKE_OCaml_USE_OCAML_TRACE "Run the script UseOCaml.cmake in trace mode")

set(MSGQ "Command used to find OCaml libraries")
if(CMAKE_OCaml_FIND)
  set(CMAKE_OCaml_CMD_QUERY ${CMAKE_OCaml_FIND} query CACHE STRING ${MSGQ})
endif()

set(MSGC "Command used for bytecode targets")
if(CMAKE_OCaml_COMPILER)
  set(CMAKE_OCaml_CMD_COMPILER ${CMAKE_OCaml_COMPILER} CACHE STRING ${MSGC})
elseif(CMAKE_OCaml_FIND)
  set(CMAKE_OCaml_CMD_COMPILER ${CMAKE_OCaml_FIND} ocamlc CACHE STRING ${MSGC})
endif()

set(MSGOC "Command used for optimized targets")
if(CMAKE_OCaml_OPT_COMPILER)
  set(CMAKE_OCaml_CMD_OPT_COMPILER ${CMAKE_OCaml_OPT_COMPILER} CACHE STRING ${MSGOC})
elseif(CMAKE_OCaml_FIND)
  set(CMAKE_OCaml_CMD_OPT_COMPILER ${CMAKE_OCaml_FIND} ocamlopt CACHE STRING ${MSGOC})
endif()

set(MSGDP "Command used to generate dependencies")
if(CMAKE_OCaml_DEP)
  set(CMAKE_OCaml_CMD_DEP ${CMAKE_OCaml_DEP} CACHE STRING ${MSGDP})
elseif(CMAKE_OCaml_FIND)
  set(CMAKE_OCaml_CMD_DEP ${CMAKE_OCaml_FIND} ocamldep -modules CACHE STRING ${MSGDP})
endif()

set (CMAKE_OCaml_FLAGS ""
  CACHE STRING "Flags used by the compiler during all build types"
  )
set (CMAKE_OCaml_FLAGS_DEBUG -g
  CACHE STRING "Flags used by the compiler during debug builds"
  )
set (CMAKE_OCaml_FLAGS_MINSIZEREL -ccopt -Os
  CACHE STRING "Flags used by the compiler during minsizerel builds"
  )
set (CMAKE_OCaml_FLAGS_RELEASE -noassert -unsafe -ccopt -O3
  CACHE STRING "Flags used by the compiler during release builds"
  )
set (CMAKE_OCaml_FLAGS_RELWITHDEBINFO -g -ccopt -O2
  CACHE STRING "Flags used by the compiler during relwithdebinfo builds"
  )

set (CMAKE_OCaml_LINKER_FLAGS ""
  CACHE STRING "Flags used for linking binaries during all build types"
  )
set (CMAKE_OCaml_LINKER_FLAGS_DEBUG -g
  CACHE STRING "Flags used for linking binaries during debug builds"
  )
set (CMAKE_OCaml_LINKER_FLAGS_MINSIZEREL ""
  CACHE STRING "Flags used for linking binaries during minsizerel builds"
  )
set (CMAKE_OCaml_LINKER_FLAGS_RELEASE ""
  CACHE STRING "Flags used for linking binaries during release builds"
  )
set (CMAKE_OCaml_LINKER_FLAGS_RELWITHDEBINFO -g
  CACHE STRING "Flags used for linking binaries during relwithdebinfo builds"
  )

mark_as_advanced (
  CMAKE_OCaml_FLAGS
  CMAKE_OCaml_FLAGS_DEBUG
  CMAKE_OCaml_FLAGS_MINSIZEREL
  CMAKE_OCaml_FLAGS_RELEASE
  CMAKE_OCaml_FLAGS_RELWITHDEBINFO
  CMAKE_OCaml_LINKER_FLAGS
  CMAKE_OCaml_LINKER_FLAGS_DEBUG
  CMAKE_OCaml_LINKER_FLAGS_MINSIZEREL
  CMAKE_OCaml_LINKER_FLAGS_RELEASE
  CMAKE_OCaml_LINKER_FLAGS_RELWITHDEBINFO
  )


### WORKAROUNDS flags definition - start

# TODO: remove the following block that is probably better handled in UseOCaml.cmake
set(CMAKE_OCaml_workaround_so_c_flags "")
set(CMAKE_OCaml_workaround_so_link_flags "")
set(CMAKE_OCaml_workaround_so_link_flags_bytecode "")
# TODO: remove this line that is needed just to load ocaml libraries that contains registered callbacks
list (APPEND CMAKE_OCaml_workaround_so_link_flags -linkall)
if(${CMAKE_OCaml_CONFIG_os_type} MATCHES "[wW][iI][nN]32")
  if(${CMAKE_OCaml_CONFIG_native_c_compiler} MATCHES "[gG][cC][cC]")
    list (APPEND CMAKE_OCaml_workaround_so_link_flags -cclib "-link -static-libgcc")
    if("${CMAKE_OCaml_CONFIG_bytecomp_c_libraries}" STREQUAL "")
      list (APPEND CMAKE_OCaml_workaround_so_link_flags_bytecode -cclib "-lws2_32")
    endif()
  endif()
elseif(${CMAKE_OCaml_CONFIG_system} MATCHES "[lL][iI][nN][uU][xX]")
  if("${CMAKE_OCaml_CONFIG_architecture}" STREQUAL "amd64")
    list (APPEND CMAKE_OCaml_workaround_so_c_flags -fPIC)
    if(NOT ${CMAKE_OCaml_VERSION} VERSION_LESS "4.00")
      list (APPEND CMAKE_OCaml_workaround_so_link_flags -runtime-variant _pic)
    endif()
  endif()
elseif(${CMAKE_OCaml_CONFIG_system} MATCHES "[mM][aA][cC][oO][sS][xX]")
  if("${CMAKE_OCaml_CONFIG_architecture}" STREQUAL "amd64")
    list (APPEND CMAKE_OCaml_workaround_so_link_flags -cclib -dynamiclib)
  endif()
else()
endif()
#MESSAGE("so_c_flags             (internal): ${CMAKE_OCaml_workaround_so_c_flags}")
#MESSAGE("so_link_flags          (internal): ${CMAKE_OCaml_workaround_so_link_flags}")
#MESSAGE("so_link_flags_bytecode (internal): ${CMAKE_OCaml_workaround_so_link_flags_bytecode}")

### WORKAROUNDS flags definition - stop


function (ocaml_parse_macro_arguments prefix arg_names)
  set (current_arg "FIRST_ARGS")
  set (${prefix}_${current_arg})
  foreach (arg ${ARGN})
    list (FIND arg_names ${arg} idx)
    if (idx LESS 0)   # Add an argument to the current option
      list (APPEND ${prefix}_${current_arg} ${arg})
    else (idx LESS 0) # Discover a new option
      list (LENGTH ${prefix}_${current_arg} length)
      if (length EQUAL 0) # The previous option has no argument. It is considered as a boolean flag.
        set (${prefix}_${current_arg} TRUE PARENT_SCOPE)
      else (length EQUAL 0)
        set (${prefix}_${current_arg} "${${prefix}_${current_arg}}" PARENT_SCOPE)
      endif (length EQUAL 0)
      set (current_arg ${arg})
      set (${prefix}_${current_arg} "")
    endif (idx LESS 0)
  endforeach (arg)
  set (${prefix}_${current_arg} "${${prefix}_${current_arg}}" PARENT_SCOPE)
endfunction (ocaml_parse_macro_arguments)

function (capitalize arg ret)
  string (SUBSTRING ${arg} 0 1 first)
  string (TOUPPER ${first} ufirst)
  string (REGEX REPLACE "^(.)(.+)$" "${ufirst}\\2" ${ret} ${arg})
  set (${ret} ${${ret}} PARENT_SCOPE)
endfunction (capitalize)

function (uncapitalize arg ret)
  string (SUBSTRING ${arg} 0 1 first)
  string (TOLOWER ${first} lfirst)
  string (REGEX REPLACE "^(.)(.+)$" "${lfirst}\\2" ${ret} ${arg})
  set (${ret} ${${ret}} PARENT_SCOPE)
endfunction (uncapitalize)

macro (find_ocaml_package name)

  string (TOUPPER ${name} name_upper)

  include (FindPackageHandleStandardArgs)

  if (CMAKE_OCaml_FIND)

    execute_process (
      COMMAND         ${CMAKE_OCaml_CMD_QUERY} ${name}
      OUTPUT_VARIABLE ${name_upper}_INCLUDE_DIR
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )

    execute_process (
      COMMAND    ${CMAKE_OCaml_CMD_QUERY} -format "%v" ${name}
      OUTPUT_VARIABLE ${name_upper}_VERSION
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )

    execute_process (
      COMMAND    ${CMAKE_OCaml_CMD_QUERY} -format "%a" -predicates byte ${name}
      OUTPUT_VARIABLE ${name_upper}_BYTE_FILE
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )

    execute_process (
      COMMAND    ${CMAKE_OCaml_CMD_QUERY} -format "%a" -predicates native ${name}
      OUTPUT_VARIABLE ${name_upper}_NATIVE_FILE
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )

    find_package_handle_standard_args (${name} DEFAULT_MSG
      ${name_upper}_VERSION
      )

  else()

    set (${name_upper}_INCLUDE_DIR ${${name_upper}_INCLUDE_DIR} CACHE PATH "")
    set (${name_upper}_LIBRARY_DIR ${${name_upper}_INCLUDE_DIR} CACHE PATH "")

    find_package_handle_standard_args (${name} DEFAULT_MSG
      ${name_upper}_INCLUDE_DIR
      ${name_upper}_LIBRARY_DIR
      )

    mark_as_advanced (
      ${name_upper}_INCLUDE_DIR
      ${name_upper}_LIBRARY_DIR
      )

    if (${name_upper}_FOUND)
      set (${name_upper}_INCLUDE_DIRS ${${name_upper}_INCLUDE_DIR})
      set (${name_upper}_LIBRARY_DIRS ${${name_upper}_LIBRARY_DIR})
    endif (${name_upper}_FOUND)

  endif()

endmacro (find_ocaml_package name)

# get_ocaml_dependencies (target filename includecmi dep)
#   Generates several files which contains the dependencies for the file ${filename}.
#   The CMake dependency file, ${filename}.dep.cmake is generated in the directory
# ${CMAKE_CURRENT_BINARY_DIR}/Dependencies/
#   The native argument is used for interface files. Indeed, the CMI file produced
# for an interface file is the same file but it could depend on CMO files or CMX files.
function (get_ocaml_dependencies target filename impl hasintf dep)

  if (CMAKE_OCaml_USE_OCAML_TRACE)
    message (STATUS "get_ocaml_dependencies (${target} ${filename} ${impl} ${hasintf})")
  endif (CMAKE_OCaml_USE_OCAML_TRACE)

  get_filename_component (name    ${filename} NAME)
  get_filename_component (name_we ${filename} NAME_WE)

  set (${dep})

  execute_process (
    COMMAND ${CMAKE_COMMAND}
      -D ocamldep=${CMAKE_OCaml_DEP}
      -D ocamlfind=${CMAKE_OCaml_FIND}
      -D filename=${filename}
      -D output=${OCAML_${target}_OUTPUT_DIR}
      -P ${CMAKE_OCAML_DEP_FILE}
    )

  include ("${OCAML_${target}_OUTPUT_DIR}/Dependencies/${name}.dep.cmake")

  separate_arguments (${name}_DEPENDS)

  # For each dependency, looking for the real file.
  foreach (depend ${${name}_DEPENDS})

    set (location)
    uncapitalize (${depend} depend_name_we)

    # Looking for the real file in the sources of the target.
    foreach (source ${OCAML_${target}_SOURCES})
      get_filename_component (source_name_we ${source} NAME_WE)
      capitalize (${source_name_we} usource_name_we)
      if(usource_name_we STREQUAL ${depend})
        set(location "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${source_name_we}.cmi")
        break()
      endif()
    endforeach (source)

    # Looking for the real file in the sources of the dependent targets, which are OCaml libraries.
    if (NOT location)
      foreach (targetdepend ${OCAML_${target}_OCAML_TARGET_LIBRARIES})
        get_target_property (sources ocaml.${targetdepend} OCAML_SOURCES)
        get_target_property (obj_dir ocaml.${targetdepend} OBJECT_DIRECTORY)
        foreach (source ${sources})
          get_filename_component (source_name_we ${source} NAME_WE)
          capitalize (${source_name_we} usource_name_we)
          if (usource_name_we STREQUAL ${depend})
            set (location "${obj_dir}/${source_name_we}.cmi")
            break ()
          endif (usource_name_we STREQUAL ${depend})
        endforeach (source)
      endforeach (targetdepend)
    endif (NOT location)

    # Looking for the real file in the include directories.
    if (NOT location)
      foreach (include ${OCAML_${target}_INCLUDE_DIRECTORIES})
        if (EXISTS "${include}/${depend_name_we}.cmi")
         set (location "${include}/${depend_name_we}.cmi")
          break ()
        elseif (EXISTS "${include}/${depend_name_we}.cmi")
          set (location "${include}/${depend}.cmi")
          break ()
        endif (EXISTS "${include}/${depend_name_we}.cmi")
      endforeach (include)
    endif (NOT location)

    # If the file has been found, add the CMI dependency.
    if (location)
      list (APPEND ${dep} "${location}")
    else (location)
      if (CMAKE_OCaml_USE_OCAML_TRACE)
        message (STATUS "Can't find location of the dependency ${depend} for ${target}")
      endif (CMAKE_OCaml_USE_OCAML_TRACE)
    endif (location)

  endforeach (depend)

  # Add the CMI dependency on the interface of this file.
  if (impl)
    if (hasintf)
      list (APPEND ${dep} "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.cmi")
    endif (hasintf)
  endif (impl)

  set (${dep} ${${dep}} PARENT_SCOPE)

  if (CMAKE_OCaml_USE_OCAML_TRACE)
    message (STATUS "Dependencies are ")
    foreach (dep ${${dep}})
      message (STATUS "  ${dep}")
    endforeach (dep)
    message (STATUS "")
  endif (CMAKE_OCaml_USE_OCAML_TRACE)

endfunction (get_ocaml_dependencies)


function (find_ocaml_package_in_targets target name outdep pkgincludedirs pkgfiles)
  if(NOT ${PKG}_BYTE_FILE AND NOT ${PKG}_NATIVE_FILE)
    if(OCAML_${name}_KIND)
      if(${OCAML_${name}_KIND} STREQUAL "LIBRARY")
        set(found FALSE)
        if(OCAML_${name}_NATIVE AND OCAML_${target}_NATIVE)
          set(found TRUE)
        elseif(NOT OCAML_${name}_NATIVE AND NOT OCAML_${target}_NATIVE)
          set(found TRUE)
        endif()
        if(found)
          get_target_property (location  ocaml.${name} LOCATION)
          get_target_property (object_directory  ocaml.${name} OBJECT_DIRECTORY)

          get_target_property (output_name  ocaml.${name} OUTPUT_NAME)
          #message("${location} ${output_name} ${object_directory}")
          set(${outdep} ocaml.${name} PARENT_SCOPE)
          set(${pkgfiles} ${location} PARENT_SCOPE)
          set(${pkgincludedirs} ${object_directory} PARENT_SCOPE)
        endif()
      endif()
    endif()
  endif()
endfunction (find_ocaml_package_in_targets)

function (ocaml_get_packages_flags target libext include_flags package_flags intertarget_dependencies)
  set(tmp_include_flags)
  if(CMAKE_OCaml_STD_LIBRARY_PATH)
    list(APPEND tmp_include_flags -I "${CMAKE_OCaml_STD_LIBRARY_PATH}")
  endif()
  foreach (include ${OCAML_${target}_INCLUDE_DIRECTORIES})
    list (APPEND tmp_include_flags -I ${include})
  endforeach (include)
  set(${include_flags} ${tmp_include_flags} PARENT_SCOPE)

  set(tmp_package_flags)
  set(tmp_intertarget_dependencies)

  foreach(pkg ${OCAML_${target}_TRANSPKGS})
    string(TOUPPER ${pkg} PKG)
    if(${PKG}_INCLUDE_DIR)
      list(APPEND tmp_package_flags -I ${${PKG}_INCLUDE_DIR})
    endif()
    if (NOT OCAML_${target}_NATIVE AND ${PKG}_BYTE_FILE)
      list(APPEND tmp_package_flags ${${PKG}_BYTE_FILE})
    elseif (OCAML_${target}_NATIVE AND ${PKG}_NATIVE_FILE)
      list(APPEND tmp_package_flags ${${PKG}_NATIVE_FILE})
    else()
      find_ocaml_package_in_targets(${target} ${pkg} deptoadd pkgincludedirs pkgfiles)
      if(deptoadd)
        list(APPEND tmp_intertarget_dependencies ${deptoadd})
        list(APPEND tmp_package_flags -I ${pkgincludedirs})
        list(APPEND tmp_package_flags ${pkgfiles})
      else()
        list(APPEND tmp_package_flags ${pkg}${libext})
      endif()
    endif()

  endforeach()
  set(${package_flags} ${tmp_package_flags} PARENT_SCOPE)
  set(${intertarget_dependencies} ${tmp_intertarget_dependencies} PARENT_SCOPE)
endfunction (ocaml_get_packages_flags)

function (ocaml_var_to_flags variable option flags)
  set (tmpflag)
  foreach(flag ${variable})
    list(APPEND tmpflag ${option} ${flag})
  endforeach()
  set(${flags} ${tmpflag} PARENT_SCOPE)
endfunction (ocaml_var_to_flags variable option flags)

# ocaml_add_c_object_target (target native source objectname)
#   Compiles the Caml source ${source} to native or bytecode object.
#   The name of the object is written in the variable ${objectname}.
macro (ocaml_add_c_object_target target source objectname)

  get_filename_component (source_name_we ${source} NAME_WE)
  get_filename_component (source_name    ${source} NAME)
  get_filename_component (source_path    ${source} PATH)

  #if(CMAKE_OCaml_CONFIG_ext_cc_obj)
  #  set (object_ext    ${CMAKE_OCaml_CONFIG_ext_cc_obj})
  if(CMAKE_OCaml_CONFIG_ext_obj)
    set (object_ext    ${CMAKE_OCaml_CONFIG_ext_obj})
  else()
    set (object_ext    .o)
  endif()
  set (${objectname} ${OCAML_${target}_OUTPUT_DIR}/${source_name_we}${object_ext})
  set (output        ${${objectname}})
  if (OCAML_${target}_NATIVE)
    set (libext ".cmxa")
    set (compiler      ${CMAKE_OCaml_CMD_OPT_COMPILER})
  else (OCAML_${target}_NATIVE)
    set (libext ".cma")
    set (compiler      ${CMAKE_OCaml_CMD_COMPILER})
  endif (OCAML_${target}_NATIVE)

  ocaml_get_packages_flags(${target} ${libext} include_flags package_flags intertarget_dependencies)
  if (${OCAML_${target}_KIND} STREQUAL "C_OBJECT")
    list(APPEND OCAML_${target}_C_FLAGS ${CMAKE_OCaml_workaround_so_c_flags})
  endif()
    
  if(OCAML_${target}_C_FLAGS)
    ocaml_var_to_flags("${OCAML_${target}_C_FLAGS}" -ccopt c_flags)
  endif()

  if(${CMAKE_OCaml_CONFIG_ccomp_type} MATCHES "[mM][sS][vV][cC]")
    set(output_ccopt "/Fo${${objectname}}")
  else()
    set(output_ccopt "-o ${${objectname}}")
  endif()

  add_custom_command (OUTPUT ${output}
    COMMAND ${compiler}
      ${CMAKE_OCaml_FLAGS} ${CMAKE_OCaml_FLAGS_${CMAKE_BUILD_TYPE_UPPER}}
      ${include_flags} ${package_flags} ${c_flags}
      -ccopt "${output_ccopt}" -c ${source}

    MAIN_DEPENDENCY   ${source}
    DEPENDS           ${depends} ${intertarget_dependencies}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT           "Building OCaml object ${source_name_we}${object_ext}"
    )

  add_custom_target (${target}.${source_name_we}${object_ext} DEPENDS ${output})

endmacro (ocaml_add_c_object_target)

# ocaml_add_object_target (target native source hasintf objectname)
#   Compiles the Caml source ${source} to native or bytecode object.
#   The name of the object is written in the variable ${objectname}.
macro (ocaml_add_object_target target source hasintf objectname)

  get_filename_component (source_name_we ${source} NAME_WE)
  get_filename_component (source_name    ${source} NAME)
  get_filename_component (source_path    ${source} PATH)

  if (OCAML_${target}_NATIVE)
    set (object_ext    .cmx)
    set (libext ".cmxa")
    set (compiler      ${CMAKE_OCaml_CMD_OPT_COMPILER})
    set (${objectname} ${OCAML_${target}_OUTPUT_DIR}/${source_name_we}${object_ext})
    set (output        ${${objectname}} ${OCAML_${target}_OUTPUT_DIR}/${source_name_we}${CMAKE_OCaml_CONFIG_ext_obj})
  else (OCAML_${target}_NATIVE)
    set (object_ext    .cmo)
    set (libext ".cma")
    set (compiler      ${CMAKE_OCaml_CMD_COMPILER})
    set (${objectname} ${OCAML_${target}_OUTPUT_DIR}/${source_name_we}${object_ext})
    set (output        ${${objectname}})
  endif (OCAML_${target}_NATIVE)

  if (NOT hasintf)
    list (APPEND output ${OCAML_${target}_OUTPUT_DIR}/${source_name_we}.cmi)
  endif (NOT hasintf)

  get_ocaml_dependencies (${target} ${source} TRUE ${hasintf} depends)

  ocaml_get_packages_flags(${target} ${libext} include_flags package_flags intertarget_dependencies)
  if(OCAML_${target}_C_FLAGS)
    ocaml_var_to_flags("${OCAML_${target}_C_FLAGS}" -ccopt c_flags)
  endif()

  if(${OCAML_${target}_KIND} STREQUAL "C_OBJECT")
    #if(CMAKE_OCaml_CONFIG_ext_cc_obj)
    #  set (object_ext    ${CMAKE_OCaml_CONFIG_ext_cc_obj})
    if(CMAKE_OCaml_CONFIG_ext_obj)
      set (object_ext    ${CMAKE_OCaml_CONFIG_ext_obj})
    else()
      set (object_ext    .o)
    endif()
    set (${objectname} ${OCAML_${target}_OUTPUT_DIR}/${source_name_we}${object_ext})
    set (output        ${${objectname}})
    add_custom_command (OUTPUT ${output}
      COMMAND ${compiler}
      ${CMAKE_OCaml_FLAGS} ${CMAKE_OCaml_FLAGS_${CMAKE_BUILD_TYPE_UPPER}}
      ${include_flags} ${package_flags} ${c_flags}
      -o ${${objectname}} -output-obj ${source}
      
      MAIN_DEPENDENCY   ${source}
      DEPENDS           ${depends} ${intertarget_dependencies}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT           "Building OCaml object ${source_name_we}${object_ext}"
      )
  else(${OCAML_${target}_KIND} STREQUAL "C_OBJECT")
    add_custom_command (OUTPUT ${output}
      COMMAND ${compiler}
      ${CMAKE_OCaml_FLAGS} ${CMAKE_OCaml_FLAGS_${CMAKE_BUILD_TYPE_UPPER}}
      ${include_flags} ${package_flags} ${c_flags}
      -o ${${objectname}} -c -impl ${source}

      MAIN_DEPENDENCY   ${source}
      DEPENDS           ${depends} ${intertarget_dependencies}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT           "Building OCaml object ${source_name_we}${object_ext}"
      )
  endif(${OCAML_${target}_KIND} STREQUAL "C_OBJECT")

  add_custom_target (${target}.${source_name_we}${object_ext} DEPENDS ${output})

endmacro (ocaml_add_object_target)

# ocaml_add_interface_object_target (target source)
#   Compiles the Caml interface ${source}.
macro (ocaml_add_interface_object_target target source)

  get_filename_component (source_name_we ${source} NAME_WE)
  get_filename_component (source_name    ${source} NAME)
  get_filename_component (source_dir     ${source} PATH)
  if (OCAML_${target}_NATIVE)
    set (libext ".cmxa")
    set (compiler      ${CMAKE_OCaml_CMD_OPT_COMPILER})
  else (OCAML_${target}_NATIVE)
    set (libext ".cma")
    set (compiler      ${CMAKE_OCaml_CMD_COMPILER})
  endif (OCAML_${target}_NATIVE)

  set (output "${OCAML_${target}_OUTPUT_DIR}/${source_name_we}.cmi")

  get_ocaml_dependencies (${target} ${source} FALSE FALSE depends)

  ocaml_get_packages_flags(${target} ${libext} include_flags package_flags intertarget_dependencies)
  if(OCAML_${target}_C_FLAGS)
    ocaml_var_to_flags("${OCAML_${target}_C_FLAGS}" -ccopt c_flags)
  endif()

  add_custom_command (OUTPUT ${output}
    COMMAND ${compiler}
      ${CMAKE_OCaml_FLAGS} ${CMAKE_OCaml_FLAGS_${CMAKE_BUILD_TYPE_UPPER}}
      ${include_flags} ${package_flags} ${c_flags}
      -o ${output} -c -intf ${source}

    MAIN_DEPENDENCY   ${source}
    DEPENDS           ${depends} ${intertarget_dependencies}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT           "Building OCaml object ${source_name_we}.cmi"
    )

  add_custom_target (${target}.${source_name_we}.cmi DEPENDS ${output})

endmacro (ocaml_add_interface_object_target)

# add_ocaml_objects (name sourcefiles)
#   Add rules to compile source files in native or bytecode.
#   Set the OCAML_${name}_OBJECTS and OCAML_${name}_NATIVE variables.
#   The real target is created by target_link_ocaml_libraries.
macro (add_ocaml_objects target)

  set (OCAML_${target}_SOURCES)

  foreach (source ${${target}_SOURCES})

    set (sources ${source})

    get_source_file_property (impl ${source} OCAML_IMPL)
    get_source_file_property (intf ${source} OCAML_INTF)
    get_source_file_property (oc ${source} OCAML_C)

    if (NOT impl OR NOT intf OR NOT oc)
      get_filename_component (ext ${source} EXT)
      if (ext STREQUAL ".ml")
        set_source_files_properties (${source} PROPERTIES OCAML_IMPL TRUE)
      elseif (ext STREQUAL ".mli")
        set_source_files_properties (${source} PROPERTIES OCAML_INTF TRUE)
      elseif (ext STREQUAL ".c")
        set_source_files_properties (${source} PROPERTIES OCAML_C TRUE)
      else (ext STREQUAL ".ml")
        set (sources)
        if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
          set (sourceml "${CMAKE_CURRENT_SOURCE_DIR}/${source}.ml")
          if (EXISTS ${sourceml})
            list (APPEND sources ${sourceml})
            set_source_files_properties (${sourceml} PROPERTIES OCAML_IMPL TRUE)
          endif (EXISTS ${sourceml})
          set (sourcemli "${CMAKE_CURRENT_SOURCE_DIR}/${source}.mli")
          if (EXISTS ${sourcemli})
            list (APPEND sources ${sourcemli})
            set_source_files_properties (${sourcemli} PROPERTIES OCAML_INTF TRUE)
          endif (EXISTS ${sourcemli})
          set (sourcec "${CMAKE_CURRENT_SOURCE_DIR}/${source}.c")
          if (EXISTS ${sourcec})
            list (APPEND sources ${sourcec})
            set_source_files_properties (${sourcec} PROPERTIES OCAML_C TRUE)
          endif (EXISTS ${sourcec})
          if (NOT sources)
            message (SEND_ERROR "Can't find OCaml files for ${source}. To have correct dependencies, all files must be generated at configuration time.")
          endif (NOT sources)
        endif (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
      endif (ext STREQUAL ".ml")
    endif (NOT impl OR NOT intf OR NOT oc)

    foreach (src ${sources})
      if (IS_ABSOLUTE "${src}")
        list (APPEND OCAML_${target}_SOURCES "${src}")
      else (IS_ABSOLUTE "${src}")
        list (APPEND OCAML_${target}_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/${src}")
      endif (IS_ABSOLUTE "${src}")
    endforeach (src)

  endforeach (source)

  set(OCAML_${target}_TRANSPKGS ${OCAML_${target}_PACKAGES})
  foreach(lib ${OCAML_${target}_LIBRARIES})
    get_target_property(transpkgs ocaml.${lib} TRANSPKGS)
    list(APPEND OCAML_${target}_TRANSPKGS ${transpkgs})
  endforeach()
  if(DEFINED OCAML_${target}_TRANSPKGS)
    list(REMOVE_DUPLICATES OCAML_${target}_TRANSPKGS)
  endif()

  set (OCAML_${target}_OBJECTS)
  set (OCAML_${target}_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir")

  list (APPEND OCAML_${target}_INCLUDE_DIRECTORIES ${OCAML_${target}_OUTPUT_DIR})
  foreach (ltarget ${OCAML_${target}_OCAML_TARGET_LIBRARIES})
    get_target_property (object_dir ocaml.${ltarget} OBJECT_DIRECTORY)
    list (APPEND OCAML_${target}_INCLUDE_DIRECTORIES ${object_dir})
  endforeach (ltarget)
  if(NOT CMAKE_OCaml_FIND)
    foreach(pkg ${OCAML_${target}_TRANSPKGS})
      string(TOUPPER ${pkg} PKG)
      list(APPEND OCAML_${target}_INCLUDE_DIRECTORIES ${${PKG}_INCLUDE_DIRS})
    endforeach()
  endif()
  get_directory_property (include_dirs INCLUDE_DIRECTORIES)
  list (APPEND OCAML_${target}_INCLUDE_DIRECTORIES ${include_dirs})

  list (REMOVE_DUPLICATES OCAML_${target}_INCLUDE_DIRECTORIES)

  foreach (source ${OCAML_${target}_SOURCES})
    get_source_file_property (impl ${source} OCAML_IMPL)
    get_source_file_property (ocamlc ${source} OCAML_C)
    get_filename_component (path    ${source} PATH)
    get_filename_component (name_we ${source} NAME_WE)
    if (impl)
      set (hasintf FALSE)
      if (EXISTS "${path}/${name_we}.mli")
        set (hasintf TRUE)
      endif (EXISTS "${path}/${name_we}.mli")
      ocaml_add_object_target (${target} ${source} ${hasintf} object)
      list (APPEND OCAML_${target}_OBJECTS ${object})
    else (impl)
      if (ocamlc)
        ocaml_add_c_object_target (${target} ${source} object)
        list (APPEND OCAML_${target}_OBJECTS ${object})
      else (ocamlc)
        ocaml_add_interface_object_target (${target} ${source})
      endif (ocamlc)
    endif (impl)
  endforeach (source)

endmacro (add_ocaml_objects)

# target_link_ocaml_libraries (target libraries)
#   See description above.
macro (target_link_ocaml_libraries target)

  set (deps)
  set (tdeps)
  set (libraries)

  if (OCAML_${target}_NATIVE)
    set (compiler ${CMAKE_OCaml_CMD_OPT_COMPILER})
    set (libext ".cmxa")
  else (OCAML_${target}_NATIVE)
    set (compiler ${CMAKE_OCaml_CMD_COMPILER})
    set (libext ".cma")
  endif (OCAML_${target}_NATIVE)

  set (opt ${CMAKE_OCaml_LINKER_FLAGS} ${CMAKE_OCaml_LINKER_FLAGS_${CMAKE_BUILD_TYPE_UPPER}})

  # if (WIN32)
  #   include(TargetArch)
  #   target_architecture(ARCH)
  #   target_compiler(COMPILER)
  #   set(CMAKE_Flexlink_TOOLCHAIN ld)
  #   if("${COMPILER}" STREQUAL "MINGW")
  #     if("${ARCH}" STREQUAL "x86_64")
  #       set(CMAKE_Flexlink_TOOLCHAIN mingw64)
  #     elseif("${ARCH}" STREQUAL "i386")
  #       set(CMAKE_Flexlink_TOOLCHAIN mingw)
  #     endif()
  #   elseif("${COMPILER}" STREQUAL "MSVC")
  #     if("${ARCH}" STREQUAL "x86_64")
  #       set(CMAKE_Flexlink_TOOLCHAIN msvc64)
  #     elseif("${ARCH}" STREQUAL "i386")
  #       set(CMAKE_Flexlink_TOOLCHAIN msvc)
  #     endif()
  #   elseif("${COMPILER}" STREQUAL "CYGWIN")
  #     if("${ARCH}" STREQUAL "x86_64")
  #       set(CMAKE_Flexlink_TOOLCHAIN cygwin64)
  #     elseif("${ARCH}" STREQUAL "i386")
  #       set(CMAKE_Flexlink_TOOLCHAIN cygwin)
  #     endif()
  #   endif()

  #   set (opt -cc \"${CMAKE_Flexlink_EXECUTABLE}\" -cclib \"-chain ${CMAKE_Flexlink_TOOLCHAIN} -exe\")
  # else (WIN32)
  #   if (CMAKE_CXX_COMPILER)
  #     set (opt ${opt} -cc \"${CMAKE_CXX_COMPILER}\")
  #   else (CMAKE_CXX_COMPILER)
  #     if (CMAKE_C_COMPILER)
  #         set (opt ${opt} -cc \"${CMAKE_C_COMPILER}\")
  #     endif (CMAKE_C_COMPILER)
  #   endif (CMAKE_CXX_COMPILER)
  # endif(WIN32)

  foreach (library ${OCAML_${target}_LIBRARIES})
    if (IS_ABSOLUTE ${library})
      list (APPEND libraries ${library}${libext})
    else (IS_ABSOLUTE ${library})
      get_target_property (location  ocaml.${library} LOCATION)
      get_target_property (ilocation ocaml.${library} IMPORTED_LOCATION)
      if (ilocation) # It is a library imported from another project
        list (APPEND deps ${ilocation})
        get_target_property (libs ocaml.${library} LINK_INTERFACE_LIBRARIES)
        list (APPEND libraries ${libs} ${ilocation})
      elseif (location) # It is a library of this project
        list (APPEND deps ${location})
        list (APPEND tdeps ocaml.${library})
        get_target_property (libs ocaml.${library} LINK_INTERFACE_LIBRARIES)
        list (APPEND libraries ${libs} ${location})
        get_target_property (libs ocaml.${library} LINK_INTERFACE_C_LIBRARIES)
        foreach (lib ${libs})
            set (dir $<TARGET_FILE_DIR:${lib}>)
            set (opt ${opt} -ccopt -L${dir})
        endforeach (lib)
      else (ilocation) # It is a standard library
        set (location ${library}${libext})
        list (APPEND libraries ${location})
      endif (ilocation)
    endif (IS_ABSOLUTE ${library})
  endforeach (library)

  set (custom FALSE)
  set (clibraries)
  foreach (library ${OCAML_${target}_C_LIBRARIES})
    set (custom TRUE)
    if (${CMAKE_MAJOR_VERSION} LESS 3)
        get_target_property (location ${library} LOCATION)
    else (${CMAKE_MAJOR_VERSION} LESS 3)
        set (location $<TARGET_FILE:${library}>)
    endif (${CMAKE_MAJOR_VERSION} LESS 3)
    if (location) # It is a target from this project
      get_target_property (name ${library} OUTPUT_NAME)
      if (NOT name)
          set (name ${library})
      endif (NOT name)
      set (opt ${opt} -cclib -l${name})
      list (APPEND clibraries ${library})
    else (location)
      get_filename_component (name_we ${library} NAME_WE)
      string (REGEX REPLACE "^lib(.*)$" "\\1" name ${name_we})
      set (opt ${opt} -cclib -l${name})
      list (APPEND clibraries ${library})
    endif (location)
    list (APPEND deps ${location})
    list (APPEND tdeps ${library})
  endforeach (library)

  if((${OCAML_${target}_KIND} STREQUAL "EXECUTABLE") OR
      (${OCAML_${target}_KIND} STREQUAL "C_OBJECT"))
      ocaml_get_packages_flags(${target} ${libext} include_flags package_flags intertarget_dependencies)
      if(package_flags OR include_flags)
        set(opt ${opt} ${include_flags} ${package_flags})
      endif()
  endif()

  if (custom)
    if (NOT OCAML_${target}_NATIVE)
      set (opt ${opt} -custom)
    endif (NOT OCAML_${target}_NATIVE)
  endif (custom)

  if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
      set (opt ${opt} -cclib "-Wl,-no_compact_unwind")
  endif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")

  get_directory_property (link_dirs LINK_DIRECTORIES)
  foreach (link_dir ${link_dirs})
    set (opt ${opt} -I ${link_dir})
  endforeach (link_dir)

  if (${OCAML_${target}_KIND} STREQUAL "EXECUTABLE")
    set (comment  "Linking OCaml executable ${target}${CMAKE_EXECUTABLE_SUFFIX}")
    set (ext      "${CMAKE_EXECUTABLE_SUFFIX}")
    set (location "${CMAKE_CURRENT_BINARY_DIR}/${target}${ext}")
    set (opt      ${opt} ${OCAML_${target}_LINK_FLAGS})
    if (NOT OCAML_${target}_NATIVE)
      set (opt ${opt} ${CMAKE_OCaml_workaround_so_link_flags_bytecode})
    endif (NOT OCAML_${target}_NATIVE)
    set (libs     ${libraries})
    message(STATUS "${target} ${location}")
  elseif (${OCAML_${target}_KIND} STREQUAL "LIBRARY")
    set (comment  "Linking OCaml library ${target}")
    set (ext      "${libext}")
    set (location "${CMAKE_CURRENT_BINARY_DIR}/${target}${ext}")
    set (opt      ${opt} -a ${OCAML_${target}_LINK_FLAGS})
    set (libs)
  elseif (${OCAML_${target}_KIND} STREQUAL "C_OBJECT")
    if((${CMAKE_OCaml_CONFIG_os_type} MATCHES "[wW][iI][nN]32") OR (${CMAKE_OCaml_CONFIG_system} MATCHES "[mM][iI][nN][gG][wW]"))
      set (opt ${opt} -cclib "-outdef ${CMAKE_CURRENT_BINARY_DIR}/${target}.def -v")
    endif()
    set (comment  "Linking OCaml C object ${target}")
    set (ext      "${CMAKE_OCaml_CONFIG_ext_dll}")
    set (location "${CMAKE_CURRENT_BINARY_DIR}/${target}${ext}")
    set (opt ${opt} -output-obj ${OCAML_${target}_LINK_FLAGS})
    if (NOT OCAML_${target}_NATIVE)
      if(CMAKE_OCaml_workaround_so_link_flags_bytecode)
        set (opt ${opt} ${CMAKE_OCaml_workaround_so_link_flags_bytecode})
      endif()
    endif (NOT OCAML_${target}_NATIVE)
    if(CMAKE_OCaml_workaround_so_link_flags)
        set (opt ${opt} ${CMAKE_OCaml_workaround_so_link_flags})
    endif()
    set (libs     ${libraries})
  endif (${OCAML_${target}_KIND} STREQUAL "EXECUTABLE")

  set(output ${location})
  set(implib "")
  if ((${OCAML_${target}_KIND} STREQUAL "LIBRARY") AND OCAML_${target}_NATIVE)
    list (APPEND output "${CMAKE_CURRENT_BINARY_DIR}/${target}${CMAKE_OCaml_CONFIG_ext_lib}")
  endif()
  set(def_commands "")
  if ((${OCAML_${target}_KIND} STREQUAL "C_OBJECT") AND ((${CMAKE_OCaml_CONFIG_os_type} MATCHES "[wW][iI][nN]32") OR (${CMAKE_OCaml_CONFIG_system} MATCHES "[mM][iI][nN][gG][wW]")))
    set(implib "${CMAKE_CURRENT_BINARY_DIR}/${target}${CMAKE_IMPORT_LIBRARY_SUFFIX}")
    set(deffile "${CMAKE_CURRENT_BINARY_DIR}/${target}.def")
    list (APPEND output "${deffile}")
    list (APPEND output "${implib}")
    if(${CMAKE_OCaml_CONFIG_ccomp_type} MATCHES "[mM][sS][vV][cC]")
      set(architecture "${CMAKE_OCaml_CONFIG_architecture}")
      if(${CMAKE_OCaml_CONFIG_architecture} MATCHES "[iI]386")
        set(architecture "x86")
      elseif(${CMAKE_OCaml_CONFIG_architecture} MATCHES "[aA][mM][dD]64")
        set(architecture "x64")
      endif()
      list (APPEND def_commands COMMAND  lib /DEF:"${deffile}" /OUT:"${implib}" /MACHINE:"${architecture}")
    else()
      list (APPEND def_commands COMMAND  ${CMAKE_DLLTOOL} -d "${deffile}" -l "${implib}")
    endif()
  endif()
  add_custom_command (OUTPUT ${output}
    COMMAND ${compiler} ${opt} -o ${target}${ext} ${libs} ${OCAML_${target}_OBJECTS}
    ${def_commands}
    DEPENDS ${OCAML_${target}_OBJECTS} ${deps} ${intertarget_dependencies}
    COMMENT "${comment}"
    )

  add_custom_target (ocaml.${target} ALL DEPENDS ${location})

  if (tdeps)
    add_dependencies (ocaml.${target} ${tdeps})
  endif (tdeps)

  set_target_properties (ocaml.${target} PROPERTIES
    KIND                       ${OCAML_${target}_KIND}
    NATIVE                     ${OCAML_${target}_NATIVE}
    LOCATION                   "${location}"
    IMPLIB                     "${implib}"
    LINK_INTERFACE_LIBRARIES   "${libraries}"
    OUTPUT_NAME                ${target}${ext}
    OBJECT_DIRECTORY           ${OCAML_${target}_OUTPUT_DIR}
    OCAML_SOURCES              "${OCAML_${target}_SOURCES}"
    TRANSPKGS                  "${OCAML_${target}_TRANSPKGS}"
    LINK_INTERFACE_C_LIBRARIES "${clibraries}"
    )

  if (CMAKE_OCaml_USE_OCAML_TRACE)
    message (STATUS "Add an OCaml target")
    message (STATUS "  KIND:             ${OCAML_${target}_KIND}")
    message (STATUS "  NATIVE:           ${OCAML_${target}_NATIVE}")
    message (STATUS "  LOCATION:         ${location}")
    message (STATUS "  OUTPUT_NAME:      ${target}${ext}")
    message (STATUS "  OBJECT_DIRECTORY: ${OCAML_${target}_OUTPUT_DIR}")
    message (STATUS "  OCAML_SOURCES:")
    foreach (s ${OCAML_${target}_SOURCES})
      message (STATUS "    ${s}")
    endforeach (s)
    message (STATUS "  LINK_INTERFACE_LIBRARIES:")
    foreach (l ${libraries})
      message (STATUS "    ${l}")
    endforeach (l)
    message (STATUS "  LINK_INTERFACE_C_LIBRARIES:")
    foreach (l ${clibraries})
      message (STATUS "    ${l}")
    endforeach (l)
    message (STATUS "")
  endif (CMAKE_OCaml_USE_OCAML_TRACE)

endmacro (target_link_ocaml_libraries)

macro (set_ocaml_target_variables target)

  if (${target}_NATIVE)
    set (OCAML_${target}_NATIVE TRUE)
  elseif (${target}_BYTECODE)
    set (OCAML_${target}_NATIVE FALSE)
  else (${target}_NATIVE)
    set (OCAML_${target}_NATIVE ${CMAKE_OCaml_NATIVE})
  endif (${target}_NATIVE)

  set(OCAML_${target}_PACKAGES ${${target}_PACKAGES})
  set(OCAML_${target}_LIBRARIES ${${target}_LIBRARIES})
  set(OCAML_${target}_C_LIBRARIES ${${target}_C_LIBRARIES})
  set(OCAML_${target}_INCLUDE_DIRECTORIES ${${target}_INCLUDE_DIRECTORIES})

  set (OCAML_${target}_OCAML_TARGET_LIBRARIES)

  foreach (library ${OCAML_${target}_LIBRARIES})
    get_target_property (kind ocaml.${library} KIND)
    if ((kind STREQUAL "LIBRARY") OR (kind STREQUAL "C_OBJECT"))
      list (APPEND OCAML_${target}_OCAML_TARGET_LIBRARIES ${library})
    endif ((kind STREQUAL "LIBRARY") OR (kind STREQUAL "C_OBJECT"))
  endforeach (library)

  set (OCAML_${target}_LINK_FLAGS ${${target}_LINK_FLAGS})
  set (OCAML_${target}_C_FLAGS ${${target}_C_FLAGS})
endmacro (set_ocaml_target_variables)

# add_ocaml_executable (name sourcefiles)
#   See description above.
macro (add_ocaml_executable target)
  ocaml_parse_macro_arguments (${target} "NATIVE;BYTECODE;SOURCES;PACKAGES;LIBRARIES;C_LIBRARIES;C_FLAGS;LINK_FLAGS;INCLUDE_DIRECTORIES" ${ARGN})
  set_ocaml_target_variables (${target})
  set (OCAML_${target}_KIND "EXECUTABLE")
  add_ocaml_objects (${target})
  target_link_ocaml_libraries (${target})
endmacro (add_ocaml_executable)

# add_ocaml_library (target sourcefiles)
#   See description above.
macro (add_ocaml_library target)
  ocaml_parse_macro_arguments (${target} "NATIVE;BYTECODE;SOURCES;PACKAGES;LIBRARIES;C_LIBRARIES;C_FLAGS;LINK_FLAGS;INCLUDE_DIRECTORIES" ${ARGN})
  set_ocaml_target_variables (${target})
  set (OCAML_${target}_KIND "LIBRARY")
  add_ocaml_objects (${target})
  target_link_ocaml_libraries (${target})
endmacro (add_ocaml_library)

# add_ocaml_pack (target sourcefiles)
#   See description above.
macro (add_ocaml_pack target)
  ocaml_parse_macro_arguments (${target} "NATIVE;BYTECODE;SOURCES;PACKAGES;LIBRARIES;C_LIBRARIES;C_FLAGS;LINK_FLAGS;INCLUDE_DIRECTORIES" ${ARGN})
  set_ocaml_target_variables (${target})
  set (OCAML_${target}_KIND "PACK")
  add_ocaml_objects (${target})
  target_link_ocaml_libraries (${target})
endmacro (add_ocaml_pack)

# add_ocaml_c_object (target sourcefiles)
#   See description above.
macro (add_ocaml_c_object target)
    ocaml_parse_macro_arguments (${target} "NATIVE;BYTECODE;SOURCES;PACKAGES;LIBRARIES;C_LIBRARIES;C_FLAGS;LINK_FLAGS;INCLUDE_DIRECTORIES" ${ARGN})
  set_ocaml_target_variables (${target})
  set (OCAML_${target}_KIND "C_OBJECT")
  add_ocaml_objects (${target})
  target_link_ocaml_libraries (${target})
  # expose target as a C library as well
  get_property(location TARGET ocaml.${target} PROPERTY LOCATION)
  get_property(implib TARGET ocaml.${target} PROPERTY IMPLIB)
  add_library(${target} SHARED IMPORTED)
  set_property(TARGET ${target} PROPERTY IMPORTED_LOCATION ${location})
  set_property(TARGET ${target} PROPERTY IMPORTED_IMPLIB ${implib})
  add_dependencies(${target} ocaml.${target})
endmacro (add_ocaml_c_object)

# install_ocaml_targets (executables DESTINATION destination)
#   See description above.
macro (install_ocaml_targets)
  ocaml_parse_macro_arguments ("install" "DESTINATION" ${ARGN})

  set (targets ${install_FIRST_ARGS})

  foreach (target ${targets})
    get_target_property (kind     ocaml.${target} KIND)
    get_target_property (location ocaml.${target} LOCATION)
    get_target_property (native ocaml.${target} NATIVE)
    if (${kind} STREQUAL "EXECUTABLE")
      install (PROGRAMS ${location} DESTINATION ${install_DESTINATION})
    elseif (${kind} STREQUAL "LIBRARY")
      install (FILES ${location} DESTINATION ${install_DESTINATION})
      if (native)
          get_filename_component (file_name_we ${location} NAME_WE)
          get_filename_component (file_path    ${location} PATH)
          install (FILES ${file_path}/${file_name_we}${CMAKE_OCaml_CONFIG_ext_lib} DESTINATION
            ${install_DESTINATION})
      endif (native)
    elseif (${kind} STREQUAL "C_OBJECT")
      install (FILES ${location} DESTINATION ${install_DESTINATION})
    endif (${kind} STREQUAL "EXECUTABLE")
    set_target_properties (ocaml.${target} PROPERTIES INSTALL_LOCATION ${install_DESTINATION})
  endforeach (target)

endmacro (install_ocaml_targets)

# install_ocaml_interfaces (...)
#   See description above.
macro (install_ocaml_interfaces target)
  ocaml_parse_macro_arguments (${target} "DESTINATION" ${ARGN})

  get_target_property (native  ocaml.${target} NATIVE)
  get_target_property (obj_dir ocaml.${target} OBJECT_DIRECTORY)

  set (interfaces)
  foreach (interface ${${target}_FIRST_ARGS})
    list (APPEND interfaces "${obj_dir}/${interface}.cmi")
  endforeach (interface)

  install (FILES ${interfaces} DESTINATION ${${target}_DESTINATION})

endmacro (install_ocaml_interfaces)

# install_ocaml_exports (...)
#   See description above.
macro (install_ocaml_exports)
  ocaml_parse_macro_arguments ("export" "DESTINATION;FILE" ${ARGN})

  set (file "${CMAKE_CURRENT_BINARY_DIR}/${export_FILE}")

  file (WRITE "${file}")
  file (APPEND "${file}"  "get_filename_component (self_dir   \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)\n")

  set (tmp)
  set (temp "${export_DESTINATION}")
  while (temp)
    get_filename_component (temp "${temp}" PATH)
    set (tmp "${tmp}/..")
  endwhile (temp)

  file (APPEND "${file}" "get_filename_component (prefix_dir \"\${self_dir}${tmp}\" ABSOLUTE)\n\n")

  foreach (target ${export_FIRST_ARGS})
    get_target_property (kind            ocaml.${target} KIND)
    get_target_property (location        ocaml.${target} INSTALL_LOCATION)
    get_target_property (link_interfaces ocaml.${target} LINK_INTERFACE_LIBRARIES)
    get_target_property (transpkgs       ocaml.${target} TRANSPKGS)
    get_target_property (name            ocaml.${target} OUTPUT_NAME)
    if (${kind} STREQUAL "EXECUTABLE")
      file (APPEND "${file}" "add_executable (ocaml.${target} IMPORTED)\n")
      file (APPEND "${file}" "set_target_properties (ocaml.${target} PROPERTIES\n")
      file (APPEND "${file}" "  IMPORTED_LOCATION \"\${prefix_dir}/${location}/${name}\"\n")
      file (APPEND "${file}" "  )\n\n")
    elseif (${kind} STREQUAL "LIBRARY")
      file (APPEND "${file}" "add_library (ocaml.${target} UNKNOWN IMPORTED)\n")
      file (APPEND "${file}" "set_target_properties (ocaml.${target} PROPERTIES\n")
      file (APPEND "${file}" "  KIND                     \"LIBRARY\"\n")
      file (APPEND "${file}" "  IMPORTED_LOCATION        \"\${prefix_dir}/${location}/${name}\"\n")
      file (APPEND "${file}" "  INCLUDE_DIRECTORIES      \"\${prefix_dir}/${location}/\"\n")
      file (APPEND "${file}" "  LINK_INTERFACE_LIBRARIES \"${link_interfaces}\"\n")
      file (APPEND "${file}" "  TRANSPKGS                \"${transpkgs}\"\n")
      file (APPEND "${file}" "  OUTPUT_NAME              ${name}\n")
      file (APPEND "${file}" "  )\n\n")
    endif (${kind} STREQUAL "EXECUTABLE")
  endforeach (target)

  install (FILES "${file}" DESTINATION "${export_DESTINATION}")

endmacro (install_ocaml_exports)

# gen_ocaml_lexers (outfilesname sources)
#   See description above.
macro (gen_ocaml_lexers outfilesname)
  set (outfilesname)
  foreach (source ${ARGN})
    get_filename_component (_name_we ${source} NAME_WE)
    set (_output ${CMAKE_CURRENT_BINARY_DIR}/${_name_we}.ml)
    if (NOT EXISTS ${_output})
      execute_process (
        COMMAND           ${CMAKE_OCaml_LEX} -o ${_output} -ml ${source}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    endif (NOT EXISTS ${_output})
    add_custom_command (OUTPUT ${_output}
      COMMAND           ${CMAKE_OCaml_LEX} -o ${_output} -ml ${source}
      MAIN_DEPENDENCY   ${source}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      )
    list (APPEND ${outfilesname} ${_output})
    add_custom_target (ocaml.${_name_we}.ml DEPENDS ${_output})
  endforeach (source)
endmacro (gen_ocaml_lexers)

# gen_ocaml_parsers (outfilesname sources)
#   See description above.
macro (gen_ocaml_parsers outfilesname)
  set (outfilesname)
  foreach (source ${ARGN})
    get_filename_component (_name_we ${source} NAME_WE)
    set (_output ${CMAKE_CURRENT_BINARY_DIR}/${_name_we}.mli ${CMAKE_CURRENT_BINARY_DIR}/${_name_we}.ml)
    if (NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${_name_we}.ml)
      execute_process (
        COMMAND           ${CMAKE_OCaml_YACC} -b ${CMAKE_CURRENT_BINARY_DIR}/${_name_we} ${source}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    endif (NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${_name_we}.ml)
    add_custom_command (OUTPUT ${_output}
      COMMAND           ${CMAKE_OCaml_YACC} -b ${CMAKE_CURRENT_BINARY_DIR}/${_name_we} ${source}
      MAIN_DEPENDENCY   ${source}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      )
    list (APPEND ${outfilesname} ${_output})
    add_custom_target (ocaml.${_name_we}.ml DEPENDS ${_output})
  endforeach (source)
endmacro (gen_ocaml_parsers)

