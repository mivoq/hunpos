###############################################################################
##                                                                           ##
## AUTHOR     : Aby Louw                                                     ##
## DATE       : July 2010                                                    ##
##                                                                           ##
## CHANGES BY : Giulio Paci                                                  ##
##                                                                           ##
###############################################################################
##                                                                           ##
## CMake toolchain file for opam-cross-windows cross-compilation             ##
## This has been copied from Speect (copied from Allegro)                    ##
##                                                                           ##
###############################################################################
##                                                                           ##
##                                                                           ##
## Use this command to build with a mingw cross compiler:                    ## 
##                                                                           ##
## cmake -DCMAKE_TOOLCHAIN_FILE=cmake/Toolchain-mingw.cmake .                ##
##                                                                           ##
## or for out of source:                                                     ##
##                                                                           ##
## cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/Toolchain-mingw.cmake ..            ##
##                                                                           ##
## You will need at least CMake 2.6.0.                                       ##
##                                                                           ##
## Adjust the following paths to suit your environment.                      ##
##                                                                           ##
## This file was based on http://www.cmake.org/Wiki/CmakeMingw               ##
##                                                                           ##
###############################################################################


#-----------------------------------------------------------------------------#
#                             CMake system target                             #
#-----------------------------------------------------------------------------#

set(CMAKE_SYSTEM_NAME Windows)


#-----------------------------------------------------------------------------#
#                             Target architecture                             #
#-----------------------------------------------------------------------------#

# Assume the target architecture.
# XXX for some reason the value set here gets cleared before we reach the
# main CMakeLists.txt; see that file for a workaround.
set(CMAKE_SYSTEM_PROCESSOR x86_64)


#-----------------------------------------------------------------------------#
#                                 Compilers                                   #
#-----------------------------------------------------------------------------#

# Which compilers to use for C and C++, and location of target
# environment.
if(EXISTS /usr/x86_64-w64-mingw32)
    # First look in standard location as used by recent Debian/Ubuntu/etc.
    set(CMAKE_C_COMPILER x86_64-w64-mingw32-gcc)
    set(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++)
    set(CMAKE_DLLTOOL x86_64-w64-mingw32-dlltool)
    set(CMAKE_OCaml_COMPILER ocamlfind -toolchain windows ocamlc)
    set(CMAKE_OCaml_OPT_COMPILER ocamlfind -toolchain windows ocamlopt)
    set(CMAKE_OCaml_DEP ocamlfind -toolchain windows ocamldep)
    set(CMAKE_OCaml_FIND ocamlfind -toolchain windows)
    set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32)
else()
    # Else fill in local path which the user will likely adjust.
    set(CMAKE_C_COMPILER /usr/local/cross-tools/bin/x86_64-mingw32-gcc)
    set(CMAKE_CXX_COMPILER /usr/local/cross-tools/bin/x86_64-mingw32-g++)
    set(CMAKE_DLLTOOL /usr/local/cross-tools/bin/x86_64-mingw32-dlltool)
    set(CMAKE_FIND_ROOT_PATH /usr/local/cross-tools)
endif()

#-----------------------------------------------------------------------------#
#                           Find command behaviour                            #
#-----------------------------------------------------------------------------#

# Adjust the default behaviour of the FIND_XXX() commands:
# search headers and libraries in the target environment, search
# programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)


#-----------------------------------------------------------------------------#
#                            pkg-config behaviour                             #
#-----------------------------------------------------------------------------#

# Tell pkg-config not to look at the target environment's .pc files.
# Setting PKG_CONFIG_LIBDIR sets the default search directory, but we have to
# set PKG_CONFIG_PATH as well to prevent pkg-config falling back to the host's
# path.
set(ENV{PKG_CONFIG_LIBDIR} ${CMAKE_FIND_ROOT_PATH}/lib/pkgconfig)
set(ENV{PKG_CONFIG_PATH} ${CMAKE_FIND_ROOT_PATH}/lib/pkgconfig)


#-----------------------------------------------------------------------------#
#                               Installation                                  #
#-----------------------------------------------------------------------------#

set(INSTALL_PREFIX ${CMAKE_FIND_ROOT_PATH})
set(ENV{MINGDIR} ${CMAKE_FIND_ROOT_PATH})


#-----------------------------------------------------------------------------#
#                               Testing                                       #
#-----------------------------------------------------------------------------#

set(TARGET_SYSTEM_EMULATOR wine)
