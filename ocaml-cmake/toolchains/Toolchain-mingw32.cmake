###############################################################################
##                                                                           ##
## AUTHOR     : Aby Louw                                                     ##
## DATE       : July 2010                                                    ##
##                                                                           ##
## CHANGES BY : Giulio Paci                                                  ##
##                                                                           ##
###############################################################################
##                                                                           ##
## CMake toolchain file for 32bit mingw cross-compilation                    ##
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
set(CMAKE_SYSTEM_PROCESSOR i686)


#-----------------------------------------------------------------------------#
#                                 Compilers                                   #
#-----------------------------------------------------------------------------#

# Which compilers to use for C and C++, and location of target
# environment.
if(EXISTS /usr/i686-w64-mingw32)
    # First look in standard location as used by recent Debian/Ubuntu/etc.
    set(CMAKE_C_COMPILER i686-w64-mingw32-gcc)
    set(CMAKE_CXX_COMPILER i686-w64-mingw32-g++)
    set(CMAKE_DLLTOOL i686-w64-mingw32-dlltool)
    set(CMAKE_OCaml_COMPILER i686-w64-mingw32-ocamlc)
    set(CMAKE_OCaml_OPT_COMPILER i686-w64-mingw32-ocamlopt)
    set(CMAKE_OCaml_DEP i686-w64-mingw32-ocamldep)
    set(CMAKE_OCaml_FIND "")
    set(CMAKE_FIND_ROOT_PATH /usr/i686-w64-mingw32)
elseif(EXISTS /usr/i586-mingw32msvc)
    # First look in standard location as used by old Debian/Ubuntu/etc.
    set(CMAKE_C_COMPILER i586-mingw32msvc-gcc)
    set(CMAKE_CXX_COMPILER i586-mingw32msvc-g++)
    set(CMAKE_DLLTOOL i586-mingw32msvc-dlltool)
    set(CMAKE_FIND_ROOT_PATH /usr/i586-mingw32msvc)
elseif(EXISTS /opt/mingw)
    # Otherwise you can get a MinGW environment using the script at
    # <http://mingw-cross-env.nongnu.org>.  It downloads and builds MinGW and
    # most of the dependencies for you.  This is a suitable location.
    set(CMAKE_C_COMPILER /opt/mingw/usr/bin/i686-pc-mingw32-gcc)
    set(CMAKE_CXX_COMPILER /opt/mingw/usr/bin/i686-pc-mingw32-g++)
    set(CMAKE_DLLTOOL /opt/mingw/usr/bin/i686-pc-mingw32-dlltool)
    set(CMAKE_FIND_ROOT_PATH /opt/mingw/usr/i686-pc-mingw32)
else()
    # Else fill in local path which the user will likely adjust.
    # This is the location assumed by
    # <http://www.libsdl.org/extras/win32/cross/>
    set(CMAKE_C_COMPILER /usr/local/cross-tools/bin/i386-mingw32-gcc)
    set(CMAKE_CXX_COMPILER /usr/local/cross-tools/bin/i386-mingw32-g++)
    set(CMAKE_DLLTOOL /usr/local/cross-tools/bin/i386-mingw32-dlltool)
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
