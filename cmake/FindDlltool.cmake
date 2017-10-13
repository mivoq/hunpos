# - Find Dlltool
# Try to find dlltool.
#
# The following variables are defined:
#  DLLTOOL - The MinGW dlltool executable
#
# Copyright (c) 2017, Giulio Paci <giuliopaci@gmail.com>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

find_program (CMAKE_DLLTOOL dlltool)

mark_as_advanced(
  CMAKE_DLLTOOL
  )
