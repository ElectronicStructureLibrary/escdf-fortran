# -*- Autoconf -*-
#
# Copyright (C) 2017 Yann Pouillon
#
# This file is part of the ESCDF-Fortran software package. For license
# information, please see the COPYING file in the top-level directory of
# the ESCDF-Fortran source distribution.
#



# ESCDFF_ESCDF_DETECT()
# -------------------
#
# Check whether the ESCDF library is working.
#
AC_DEFUN([ESCDFF_ESCDF_DETECT],[
  dnl Init
  escdff_escdf_ok="unknown"
  escdff_escdf_has_hdrs="unknown"
  escdff_escdf_has_libs="unknown"

  dnl Backup environment
  escdff_saved_CPPFLAGS="${CPPFLAGS}"
  escdff_saved_LIBS="${LIBS}"

  dnl Prepare build parameters
  CPPFLAGS="${CPPFLAGS} ${escdff_escdf_incs}"
  LIBS="${escdff_escdf_libs} ${LIBS}"

  dnl Look for C includes
  AC_LANG_PUSH([C])
  AC_CHECK_HEADERS([escdf.h],
    [escdff_escdf_has_hdrs="yes"], [escdff_escdf_has_hdrs="no"])
  AC_LANG_POP([C])

  dnl Look for C libraries and routines
  if test "${escdff_escdf_has_hdrs}" = "yes"; then
    AC_LANG_PUSH([C])
    AC_MSG_CHECKING([whether the ESCDF library works])
    AC_LINK_IFELSE([AC_LANG_PROGRAM(
      [[
#include <escdf.h>
      ]],
      [[
        escdf_handle_t *htest;
        htest = escdf_create("conftest.h5", "/test");
      ]])], [escdff_escdf_has_libs="yes"], [escdff_escdf_has_libs="no"])
    AC_MSG_RESULT([${escdff_escdf_has_libs}])
    AC_LANG_POP([C])
  fi

  dnl Take final decision
  AC_MSG_CHECKING([whether we have a full ESCDF support])
  if test "${escdff_escdf_has_hdrs}" = "yes" -a \
          "${escdff_escdf_has_libs}" = "yes"; then
    escdff_escdf_ok="yes"
  else
    escdff_escdf_ok="no"
  fi
  AC_MSG_RESULT([${escdff_escdf_ok}])

  dnl Restore environment
  CPPFLAGS="${escdff_saved_CPPFLAGS}"
  LIBS="${escdff_saved_LIBS}"
]) # ESCDFF_ESCDF_DETECT
