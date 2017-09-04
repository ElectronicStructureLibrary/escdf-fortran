# -*- Autoconf -*-
#
# Copyright (C) 2017 Yann Pouillon
#
# This file is part of the ESCDF-Fortran software package. For license
# information, please see the COPYING file in the top-level directory of
# the ESCDF-Fortran source distribution.
#



# ESCDFF_HDF5_DETECT()
# -------------------
#
# Check whether the HDF5 library is working.
#
AC_DEFUN([ESCDFF_HDF5_DETECT],[
  dnl Init
  escdff_hdf5_ok="unknown"
  escdff_hdf5_has_hdrs="unknown"
  escdff_hdf5_has_libs="unknown"

  dnl Backup environment
  escdff_saved_CPPFLAGS="${CPPFLAGS}"
  escdff_saved_LIBS="${LIBS}"

  dnl Prepare build parameters
  CPPFLAGS="${CPPFLAGS} ${escdff_hdf5_incs}"
  LIBS="${escdff_hdf5_libs} ${LIBS}"

  dnl Look for C includes
  AC_LANG_PUSH([C])
  AC_CHECK_HEADERS([hdf5.h],
    [escdff_hdf5_has_hdrs="yes"], [escdff_hdf5_has_hdrs="no"])
  AC_LANG_POP([C])

  dnl Look for C libraries and routines
  if test "${escdff_hdf5_has_hdrs}" = "yes"; then
    AC_LANG_PUSH([C])
    AC_MSG_CHECKING([whether the serial HDF5 libraries work])
    AC_LINK_IFELSE([AC_LANG_PROGRAM(
      [[
#include <hdf5.h>
      ]],
      [[
        herr_t h5err;
        h5err = H5open();
      ]])], [escdff_hdf5_has_libs="yes"], [escdff_hdf5_has_libs="no"])
    AC_MSG_RESULT([${escdff_hdf5_has_libs}])
    AC_LANG_POP([C])
  fi

  dnl Take final decision
  AC_MSG_CHECKING([whether we have a full HDF5 support])
  if test "${escdff_hdf5_has_hdrs}" = "yes" -a \
          "${escdff_hdf5_has_libs}" = "yes"; then
    escdff_hdf5_ok="yes"
  else
    escdff_hdf5_ok="no"
  fi
  AC_MSG_RESULT([${escdff_hdf5_ok}])

  dnl Restore environment
  CPPFLAGS="${escdff_saved_CPPFLAGS}"
  LIBS="${escdff_saved_LIBS}"
]) # ESCDFF_HDF5_DETECT
