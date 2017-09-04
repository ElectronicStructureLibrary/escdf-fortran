# -*- Autoconf -*-
#
# Copyright (C) 2017 Yann Pouillon
#
# This file is part of the ESCDF-Fortran software package. For license
# information, please see the COPYING file in the top-level directory of
# the ESCDF-Fortran source distribution.
#



# ESCDFF_MPI_DETECT()
# ------------------
#
# Chechks whether the configured MPI implementation is working.
#
AC_DEFUN([ESCDFF_MPI_DETECT], [
  dnl Init
  escdff_mpi_ok="unknown"

  dnl Display current MPI status
  AC_MSG_CHECKING([how MPI parameters have been set])
  AC_MSG_RESULT([${escdff_mpi_type}])
  AC_MSG_CHECKING([whether the MPI C compiler is set])
  AC_MSG_RESULT([${escdff_mpi_cc_set}])
  AC_MSG_CHECKING([whether the MPI C compiler is wrapped])
  AC_MSG_RESULT([${escdff_mpi_cc_wrap}])
  AC_MSG_CHECKING([whether the MPI Fortran compiler is set])
  AC_MSG_RESULT([${escdff_mpi_fc_set}])
  AC_MSG_CHECKING([whether the MPI Fortran compiler is wrapped])
  AC_MSG_RESULT([${escdff_mpi_fc_wrap}])

  dnl Warn if serial component of wrapped compilers supports MPI
  if test "${escdff_mpi_cc_wrap}" = "yes"; then
    AC_MSG_NOTICE([validating that '${escdff_sercc}' is indeed serial])
    AC_MSG_NOTICE([please ignore possible warnings about mpi.h not found])
    _ESCDFF_MPI_CHECK_CC([${escdff_sercc}])
    if test "${escdff_mpi_cc_ok}" = "yes"; then
      AC_MSG_WARN([the serial C compiler is MPI-aware
                    Your current configuration is probably ill-defined.
                    The build will likely fail.])
      sleep 5
    fi
  fi
  if test "${escdff_mpi_fc_wrap}" = "yes"; then
    AC_MSG_NOTICE([validating that '${escdff_serfc}' is indeed serial])
    _ESCDFF_MPI_CHECK_FC([${escdff_serfc}])
    if test "${escdff_mpi_fc_ok}" = "yes"; then
      AC_MSG_WARN([the serial Fortran compiler is MPI-aware
                    Your current configuration is probably ill-defined.
                    The build will likely fail.])
      sleep 5
    fi
  fi

  dnl Test MPI compilers
  _ESCDFF_MPI_CHECK_CC([${CC}])
  if test "${escdff_mpi_cc_ok}" = "yes"; then
    _ESCDFF_MPI_CHECK_FC([${FC}])
  fi

  dnl Take final decision
  AC_MSG_CHECKING([whether we have a full MPI support])
  if test "${escdff_mpi_cc_ok}" = "yes" -a \
          "${escdff_mpi_fc_ok}" = "yes"; then
    escdff_mpi_ok="yes"
  else
    escdff_mpi_ok="no"
  fi
  AC_MSG_RESULT([${escdff_mpi_ok}])
]) # ESCDFF_MPI_DETECT



# ESCDFF_MPI_INIT()
# ----------------
#
# Initializes MPI parameters.
#
AC_DEFUN([ESCDFF_MPI_INIT], [
  if test "${escdff_mpi_enable}" != "no"; then
    AC_MSG_CHECKING([how MPI parameters have been set])
    AC_MSG_RESULT([${escdff_mpi_type}])
    if test "${escdff_mpi_type}" = "env"; then
      _AC_SRCDIRS(["."])
    fi
    _ESCDFF_MPI_INIT_CC
    _ESCDFF_MPI_INIT_FC
  fi
]) # ESCDFF_MPI_INIT



                    ########################################



# _ESCDFF_MPI_CHECK_CC(CC)
# -----------------------
#
# Check whether the MPI C compiler is working.
#
AC_DEFUN([_ESCDFF_MPI_CHECK_CC], [
  dnl Init
  escdff_mpi_cc_ok="unknown"
  escdff_mpi_cc_has_funs="unknown"
  escdff_mpi_cc_has_hdrs="unknown"

  dnl Prepare environment
  escdff_saved_CC="${CC}"
  CC="$1"
  tmp_mpi_header=mpi.h
  tmp_mpi_cache=AS_TR_SH([ac_cv_header_${tmp_mpi_header}])
  ${as_unset} ${tmp_mpi_cache}

  dnl Look for C includes
  AC_LANG_PUSH([C])
  AC_CHECK_HEADERS([mpi.h],
    [escdff_mpi_cc_has_hdrs="yes"], [escdff_mpi_cc_has_hdrs="no"])
  AC_LANG_POP([C])

  dnl Look for C functions
  if test "${escdff_mpi_cc_has_hdrs}" = "yes"; then
    AC_CHECK_FUNC([MPI_Init], [escdff_mpi_cc_has_funs="yes"],
      [escdff_mpi_cc_has_funs="no"])
  fi

  dnl Validate C support
  AC_MSG_CHECKING([whether the MPI C compiler works])
  if test "${escdff_mpi_cc_has_funs}" = "yes" -a \
          "${escdff_mpi_cc_has_hdrs}" = "yes"; then
    escdff_mpi_cc_ok="yes"
  else
    escdff_mpi_cc_ok="no"
  fi
  AC_MSG_RESULT([${escdff_mpi_cc_ok}])

  dnl Restore environment
  CC="${escdff_saved_CC}"
  unset tmp_mpi_cache
  unset tmp_mpi_header
]) # _ESCDFF_MPI_CHECK_CC



# _ESCDFF_MPI_CHECK_FC(FC)
# -----------------------
#
# Check whether the MPI Fortran compiler is working.
#
AC_DEFUN([_ESCDFF_MPI_CHECK_FC], [
  dnl Init
  escdff_mpi_fc_ok="unknown"
  escdff_mpi_fc_has_funs="unknown"
  escdff_mpi_fc_has_mods="unknown"

  dnl Prepare environment
  escdff_saved_FC="${FC}"
  FC="$1"

  dnl Look for Fortran modules
  AC_LANG_PUSH([Fortran])
  AC_MSG_CHECKING([for a Fortran MPI module])
  AC_LINK_IFELSE([AC_LANG_PROGRAM([],
    [[
      use mpi
    ]])], [escdff_mpi_fc_has_mods="yes"], [escdff_mpi_fc_has_mods="no"])
  AC_MSG_RESULT([${escdff_mpi_fc_has_mods}])
  AC_LANG_POP([Fortran])

  dnl Look for Fortran functions
  if test "${escdff_mpi_fc_has_mods}" = "yes"; then
    AC_LANG_PUSH([Fortran])
    AC_MSG_CHECKING([for a Fortran MPI_Init])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([],
      [[
        use mpi
        integer :: ierr
        call mpi_init(ierr)
      ]])], [escdff_mpi_fc_has_funs="yes"], [escdff_mpi_fc_has_funs="no"])
    AC_MSG_RESULT([${escdff_mpi_fc_has_funs}])
    AC_LANG_POP([Fortran])
  fi

  dnl Validate Fortran support
  AC_MSG_CHECKING([whether the MPI Fortran compiler works])
  if test "${escdff_mpi_fc_has_funs}" = "yes" -a \
          "${escdff_mpi_fc_has_mods}" = "yes"; then
    escdff_mpi_fc_ok="yes"
  else
    escdff_mpi_fc_ok="no"
  fi
  AC_MSG_RESULT([${escdff_mpi_fc_ok}])

  dnl Restore environment
  FC="${escdff_saved_FC}"
]) # _ESCDFF_MPI_CHECK_FC



# _ESCDFF_MPI_INIT_CC()
# --------------------
#
# Initializes MPI parameters related to the C compiler.
#
AC_DEFUN([_ESCDFF_MPI_INIT_CC], [
  dnl Init
  escdff_sercc="${CC}"
  escdff_mpicc=""
  escdff_mpi_cc_set="no"
  escdff_mpi_cc_wrap="unknown"

  dnl Look for a MPI C compiler
  case "${escdff_mpi_type}" in

    def)
      escdff_mpi_cc_wrap="no"
      ;;

    dir)
      escdff_mpicc="${with_mpi}/bin/mpicc"
      if test -x "${escdff_mpicc}"; then
        AC_MSG_CHECKING([for an executable MPI C compiler])
        AC_MSG_RESULT([${escdff_mpicc}])
        if test "${escdff_sercc}" = ""; then
          AC_MSG_NOTICE([setting CC to '${escdff_mpicc}'])
          CC="${escdff_mpicc}"
          escdff_mpi_cc_set="yes"
          escdff_mpi_cc_wrap="no"
        else
          escdff_mpi_cc_wrap="yes"
        fi
      else
        AC_MSG_ERROR([MPI C compiler not found in ${with_mpi}/bin])
      fi
      ;;

    env|yon)
      if test -n "${MPICC}"; then
        escdff_mpicc="${MPICC}"
      else
        AC_CHECK_PROGS([escdff_mpicc], [mpicc])
      fi
      if test -n "${escdff_sercc}" -a -n "${escdff_mpicc}"; then
        escdff_mpi_cc_wrap="yes"
      elif test -n "${escdff_mpicc}"; then
        AC_MSG_NOTICE([setting CC to '${escdff_mpicc}'])
        CC="${escdff_mpicc}"
        escdff_mpi_cc_set="yes"
        escdff_mpi_cc_wrap="no"
      fi
      ;;

  esac

  if test "${escdff_mpi_cc_wrap}" = "yes"; then
    _ESCDFF_MPI_CREATE_WRAPPER([CC], [${escdff_sercc}], [${escdff_mpicc}])
    escdff_mpi_cc_set="yes"
  fi
]) # _ESCDFF_MPI_INIT_CC



# _ESCDFF_MPI_INIT_FC()
# --------------------
#
# Initializes MPI parameters related to the Fortran compiler.
#
AC_DEFUN([_ESCDFF_MPI_INIT_FC], [
  dnl Init
  escdff_serfc="${FC}"
  escdff_mpifc=""
  escdff_mpi_fc_set="no"
  escdff_mpi_fc_wrap="unknown"

  dnl Look for a MPI Fortran compiler
  case "${escdff_mpi_type}" in

    def)
      escdff_mpi_fc_wrap="no"
      ;;

    dir)
      escdff_mpifc="${with_mpi}/bin/mpif90"
      if test -x "${escdff_mpifc}"; then
        AC_MSG_CHECKING([for an executable MPI Fortran compiler])
        AC_MSG_RESULT([${escdff_mpifc}])
        if test "${escdff_serfc}" = ""; then
          AC_MSG_NOTICE([setting FC to '${escdff_mpifc}'])
          FC="${escdff_mpifc}"
          escdff_mpi_fc_set="yes"
          escdff_mpi_fc_wrap="no"
        else
          escdff_mpi_fc_wrap="yes"
        fi
      else
        AC_MSG_ERROR([MPI Fortran compiler not found in ${with_mpi}/bin])
      fi
      ;;

    env|yon)
      if test -n "${MPIFC}"; then
        escdff_mpifc="${MPIFC}"
      else
        AC_CHECK_PROGS([escdff_mpifc], [mpif90 mpif95])
      fi
      if test -n "${escdff_serfc}" -a -n "${escdff_mpifc}"; then
        escdff_mpi_fc_wrap="yes"
      elif test -n "${escdff_mpifc}"; then
        AC_MSG_NOTICE([setting FC to '${escdff_mpifc}'])
        FC="${escdff_mpifc}"
        escdff_mpi_fc_set="yes"
        escdff_mpi_fc_wrap="no"
      fi
      ;;

  esac

  if test "${escdff_mpi_fc_wrap}" = "yes"; then
    _ESCDFF_MPI_CREATE_WRAPPER([FC], [${escdff_serfc}], [${escdff_mpifc}])
    escdff_mpi_fc_set="yes"
  fi
]) # _ESCDFF_MPI_INIT_FC



# _ESCDFF_MPI_CREATE_WRAPPER(COMPILER_TYPE, SERIAL_COMPILER, MPI_COMPILER)
# -----------------------------------------------------------------------
#
# Creates a wrapper for MPI compilers when they can be configured to
# accept different serial compilers.
#
# Note: it is impossible to set two compiler levels with the Autotools,
#       because Automake requires CC, CXX, and FC to be set to
#       the actual compilers.
#
AC_DEFUN([_ESCDFF_MPI_CREATE_WRAPPER], [
  dnl Init
  tmp_comp_name=`echo "$1" | sed -e 's/.*/\L&/'`
  ${MKDIR_P} config/wrappers

  dnl Create file
  cat >config/wrappers/wrap-mpi${tmp_comp_name} <<EOF
#!/bin/sh

$1="$2"
export $1

$3 \[$]{*}
EOF

  dnl Fix permissions
  chmod u+x config/wrappers/wrap-mpi${tmp_comp_name}

  dnl Overwrite compiler setting
  eval tmp_wrapper_path="${ac_abs_top_builddir}/config/wrappers/wrap-mpi${tmp_comp_name}"
  tmp_wrapper_name=`basename "${tmp_wrapper_path}"`
  AC_MSG_NOTICE([wrapping serial and MPI compilers into ${tmp_wrapper_name}])
  $1="${tmp_wrapper_path}"

  dnl Clean-up
  unset tmp_comp_name
  unset tmp_wrapper_name
  unset tmp_wrapper_path
]) # _ESCDFF_MPI_CREATE_WRAPPER
