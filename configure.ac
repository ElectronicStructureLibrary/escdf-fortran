# Copyright (C) 2017 Yann Pouillon <devops@materialsevolution.es>
#
# This file is part of ESCDF-Fortran.
#
# ESCDF-Fortran is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation, version 2.1 of the License, or (at your option) any
# later version.
#
# ESCDF-Fortran is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with ESCDF-Fortran.  If not, see <http://www.gnu.org/licenses/> or
# write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
# Boston, MA  02110-1301  USA.

#
# Process this file with autoconf to produce a configure script.
#

# Enhance help message (must be set before anything else)
m4_divert_text([HELP_END], [
More information on how to configure the build of ESCDF-Fortran is available
in the doc/ subdirectory of the source tree, in particular in the
configuring-escdf-fortran.md file.

ESCDF-Fortran is part of a diverse software ecosystem. You can find more
information about it at https://esl.cecam.org/, the homepage of the
Electronic Structure Library Wiki.])

# Init Autoconf
AC_PREREQ([2.69])
AC_INIT([ESCDF-Fortran], [0.1.0], [https://gitlab.e-cam2020.eu/esl/escdf-fortran/issues], [escdf-fortran])
AC_CONFIG_AUX_DIR([config/gnu])
AC_CONFIG_MACRO_DIR([config/m4])
AC_CONFIG_SRCDIR([src/escdff.F90])
_AC_SRCDIRS([.])

# Init Automake
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.13 parallel-tests color-tests])
AM_CONFIG_HEADER([config.h])

# Generate timestamp
escdff_timestamp=`date '+%Y%m%dT%H%M%S%z'`
AC_SUBST(escdff_timestamp)

                    # ------------------------------------ #

#
# System utilities
#

# Check for common programs and features
AC_PROG_AWK
AC_PROG_GREP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_SED

# Workaround for the wrong path to install-sh on Mac systems
AX_PROG_MKDIR_P

# Init variables
escdff_core_incs=""
AC_SUBST(escdff_core_incs) 

                    # ------------------------------------ #

#
# Default settings
#

# Note for developers: you may edit these parameters to tune the behaviour
# of the build system. Please do it with care.

# Optional features
escdff_debug_enable_def="no"

# MPI
escdff_mpi_enable_def="auto"

# HDF5 (serial and MPI cases are distinct)
escdff_hdf5_enable_def="yes"
escdff_hdf5_ser_incs_def=""
escdff_hdf5_ser_libs_def="-lhdf5"
escdff_hdf5_mpi_incs_def=""
escdff_hdf5_mpi_libs_def="-lhdf5_mpi -lhdf5"

# ESCDF
escdff_escdf_enable_def="yes"
escdff_escdf_incs_def=""
escdff_escdf_libs_def="-lescdf"

                    # ------------------------------------ #

#
# Check consistency of defaults
#

# Default triggers for optional features must be yes or no, and not empty
for escdff_cfg_default in debug; do
  tmp_default=`eval echo \$\{escdff_${escdff_cfg_default}_enable_def\}`
  if test "${tmp_default}" != "no" -a \
          "${tmp_default}" != "yes"; then
    AC_MSG_ERROR([invalid default value: escdff_${escdff_cfg_default}_enable_def = ${tmp_default}])
  fi
done

# Default triggers for external dependencies must be yes, no, or auto
for escdff_cfg_default in hdf5 mpi; do
  tmp_default=`eval echo \$\{escdff_${escdff_cfg_default}_enable_def\}`
  if test "${tmp_default}" != "" -a \
          "${tmp_default}" != "auto" -a \
          "${tmp_default}" != "no" -a \
          "${tmp_default}" != "yes"; then
    AC_MSG_ERROR([invalid default value: escdff_${escdff_cfg_default}_enable_def = ${tmp_default}])
  fi
done

# Clean-up
unset tmp_default

                    # ------------------------------------ #

#
# Optional features
#

# Debugging
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [Enable verbose function output (default: ${escdff_debug_enable_def})])],
  [escdff_debug_enable="${enableval}"; escdff_debug_type="yon"],
  [escdff_debug_enable="${escdff_debug_enable_def}"; escdff_debug_type="def"])
AC_SUBST(enable_debug)

                    # ------------------------------------ #

#
# External dependencies
#

# ESCDF (mandatory)
AC_ARG_WITH([escdf],
  [AS_HELP_STRING([--with-escdf],
    [Install prefix of the ESCDF library (e.g. /usr/local). Since ESCDF is a mandatory dependency, --with-escdf without argument has no effect, while --without-escdf produces an error.])],
  [ if test "${withval}" = "no" -o "${withval}" = "yes"; then
      escdff_escdf_enable="${withval}"
      escdff_escdf_type="yon"
    else
      escdff_escdf_enable="yes"
      escdff_escdf_type="dir"
      escdff_escdf_incs="-I${withval}/include"
      escdff_escdf_libs="-L${withval}/lib"
    fi],
  [escdff_escdf_enable="${escdff_escdf_enable_def}"; escdff_escdf_type="def";
    escdff_escdf_incs=""; escdff_escdf_libs=""])
AC_ARG_VAR([ESCDF_INCLUDES],
  [Include flags for ESCDF. Please note that this variable conflicts with the --with-escdf option.])
AC_ARG_VAR([ESCDF_LIBS],
    [Library flags for ESCDF. Please note that this variable conflicts with the --with-escdf option.])
AC_SUBST(with_escdf)

# HDF5 (mandatory)
AC_ARG_WITH([hdf5],
  [AS_HELP_STRING([--with-hdf5],
    [Install prefix of the HDF5 library (e.g. /usr/local). Since HDF5 is a mandatory dependency, --with-hdf5 without argument has no effect, while --without-hdf5 produces an error.])],
  [ if test "${withval}" = "no" -o "${withval}" = "yes"; then
      escdff_hdf5_enable="${withval}"
      escdff_hdf5_type="yon"
    else
      escdff_hdf5_enable="yes"
      escdff_hdf5_type="dir"
      escdff_hdf5_incs="-I${withval}/include"
      escdff_hdf5_libs="-L${withval}/lib"
    fi],
  [escdff_hdf5_enable="${escdff_hdf5_enable_def}"; escdff_hdf5_type="def";
    escdff_hdf5_incs=""; escdff_hdf5_libs=""])
AC_ARG_VAR([HDF5_INCLUDES],
  [Include flags for HDF5. Please note that this variable conflicts with the --with-hdf5 option.])
AC_ARG_VAR([HDF5_LIBS],
    [Library flags for HDF5. Please note that this variable conflicts with the --with-hdf5 option.])
AC_SUBST(with_hdf5)

# MPI (optional)
AC_ARG_WITH([mpi],
  [AS_HELP_STRING([--with-mpi],
    [Install prefix of MPI (e.g. /usr/local). The default behaviour is to detect whether the specified compilers properly support MPI and to fall back to serial mode if not. You may use --with-mpi without argument to force MPI detection, in which case detection failures will result in errors, and --without-mpi to disable MPI support completely.])],
  [ if test "${withval}" = "no" -o "${withval}" = "yes"; then
      escdff_mpi_enable="${withval}"
      escdff_mpi_type="yon"
    else
      escdff_mpi_enable="yes"
      escdff_mpi_type="dir"
    fi],
  [escdff_mpi_enable="${escdff_mpi_enable_def}"; escdff_mpi_type="def"])
AC_ARG_VAR([MPICC], [MPI-aware C compiler (usually called 'mpicc'). Please note that this variable conflicts with the --with-mpi option.])
AC_ARG_VAR([MPIFC], [MPI-aware Fortran compiler (usually called 'mpif90'. Please note that this variable conflicts with the --with-mpi option.])
AC_SUBST(with_mpi)

                    # ------------------------------------ #

#
# Check option consistency
#

# All --enable-* options must be yes or no
for escdff_cfg_option in debug; do
  tmp_option=`eval echo \$\{enable_${escdff_cfg_option}\}`
  if test "${tmp_option}" != "" -a \
          "${tmp_option}" != "no" -a \
          "${tmp_option}" != "yes"; then
    AC_MSG_ERROR([--enable-${escdff_cfg_option} must be "yes" or "no"])
  fi
done
unset tmp_option

# ESCDF is mandatory
if test "${escdff_escdf_enable}" = "no"; then
  AC_MSG_ERROR([ESCDF-Fortran requires ESCDF
                  Please see https://www.hdfgroup.org/ESCDF/ for installation
                  instructions.])
fi
if test "${escdff_escdf_enable}" = "auto"; then
  AC_MSG_NOTICE([setting ESCDF trigger to yes])
  escdff_escdf_enable="yes"
fi

# HDF5 is mandatory
if test "${escdff_hdf5_enable}" = "no"; then
  AC_MSG_ERROR([ESCDF-Fortran requires HDF5
                  Please see https://www.hdfgroup.org/HDF5/ for installation
                  instructions.])
fi
if test "${escdff_hdf5_enable}" = "auto"; then
  AC_MSG_NOTICE([setting HDF5 trigger to yes])
  escdff_hdf5_enable="yes"
fi

# All *_INCLUDES and *_LIBS environment variables must be provided at once
tmp_escdf_vars="${ESCDF_INCLUDES}${ESCDF_LIBS}"
if test -n "${tmp_escdf_vars}" -a \
        \( -z "${ESCDF_INCLUDES}" -o -z "${ESCDF_LIBS}" \); then
  AC_MSG_ERROR([ESCDF_INCLUDES and ESCDF_LIBS must both be set])
fi
tmp_hdf5_vars="${HDF5_INCLUDES}${HDF5_LIBS}"
if test -n "${tmp_hdf5_vars}" -a \
        \( -z "${HDF5_INCLUDES}" -o -z "${HDF5_LIBS}" \); then
  AC_MSG_ERROR([HDF5_INCLUDES and HDF5_LIBS must both be set])
fi
tmp_mpi_vars="${MPICC}${MPIFC}"
if test -n "${tmp_mpi_vars}" -a \
        \( -z "${MPICC}" -o -z "${MPIFC}" \); then
  AC_MSG_ERROR([MPICC and MPIFC must both be set])
fi

# Environment variables conflict with --with-* options
if test -n "${tmp_hdf5_vars}" -a -n "${with_hdf5}"; then
  AC_MSG_ERROR([conflicting option settings
                  Please use HDF5_INCLUDES + HDF5_LIBS or --with-hdf5,
                  not both.])
fi
if test -n "${tmp_mpi_vars}" -a -n "${with_mpi}"; then
  AC_MSG_ERROR([conflicting option settings
                  Please use MPICC + MPIFC or --with-mpi,
                  not both.])
fi

# When using environment variables, triggers must be set to yes
if test -n "${tmp_escdf_vars}"; then
  escdff_escdf_enable="yes"
  escdff_escdf_type="env"
fi
if test -n "${tmp_hdf5_vars}"; then
  escdff_hdf5_enable="yes"
  escdff_hdf5_type="env"
fi
if test -n "${tmp_mpi_vars}"; then
  escdff_mpi_enable="yes"
  escdff_mpi_type="env"
fi

# Clean-up
unset tmp_escdf_vars
unset tmp_hdf5_vars
unset tmp_mpi_vars

                    # ------------------------------------ #

#
# Substitute build-system variables
#

# Defaults
AC_SUBST(escdff_debug_enable_def)
AC_SUBST(escdff_escdf_enable_def)
AC_SUBST(escdff_escdf_incs_def)
AC_SUBST(escdff_escdf_libs_def)
AC_SUBST(escdff_hdf5_enable_def)
AC_SUBST(escdff_hdf5_ser_incs_def)
AC_SUBST(escdff_hdf5_ser_libs_def)
AC_SUBST(escdff_hdf5_mpi_incs_def)
AC_SUBST(escdff_hdf5_mpi_libs_def)
AC_SUBST(escdff_mpi_enable_def)

# Triggers
AC_SUBST(escdff_debug_enable)
AC_SUBST(escdff_escdf_enable)
AC_SUBST(escdff_hdf5_enable)
AC_SUBST(escdff_mpi_enable)

# Initialization types
AC_SUBST(escdff_debug_type)
AC_SUBST(escdff_escdf_type)
AC_SUBST(escdff_hdf5_type)
AC_SUBST(escdff_mpi_type)

# Internal MPI parameters
AC_SUBST(escdff_mpi_cc_set)
AC_SUBST(escdff_mpi_cc_wrap)
AC_SUBST(escdff_mpi_fc_set)
AC_SUBST(escdff_mpi_fc_wrap)

# Build flags for external dependencies
AC_SUBST(escdff_escdf_incs)
AC_SUBST(escdff_escdf_libs)
AC_SUBST(escdff_hdf5_incs)
AC_SUBST(escdff_hdf5_libs)

                    # ------------------------------------ #

#
# Architecture setup
#

# Init MPI parameters, possibly setting and/or wrapping compilers
ESCDFF_MPI_INIT

                    # ------------------------------------ #

#
# C language support
#

# Init C compiler and preprocessor
if test "${CC}" != "" -a ! -x "${CC}"; then
  escdff_cc_probe=`echo "${CC}" | sed -e 's/ .*//'`
  if test ! -x "${escdff_cc_probe}"; then
    AC_PATH_PROG([escdff_cc_path],[${escdff_cc_probe}])
    if test "${escdff_cc_path}" = ""; then
      AC_MSG_ERROR([could not run C compiler "${CC}"])
    fi
  fi
fi
AC_PROG_CC
AC_PROG_CPP

                    # ------------------------------------ #

#
# Fortran language support
#

# Init Fortran compiler
if test "${FC}" != "" -a ! -x "${FC}"; then
  escdff_fc_probe=`echo "${FC}" | sed -e 's/ .*//'`
  if test ! -x "${escdff_fc_probe}"; then
    AC_PATH_PROG([escdff_fc_path], [${escdff_fc_probe}])
    if test "${escdff_fc_path}" = ""; then
      AC_MSG_ERROR([could not run Fortran compiler "${FC}"])
    fi
  fi
fi
AC_PROG_FC

# Enforce strict file extensions
escdff_fc_src_ok="unknown"
AC_FC_SRCEXT([F90], [escdff_fc_src_ok="yes"], [escdff_fc_src_ok="no"])
if test "${escdff_fc_src_ok}" != "yes"; then
  AC_MSG_WARN([Fortran file extension could not be changed])
  AC_MSG_WARN([Fortran tests may fail])
fi

# Look for the iso_c_binding module
escdff_fc_iso_c_binding="unknown"
AC_MSG_CHECKING([for the ISO_C_BINDING Fortran module])
AC_LANG_PUSH([Fortran])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([], [[use iso_c_binding]])],
  [escdff_fc_iso_c_binding="yes"],
  [escdff_fc_iso_c_binding="no"])
AC_LANG_POP([Fortran])
AC_MSG_RESULT([${escdff_fc_iso_c_binding}])
if test "${escdff_fc_iso_c_binding}" != "yes"; then
  AC_MSG_ERROR([the Fortran compiler does not provide
                the iso_c_binding module
                This probably happens because this compiler
                is an antiquity
                Please disable Fortran support or use a modern
                Fortran compiler])
fi

# Fortran compiler peculiarities
AX_F90_MODULE_EXTENSION
AC_SUBST(ax_cv_f90_modext)
AX_F90_MODULE_CASE

# Language mixing
AC_FC_WRAPPERS

# Inform Automake
AM_CONDITIONAL([F90_MOD_UPPERCASE],
  [test "${ax_cv_f90_mod_uppercase}" = "yes"])

                    # ------------------------------------ #

#
# Libtool configuration
#

# Init Libtool (must be done once compilers are fully set)
LT_INIT
LT_PREREQ([2.4])
LTOBJEXT="lo"
AC_SUBST(LTOBJEXT)

                    # ------------------------------------ #

#
# Multicore architecture support
#

# Look for MPI
if test "${escdff_mpi_enable}" != "no"; then
  ESCDFF_MPI_DETECT
  if test "${escdff_mpi_ok}" = "yes"; then
    AC_DEFINE([HAVE_MPI], 1, [Define to 1 to enable MPI support.])
    escdff_mpi_enable="yes"
  else
    if test "${escdff_mpi_enable}" = "auto"; then
      AC_MSG_NOTICE([disabling MPI support])
      escdff_mpi_enable="no"
    else
      AC_MSG_FAILURE([MPI support is broken - please check your configuration])
    fi
  fi
fi

# From now on, the MPI trigger must be yes or no
if test "${escdff_mpi_enable}" != "no" -a "${escdff_mpi_enable}" != "yes"; then
  AC_MSG_ERROR([unexpected MPI trigger value: '${escdff_mpi_enable}'])
fi

                    # ------------------------------------ #

#
# Adjust build parameters according to configure options
#

# Debugging support
if test "${escdff_debug_enable}" = "yes"; then
  AC_DEFINE([ESCDFF_DEBUG], 1, [Define to 1 to enable internal debugging.])
fi

                    # ------------------------------------ #

#
# Look for external dependencies (the order strongly matters)
#

# Check for mathematical functions
AC_CHECK_LIB(m, sqrt, [], [
  AC_MSG_WARN([missing math library (sqrt not found)])])

# Set HDF5 includes
if test "${escdff_hdf5_type}" != "dir"; then
  if test -n "${HDF5_INCLUDES}"; then
    escdff_hdf5_incs="${HDF5_INCLUDES}"
  else
    if test "${escdff_mpi_enable}" = "yes"; then
      escdff_hdf5_incs="${escdff_hdf5_mpi_incs_def}"
    else
      escdff_hdf5_incs="${escdff_hdf5_ser_incs_def}"
    fi
  fi
fi

# Set HDF5 libraries (requires MPI to be fully set)
if test "${escdff_hdf5_type}" = "dir"; then
  if test "${escdff_mpi_enable}" = "yes"; then
    escdff_hdf5_libs="${escdff_hdf5_libs} ${escdff_hdf5_mpi_libs_def}"
  else
    escdff_hdf5_libs="${escdff_hdf5_libs} ${escdff_hdf5_ser_libs_def}"
  fi
else
  if test -n "${HDF5_LIBS}"; then
    escdff_hdf5_libs="${HDF5_LIBS}"
  else
    if test "${escdff_mpi_enable}" = "yes"; then
      escdff_hdf5_libs="${escdff_hdf5_mpi_libs_def}"
    else
      escdff_hdf5_libs="${escdff_hdf5_ser_libs_def}"
    fi
  fi
fi

# Look for HDF5
AC_MSG_CHECKING([how HDF5 parameters have been set])
AC_MSG_RESULT([${escdff_hdf5_type}])
AC_MSG_CHECKING([for HDF5 include flags])
AC_MSG_RESULT([${escdff_hdf5_incs}])
AC_MSG_CHECKING([for HDF5 library flags])
AC_MSG_RESULT([${escdff_hdf5_libs}])
ESCDFF_HDF5_DETECT
if test "${escdff_hdf5_ok}" = "yes"; then
  CPPFLAGS="${CPPFLAGS} ${escdff_hdf5_incs}"
  LIBS="${escdff_hdf5_libs} ${LIBS}"
else
  AC_MSG_FAILURE([HDF5 is missing or incomplete])
fi

# Set ESCDF includes
if test "${escdff_escdf_type}" != "dir"; then
  if test -n "${ESCDF_INCLUDES}"; then
    escdff_escdf_incs="${ESCDF_INCLUDES}"
  else
    escdff_escdf_incs="${escdff_escdf_incs_def}"
  fi
fi

# Set ESCDF libraries (requires MPI to be fully set)
if test "${escdff_escdf_type}" = "dir"; then
  escdff_escdf_libs="${escdff_escdf_libs} ${escdff_escdf_libs_def}"
else
  if test -n "${ESCDF_LIBS}"; then
    escdff_escdf_libs="${ESCDF_LIBS}"
  else
    escdff_escdf_libs="${escdff_escdf_libs_def}"
  fi
fi

# Look for ESCDF
AC_MSG_CHECKING([how ESCDF parameters have been set])
AC_MSG_RESULT([${escdff_escdf_type}])
AC_MSG_CHECKING([for ESCDF include flags])
AC_MSG_RESULT([${escdff_escdf_incs}])
AC_MSG_CHECKING([for ESCDF library flags])
AC_MSG_RESULT([${escdff_escdf_libs}])
ESCDFF_ESCDF_DETECT
if test "${escdff_escdf_ok}" = "yes"; then
  CPPFLAGS="${CPPFLAGS} ${escdff_escdf_incs}"
  LIBS="${escdff_escdf_libs} ${LIBS}"
else
  AC_MSG_FAILURE([ESCDF is missing or incomplete])
fi

                    # ------------------------------------ #

# Write down YAML configuration
AC_OUTPUT([config/data/escdf-fortran-config.yml])

# Inform Automake
AM_CONDITIONAL([DO_BUILD_MPI], [test "${escdff_mpi_enable}" = "yes"])

# Report configuration
AC_MSG_NOTICE([])
AC_MSG_NOTICE([Final build parameters])
AC_MSG_NOTICE([----------------------])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([TSTAMP   = ${escdff_timestamp}])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([DEBUG     = ${escdff_debug_enable} (init: ${escdff_debug_type})])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([ESCDF    = ${escdff_escdf_enable} (init: ${escdff_escdf_type})])
AC_MSG_NOTICE([HDF5     = ${escdff_hdf5_enable} (init: ${escdff_hdf5_type})])
AC_MSG_NOTICE([MPI      = ${escdff_mpi_enable} (init: ${escdff_mpi_type})])
AC_MSG_NOTICE([])
AC_MSG_NOTICE([CPP      = ${CPP}])
AC_MSG_NOTICE([CPPFLAGS = ${CPPFLAGS}])
AC_MSG_NOTICE([CC       = ${CC}])
AC_MSG_NOTICE([MPICC    = ${MPICC}])
AC_MSG_NOTICE([CFLAGS   = ${CFLAGS}])
AC_MSG_NOTICE([FC       = ${FC}])
AC_MSG_NOTICE([MPIFC    = ${MPIFC}])
AC_MSG_NOTICE([FCFLAGS  = ${FCFLAGS}])
AC_MSG_NOTICE([LDFLAGS  = ${LDFLAGS}])
AC_MSG_NOTICE([LIBS     = ${LIBS}])
AC_MSG_NOTICE([])

AC_CONFIG_FILES([
  Makefile
  config/data/escdf-fortran.pc
  doc/Makefile
  src/Makefile])

AC_OUTPUT
