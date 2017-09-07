# Copyright (C) 2011-2016 Micael Oliveira <micael.oliveira@mpsd.mpg.de>
#                         Yann Pouillon <notifications@materialsevolution.es>
#
# This file is part of Libescdf.
#
# Libescdf is free software: you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free
# Software Foundation, version 3 of the License, or (at your option) any later
# version.
#
# Libescdf is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Libescdf.  If not, see <http://www.gnu.org/licenses/> or write to
# the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301  USA.

#
# Makefile for the tests of ESCDF-Fortran
#

                    # ------------------------------------ #

#
# Build parameters
#

AM_FCFLAGS = -I../src

                    # ------------------------------------ #

#
# FRUIT unit test framework
#

noinst_LTLIBRARIES = libfruit.la
libfruit_la_SOURCES = fruit.f90

                    # ------------------------------------ #

#
# Test programs
#
escdff_basic_tests = \
  test_fortran_error

check_PROGRAMS = $(escdff_basic_tests)

test_fortran_error_SOURCES = \
  fruit_error_test.F90 \
  test_fortran_error.F90
test_fortran_error_LDADD = \
  libfruit.la \
  ../src/libescdff.la \
  @escdff_escdf_libs@
test_fortran_error_DEPENDENCIES = \
  ../src/libescdff.la

TESTS = \
  $(escdff_basic_tests)

                    # ------------------------------------ #

#
# Explicit dependencies
#

# Fortran objects
test_fortran_error.$(OBJEXT): fruit_error_test.$(OBJEXT)

                    # ------------------------------------ #

#
# Explicit targets
#

# Temporary test outputs
MOSTLYCLEANFILES = test_*.tmp

# Fortran modules
CLEANFILES = *.@ax_cv_f90_modext@
