#
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
# Makefile for the sources of ESCDF-Fortran
#

                    # ------------------------------------ #

#
# Source files
#

# Fortran source - keep this in alphabetical order
escdff_srcs = \
  escdff.F90

escdff_inc_srcs =

# Fortran modules
if F90_MOD_UPPERCASE
  escdff_mods = ESCDFF.@ax_cv_f90_modext@
else
  escdff_mods = escdff.@ax_cv_f90_modext@
endif

# Included Fortran files are not distributed by default
EXTRA_DIST = $(escdff_inc_srcs)

                    # ------------------------------------ #

#
# Build targets
#

# Libraries
lib_LTLIBRARIES = libescdff.la

libescdff_la_LDFLAGS = -version-info 0:0:0
libescdff_la_SOURCES = $(escdff_srcs)

# Headers
nodist_include_HEADERS = $(escdff_mods)

                    # ------------------------------------ #

#
# Explicit dependencies
#

# Fortran
escdff.$(LTOBJEXT): $(escdff_inc_srcs)

$(escdff_mods): escdff.$(LTOBJEXT)
	@echo "Modules already built"

                    # ------------------------------------ #

#
# Explicit targets
#

# Fortran modules have to be cleaned explicitely
CLEANFILES = $(escdff_mods)
