#!/usr/bin/env python
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

"""\
This script generates Fortran constants in src/escdff.F90 from those
declared in ~escdf/src/escdf_common.h, to ensure a perfect match between C
and Fortran namespaces and avoid the inclusion of escdf_common.h in
Fortran files.
"""

__author__ = "Yann Pouillon"
__copyright__ = "Copyright (C) 2014-2017 Yann Pouillon"
__license__ = "LGPL version 2.1+"
__version__ = "0.1"

import os
import re
import sys

# Check command-line arguments
if ( (len(sys.argv) < 2) or (not os.path.exists(sys.argv[1])) ):
  sys.stderr.write("Error: source ESCDF C header not found\n")
  sys.exit(1)

# Init
re_c_cmts  = re.compile("(/\*.*\*/|//.*)")
re_cst_def = re.compile("^#define ")
re_cst_f90 = re.compile("    !%%% BEGIN ESCDF CONSTANTS.*    !%%% END ESCDF CONSTANTS", flags=re.MULTILINE+re.DOTALL)

# Translate C definitions into Fortran
f90_defs = "    !%%% BEGIN ESCDF CONSTANTS\n"
for line in open(sys.argv[1], "r").readlines():
  if ( re_cst_def.match(line) ):
    line = re.sub(re_c_cmts, "", line)
    line = line.split()
    cst_name = line[1]
    cst_value = " ".join(line[2:])
    if ( cst_value != "" ):
      try:
        cst_value = int(cst_value)
        f90_defs += "    integer(c_int), parameter, public :: %s = %d\n" % \
          (cst_name, cst_value)
      except:
        # The following should be change to use the iso_c_bindings
        f90_defs += "    character(len=*), parameter, public :: %s = %s\n" % \
          (cst_name, cst_value)
f90_defs += "    !%%% END ESCDF CONSTANTS"

# Replace existing Fortran definitions
f90_file = "src/escdff_common.F90"
f90_src  = open(f90_file, "r").read()
open(f90_file, "w").write(re.sub(re_cst_f90, f90_defs, f90_src))
