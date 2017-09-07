#!/bin/sh

# Init ESCDF params
escdf_url="https://gitlab.e-cam2020.eu/esl"
escdf_revno="e998cc5ef44c7993f4471ab4ce9e28ec6a4c86d1"
escdff_revno="788fc4ca34817af64c7b40447aefbac6db227059"

# Init internal params
my_name=`basename $0`

# Check command-line arguments
if test "$#" != 1; then
cat >&2 <<EOF
Usage: ${my_name} install_prefix

The install_prefix argument must be the absolute path to a non-existing directory.

The ESCDF project is built on top of HDF5 (https://support.hdfgroup.org/HDF5/). You
will thus have to make sure that the HDF5 libraries and their development files
are available on your system before building the ESCDF libraries. Please note that
binary packages are available for most Unix-compatible systems, in particular
Linux- and BSD-based ones, as well as Portfiles on MacPorts for MacOSX.

The install process will be successful provided you have also installed the following
packages beforehand (either through your package manager or manually):

  - autoconf (https://gnu.org/software/autoconf)
  - automake (https://gnu.org/software/automake)
  - check (https://github.com/libcheck/check)
  - git (https://git-scm.org/)
  - libtool (https://gnu.org/software/libtool)
  - m4 (https://gnu.org/software/m4)

You will also need the following toolchain (the vendor is up to you):

  - a working C preprocessor
  - a working C compiler
  - a working Fortran compiler

This installer has been tested with the foss/2016b toolchain of EasyBuild, using
Lmod to manage Environment Modules. With this configuration, HDF5 is detected
out-of-the-box, without having to provide any option to the configure scripts of
escdf and escdf-fortran. You may wish to have a look at these two valuable tools
and possibly install them:

  - Lmod (https://lmod.readthedocs.org/)
  - EasyBuild (https://easybuild.readthedocs.org/)

To install HDF5 through EasyBuild, you can type the following:

  eb HDF5-1.8.17-foss-2016b.eb -r
  module load HDF5

Please note that it may take up to a couple of hours to build all the dependencies
of HDF5 if this is the first time you run EasyBuild. In exchange, you will have a
reference toolchain that you can use for further developments, with the assurance
that it will be easy for us to reproduce any issue you may encounter on your way.

Should you need to provide HDF5 parameters manually, we recommend you to set them
through the HDF5_INCLUDES and HDF5_LIBS environment variables, e.g.:

  export HDF5_INCLUDES="-I/path/to/hdf5/include"
  export HDF5_LIBS="-L/path/to/hdf5/lib -lhdf5"

Please report issues to:

  - https://gitlab.e-cam2020.eu/esl/escdf/issues or
  - https://gitlab.e-cam2020.eu/esl/escdf-fortran/issues

depending on your current programming language.

EOF
exit 0
fi
my_path="$1"

# Check that install path does not exist yet
if test -e "${my_path}"; then
  echo "${my_name}: Error: install path already exists" >&2
  exit 1
fi

# Check that install path is absolute
first_dir=`echo "${my_path}" | cut -d/ -f2`
if test ! -d "/${first_dir}"; then
  echo "${my_name}: Error: install path must be an absolute path" >&2
  exit 2
fi

# Stop at first error
set -e

# Prepare the way
mkdir -p "${my_path}/src"
cd "${my_path}/src"
git clone "${escdf_url}/escdf.git"
git clone "${escdf_url}/escdf-fortran.git"

# Install ESCDF
cd escdf
git checkout "${escdf_revno}"
./autogen.sh
mkdir tmp
cd tmp
../configure --prefix="${my_path}"
make install
cd ../..

# Install ESCDF-Fortran
cd escdf-fortran
git checkout "${escdff_revno}"
./autogen.sh
mkdir tmp
cd tmp
../configure --prefix="${my_path}" --with-escdf="${my_path}"
make install
cd ../..

# Display information on how to use the libraries
cat <<EOF

ESCDF and ESCDF-Fortran are now ready to use.

To link and run your programs with these versions of the libraries, please make sure
to set your shell parameters appropriately. For Bourne-like shells, this means typing
a few commands like "export VARIABLE="value:\$VARIABLE". For C-like shells, it looks
like "setenv VARIABLE value:\$VARIABLE".

On Linux and BSD, please set your environment this way for Bourne-like shells:

  export CPATH="${my_path}/include:\${CPATH}"
  export LD_LIBRARY_PATH="${my_path}/lib:\${LD_LIBRARY_PATH}
  export LIBRARY_PATH="${my_path}/lib:\${LIBRARY_PATH}

and that way for C-like shells:

  setenv CPATH "${my_path}/include:\${CPATH}"
  setenv LD_LIBRARY_PATH "${my_path}/lib:\${LD_LIBRARY_PATH}
  setenv LIBRARY_PATH "${my_path}/lib:\${LIBRARY_PATH}

On MacOSX, you should type the following:

  export CPATH="${my_path}/include:\${CPATH}"
  export DYLD_LIBRARY_PATH="${my_path}/lib:\${DYLD_LIBRARY_PATH}

Once done, you will be able to link and run your programs by providing the following
flags at link-time:

  -lescdff -lescdf ... other libraries ...

Should you need more explicit compile-time parameters, please use the following:

  CPPFLAGS="\${CPPFLAGS} -I${my_path}"
  FCFLAGS="\${FCFLAGS} -I${my_path}"
  LIBS="-L${my_path} -lescdff -lescdf \${LIBS}"

Enjoy the ESCDF libraries!

EOF
