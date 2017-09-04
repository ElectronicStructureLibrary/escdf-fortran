#
# Top Makefile for ESCDF-Fortran
#

ACLOCAL_AMFLAGS = -I config/m4

# Make sure that 'make distcheck' performs a minimal build
AM_DISTCHECK_CONFIGURE_FLAGS = \
  --disable-debug \
  --without-mpi

# Build targets are expected to be in subdirectories
SUBDIRS = src doc

# TODO: write script to generated the environment module
# Files to install for the Environment Modules
# See http://modules.sourceforge.net/ for details
#envmoddir = $(datarootdir)/$(PACKAGE)/modulefiles/escdf-fortran
#nodist_envmod_DATA = config/data/escdf-fortran/$(PACKAGE_VERSION)

# Files to install for pkg-config
# See http://www.freedesktop.org/wiki/Software/pkg-config/ for details
pkgconfigdir = $(libdir)/pkgconfig
nodist_pkgconfig_DATA = config/data/escdf-fortran.pc

# Some files are not removed by default and cause "make distclean" to fail
DISTCLEANFILES = \
  libescdff-*.tar.bz2 \
  libescdff-*.tar.gz \
  libescdff-*.tar.xz \
  config/data/escdf-fortran.pc \
  config/data/escdf-fortran/$(PACKAGE_VERSION) \
  config/data/escdf-fortran/$(PACKAGE_VERSION).in

# We use our own version of Markdown to build the documentation
EXTRA_DIST = config/scripts/markdown
