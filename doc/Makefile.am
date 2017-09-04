#
# Makefile for the documentation of ESCDF-Fortran
#

# Markdown sources
escdff_mddocs = \
  configuring-escdf-fortran.md \
  contributing-to-escdf-fortran.md \
  hacking-the-build-system.md

# HTML documents
escdff_htmldocs = \
  configuring-escdf-fortran.html \
  contributing-to-escdf-fortran.html \
  hacking-the-build-system.html

                    ########################################

# User-defined rules
MARKDOWN = $(top_srcdir)/config/scripts/markdown
MDFLAGS  = 

SUFFIXES = .md .html
.md.html:
	sed -e "s/@TITLE@/$$(head -n 1 $<)/" \
	  $(srcdir)/data/escdff_header.html >$@
	$(MARKDOWN) $(MDFLAGS) $< >>$@
	cat $(srcdir)/data/escdff_footer.html >>$@

# Targets
htmldocdir = $(docdir)/html
htmldoc_DATA = $(escdff_htmldocs)

# Inform Automake
CLEANFILES = $(escdff_htmldocs)
EXTRA_DIST = data/escdff_header.html data/escdff_footer.html $(escdff_mddocs)
