#.........................................................................
# VERSION "$Id: Makefile 1 2014-03-14 20:22:54Z coats $"
#      EDSS/Models-3 I/O API Version 3.
#.........................................................................
# COPYRIGHT
#       (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
#       (C) 2003-2004 by Baron Advanced Meteorological Systems.
#       Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
#       See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Environment Variables:
#
#       BIN     machine/OS/compiler/mode type. Shows up as suffix
#               for "$(IODIR)/Makeinclude.$(BIN)" to determine compilation
#               flags, and in $(OBJDIR) and $(INSTALL) to determine
#               binary directories
#
#       INSTALL installation-directory root, used for "make install":
#               "libioapi.a" and the tool executables will be installed
#               in $(INSTALL)/$(BIN)
#.........................................................................
#  Directories:
#
#       $(BASEDIR)  is the root directory for the I/O API library source,
#                   the M3Tools and M3Test source,the  HTML documentation,
#                   and the (machine/compiler/flag-specific) binary
#                   object/library/executable directories.
#       $(HTMLDIR)  is the web documentation
#       $(IODIR)    is the I/O API library source
#       $(TOOLDIR)  is the "M3TOOLS" source
#       $(TESTDIR)  is the "IOTESTS" source and binaries
#       $(OBJDIR)   is the current machine/compiler/flag-specific
#                   build-directory
#       $(INSTALL)  installation-directory root, used for "make install":
#                   "libioapi.a" and the tool executables will be installed
#                   in $(INSTALL)/$(BIN)
#       object/library/executable directory
#.........................................................................
# Note On Library Versions and configuration:
#
#       Environment variable "BIN" specifies library version up to
#       link- and compile-flag compatibility.  Dependecies upon machine,
#       OS, and compiler are found in file "Makeinclude.$(BIN)".
#
#       IN PARTICULAR, pay attention to the notes for various versions
#       that may be built for Linux x86 with the Portland Group
#       compilers:  see comments in $(IODIR)/include 'MAKEINCLUDE'.Linux2_x86pg
#.........................................................................
# Special Make-targets
#
#       configure:  "Makefile"s, with the definitions indicated below.
#       all:      OBJDIR, FIXDIR, libioapi.a, and executables, with
#                 coupling mode enabled
#       [no]cpl:  OBJDIR, FIXDIR, libioapi.a, and executables, with
#                 coupling mode en[dis]abled
#       f77:      OBJDIR, FIXDIR, libioapi.a, and F77-based executables
#       nocpl77:  OBJDIR, FIXDIR, libioapi.a, and F77-based executables,
#                 with coupling mode disabled
#       lib:      OBJDIR, FIXDIR, libioapi.a
#       clean:    remove .o's, libioapi.a, and executables from OBJDIR
#       rmexe:    remove executables from OBJDIR
#       relink:   rebuild executables from OBJDIR
#       install:  copy "libioapi.a" and executables to $(INSTDIR)
#       dirs:     make OBJDIR and FIXDIR directories
#       fix:      FIXDIR and extended-fixed-source INCLUDE-files
#       gtar:     GZipped tar-file of the source and docs
#       nametest: test of name-mangling compatibility (requires that
#                 libnetcdf.a be manually placed into $(OBJDIR))
#
######################################################################

VERSION = 3.2

#      ----------   Definitions for "make configure"  ------------------
#
#  VERSIONING DEFINITIONS:  the preprocessor definitions in $(IOAPIDEFS)
#  (below) govern I/O API behavior; versions with distinct combinations
#  of these options are NOT library- nor object-compatible and should
#  be built in *distinct*  $(OBJDIR)s:
#
#       Defining IOAPICPL turns on PVM-enabled "coupling mode" and
#       requires "libpvm3.a" for linking.
#
#       Defining IOAPI_NO_STDOUT suppresses WRITEs to the screen in
#       routines INIT3(), M3MSG2(), M3MESG(), M3PARAG(), and M3ABORT().
#       This also helps control the "double-printed-message" behavior
#       caused by recent SGI compilers.
#
#       Defining IO_360 creates the 360-day "global climate" version
#       of the library.
#
#       Defining BIN3_DEBUG turns on trace-messages for native-binary
#       mode routines.

BASEDIR    = ${HOME}/ioapi-3.2
INSTALL    = /SOMEWHERE
LIBINST    = $(INSTALL)/$(BIN)
BININST    = $(INSTALL)/$(BIN)

CPLMODE    = nocpl
IOAPIDEFS  = # -DIOAPICPL
PVMINCL    = $(PVM_ROOT)/conf/$(PVM_ARCH).def

#               ****   Variants   ****
#
# CPLMODE  = cpl                #  turn on PVM coupling mode
# IOAPIDEFS=                    #  for "nocpl"
# PVMINCL  = /dev/null          #  for "nocpl"
# PVM_ROOT = <for "cpl":  root for PVM installation>
# PVM_ARCH = <for "cpl":  PVM machine/compiler type; may need to override>
# BIN      = <I/O AIP machine/compiler-type for $(IODIR)/Makeinclude.*>
# INSTALL  = <installation base-directory>
# LIBINST  = $(INSTALL)/lib
# BININST  = $(INSTALL)/bin

# Edit-command used by "make configure"  to customize the "*/Makefile*"

SEDCMD = \
-e 's|IOAPI_BASE|$(BASEDIR)|' \
-e 's|LIBINSTALL|$(LIBINST)|' \
-e 's|BININSTALL|$(BININST)|' \
-e 's|IOAPI_DEFS|$(IOAPIDEFS)|' \
-e 's|MAKEINCLUDE|include $(IODIR)/Makeinclude.$(BIN)|' \
-e 's|PVMINCLUDE|include  $(PVMINCL)|'


#      ----------   I/O API Build System directory definitions  --------

IODIR      = $(BASEDIR)/ioapi
FIXDIR     = $(IODIR)/fixed_src
HTMLDIR    = $(BASEDIR)/HTML
TOOLDIR    = $(BASEDIR)/m3tools
RTTDIR     = $(BASEDIR)/rttools
OBJDIR     = $(BASEDIR)/$(BIN)
#
#
#      ----------------------   TOP-LEVEL TARGETS:   ------------------
#
all:  dirs fix
	(cd $(IODIR)   ; make all)
	(cd $(TOOLDIR) ; make all)
	(cd $(RTTDIR)  ; make all)

configure:
	(cd $(IODIR)   ;  sed $(SEDCMD) < Makefile.$(CPLMODE).sed > Makefile )
	(cd $(NOTDIR)  ;  sed $(SEDCMD) < Makefile.sed            > Makefile )
	(cd $(TOOLDIR) ;  sed $(SEDCMD) < Makefile.$(CPLMODE).sed > Makefile )
	(cd $(TESTDIR) ;  sed $(SEDCMD) < Makefile.$(CPLMODE).sed > Makefile )

bins:  dirs
	(cd $(IODIR)   ; make bins)
	(cd $(TOOLDIR) ; make bins)
	(cd $(RTTDIR)  ; make bins)

clean:
	(cd $(IODIR)   ; make -i clean)
	(cd $(TOOLDIR) ; make -i clean)
	(cd $(RTTDIR)  ; make -i clean)

relink:
	(cd $(TOOLDIR) ; make -i rmexe; make)
	(cd $(RTTDIR)  ; make -i rmexe; make)

install: $(LIBINST) $(BININST)
	echo "Installing I/O API and M3TOOLS in $(LIBINST) and $(BININST)"
	(cd $(IODIR)   ; make install)
	(cd $(TOOLDIR) ; make install)

dirs: $(OBJDIR) $(FIXDIR)

fix:
	(cd $(IODIR)   ; make fixed_src)

gtar:
	cd $(BASEDIR); date > VERSION.txt; \
gtar cvfz ioapi-$(VERSION).tar.gz --dereference -X $(BASEDIR)/exclude \
Makefile*  VERSION.txt exclude ioapi HTML m3tools

lib:  dirs
	(cd $(IODIR)   ; make all)

not:  dirs
	(cd $(NOTDIR)  ; make all)

nametest:  lib $(OBJDIR)/libnetcdff.a
	(cd $(IODIR)   ; make nametest)

$(FIXDIR):
	mkdir -p $(FIXDIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(LIBINST): $(INSTALL)
	cd $(INSTALL); mkdir -p $(LIBINST)

$(BININST): $(INSTALL)
	cd $(INSTALL); mkdir -p $(BININST)

